use std::collections::HashMap;

use anyhow::Error as AnyError;
use cranelift_codegen::{
    ir::{
        types::{F64, I64},
        AbiParam, ExternalName, FuncRef, Function, InstBuilder, MemFlags, Signature, Value,
    },
    isa::TargetIsa,
    Context,
};
use cranelift_frontend::{FunctionBuilder, FunctionBuilderContext};
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{FuncId, Linkage, Module, ModuleError};
use interpreter::{Expr, Factor, Func, Identifier, Term};

pub const DEF_FUNCS: [Func; 7] = [
    Func::Sin,
    Func::Cos,
    Func::Tan,
    Func::Asin,
    Func::Acos,
    Func::Atan,
    Func::Ln,
];

fn index_of(func: &Func) -> usize {
    match func {
        Func::Sin => 0,
        Func::Cos => 1,
        Func::Tan => 2,
        Func::Asin => 3,
        Func::Acos => 4,
        Func::Atan => 5,
        Func::Ln => 6,
        _ => panic!("unexpected function"),
    }
}
#[derive(Clone)]
pub struct External<T> {
    libc_pow: T,
    functions: [T; DEF_FUNCS.len()],
}

impl<T> External<T> {
    fn map<U>(self, mut map: impl FnMut(T) -> U) -> External<U> {
        External {
            libc_pow: map(self.libc_pow),
            functions: self.functions.map(|f| map(f)),
        }
    }
}

#[allow(dead_code)]
struct JITContext {
    jit_module: JITModule,
    external: External<FuncId>,
    context: Context,
    function_sig: Signature,
    codegen_flags: cranelift_codegen::settings::Flags,
    isa: Box<dyn TargetIsa>,
    fn_builder_ctx: FunctionBuilderContext,
}

impl JITContext {
    fn new() -> Result<Self, ModuleError> {
        let mut jit_builder = JITBuilder::new(cranelift_module::default_libcall_names())?;
        for func in DEF_FUNCS.iter() {
            jit_builder.symbol(func.as_str(), func.get_func() as *const u8);
        }

        let mut jit_module = JITModule::new(jit_builder);
        let mut libc_pow_sig = jit_module.make_signature();
        libc_pow_sig.returns.push(AbiParam::new(F64));
        libc_pow_sig.params.push(AbiParam::new(F64));
        libc_pow_sig.params.push(AbiParam::new(F64));
        let libc_pow = jit_module.declare_function("pow", Linkage::Import, &libc_pow_sig)?;
        let mut f_sig = jit_module.make_signature();
        f_sig.returns.push(AbiParam::new(F64));
        f_sig.params.push(AbiParam::new(F64));

        let functions = DEF_FUNCS
            .try_map(|f| jit_module.declare_function(f.as_str(), Linkage::Import, &f_sig))?;
        let mut function_sig = jit_module.make_signature();
        function_sig.params.push(AbiParam::new(I64));
        function_sig.returns.push(AbiParam::new(F64));
        let context = jit_module.make_context();
        let codegen_flags =
            cranelift_codegen::settings::Flags::new(cranelift_codegen::settings::builder());
        let isa = cranelift_native::builder()
            .map_err(|e| ModuleError::Backend(AnyError::msg(e)))?
            .finish(codegen_flags.clone())
            .map_err(|e| ModuleError::Backend(AnyError::msg(e)))?;

        let fn_builder_ctx = FunctionBuilderContext::new();

        Ok(Self {
            jit_module,
            external: External {
                libc_pow,
                functions,
            },
            context,
            function_sig,
            codegen_flags,
            isa,
            fn_builder_ctx,
        })
    }

    fn codegen<E: CodeGen>(
        &mut self,
        expr: &E,
        defines: &[Identifier],
    ) -> Result<Function, ModuleError> {
        let mut func = Function::with_name_signature(
            ExternalName::User {
                namespace: 0,
                index: 0,
            },
            self.function_sig.clone(),
        );
        let external = self
            .external
            .clone()
            .map(|id| self.jit_module.declare_func_in_func(id, &mut func));

        let mut builder = FunctionBuilder::new(&mut func, &mut self.fn_builder_ctx);
        let header = builder.create_block();
        builder.switch_to_block(header);
        builder.seal_block(header);
        builder.append_block_params_for_function_params(header);

        let args = builder.block_params(header);
        let r = args[0];
        let defines = defines
            .iter()
            .cloned()
            .enumerate()
            .map(|(i, id)| {
                let offset = i as i32 * 8;
                (id, builder.ins().load(F64, MemFlags::new(), r, offset))
            })
            .collect();

        let val = expr.inline(&mut builder, &defines, &external);
        builder.ins().return_(&[val]);
        builder.seal_all_blocks();
        builder.finalize();

        Ok(func)
    }

    fn get_addr<E: CodeGen>(
        &mut self,
        expr: &E,
        defines: &[Identifier],
    ) -> Result<*const u8, ModuleError> {
        let func_name = "f";
        let func_id =
            self.jit_module
                .declare_function(func_name, Linkage::Export, &self.function_sig)?;
        self.context.clear();
        self.context.func = self.codegen(expr, defines)?;

        self.jit_module
            .define_function(func_id, &mut self.context)?;
        self.jit_module.finalize_definitions();

        Ok(self.jit_module.get_finalized_function(func_id))
    }
}

type Defines = HashMap<Identifier, Value>;

pub trait CodeGen {
    fn inline(
        &self,
        builder: &mut FunctionBuilder,
        defines: &Defines,
        external: &External<FuncRef>,
    ) -> Value;
}

impl CodeGen for Factor {
    fn inline(
        &self,
        builder: &mut FunctionBuilder,
        defines: &Defines,
        external: &External<FuncRef>,
    ) -> Value {
        match self {
            Factor::Number(n) => builder.ins().f64const(*n),
            Factor::Identifier(id) => defines.get(id).unwrap().clone(),
            Factor::Group(g) => g.inline(builder, defines, external),
            Factor::Func(f, arg) => {
                let arg = arg.inline(builder, defines, external);
                match f {
                    Func::Abs => builder.ins().fabs(arg),
                    f => {
                        let c = builder.ins().call(external.functions[index_of(f)], &[arg]);
                        builder.func.dfg.first_result(c)
                    }
                }
            }
            Factor::Pow(a, b) => {
                let a = a.inline(builder, defines, external);
                let b = b.inline(builder, defines, external);
                let c = builder.ins().call(external.libc_pow, &[a, b]);
                builder.func.dfg.first_result(c)
            }
            Factor::Call(_, _) => todo!(),
        }
    }
}

impl CodeGen for Term {
    fn inline(
        &self,
        builder: &mut FunctionBuilder,
        defines: &Defines,
        external: &External<FuncRef>,
    ) -> Value {
        let Some(n) = self
            .factors
            .iter()
            .map(|f| f.inline(builder, defines, external))
            .collect::<Vec<_>>()
            .into_iter()
            .reduce(|acc, x| builder.ins().fmul(acc, x)) else {
                return builder.ins().f64const(0.0);
            };

        let v = if let Some(d) = self
            .denominators
            .iter()
            .map(|d| d.inline(builder, defines, external))
            .collect::<Vec<_>>()
            .into_iter()
            .reduce(|acc, x| builder.ins().fmul(acc, x))
        {
            builder.ins().fdiv(d, n)
        } else {
            n
        };

        if self.consts.is_neg() {
            let c = builder.ins().f64const(f64::from(self.consts));
            builder.ins().fneg(v)
        } else {
            v
        }
    }
}

impl CodeGen for Expr {
    fn inline(
        &self,
        builder: &mut FunctionBuilder,
        defines: &Defines,
        external: &External<FuncRef>,
    ) -> Value {
        self.terms
            .iter()
            .map(|t| t.inline(builder, defines, external))
            .collect::<Vec<_>>()
            .into_iter()
            .reduce(|acc, x| builder.ins().fadd(acc, x))
            .unwrap_or_else(|| builder.ins().f64const(0.0))
    }
}

pub fn compile<E: CodeGen, const N: usize>(
    expr: &E,
    args: [Identifier; N],
) -> Result<fn(&[f64; N]) -> f64, ModuleError> {
    let mut context = JITContext::new()?;

    context
        .get_addr(expr, &args)
        .map(|addr| unsafe { std::mem::transmute(addr) })
}

#[cfg(test)]
mod tests {
    use std::time::Instant;

    use interpreter::{Expr, Expression, Identifier, IntoDefines};

    use super::compile;

    #[test]
    fn simple_jit() {
        let inst = Instant::now();
        let o_expr = Expr::try_from("2 * x + ((x + 3 * (x)) ^ 2) ^ 2 + 2 + x ^ (3 * (x) + x) + x - x + x ^ 2 + x * (1 + 2 + 3) + 1 + 2 + 3 + 1x + 2x + 3x + 1^x + 1^x + 1^x + 1 + 1 + 1 + 1 + 1^x + 1 + 1 + 1 + 1 + 1 + 1^x + 1 + 1 + 1 + 1 + 1 + 1^(x ^ 1 ^ x ^ 1 ^ x ^ 1 ^ x ^ 1 ^ x ^ 1 ^ x ^ 1 ^ x)")
            .unwrap();
        let parse_time = inst.elapsed();

        let inst = Instant::now();
        let expr = o_expr.simplify();
        let simplify_time = inst.elapsed();

        let inst = Instant::now();
        let func = compile(&expr, [Identifier::from('x')]).unwrap();
        let compile_time = inst.elapsed();
        let defines = &('x', 3.0).def();
        let inst = Instant::now();
        let a = expr.evaluate(defines);
        let interpret_time = inst.elapsed();

        let inst = Instant::now();
        let b = func(&[3.0]);
        let jit_time = inst.elapsed();

        let inst = Instant::now();
        let c = o_expr.evaluate(defines);
        let unsimified_time = inst.elapsed();

        println!("Expression: {o_expr}");
        println!("Simplified expression: {expr}");
        println!("Parse time: {parse_time:?}",);
        println!("Simplify time: {simplify_time:?}");
        println!("Compile time: {compile_time:?}");
        println!("Interpret unsimplified time: {unsimified_time:?}");
        println!("Interpret time: {interpret_time:?}");
        println!("JIT time: {jit_time:?}");
        assert_eq!(a, b);

        assert_eq!(a, c);
    }
}
