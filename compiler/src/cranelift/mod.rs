use std::{collections::HashMap, mem::size_of_val};

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
use cranelift_module::{FuncId, Linkage, Module};
use interpreter::*;

pub use cranelift_module::ModuleError;

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

pub struct Cache {
    cached: HashMap<u64, Value>,
}

impl Cache {
    fn new() -> Self {
        Self {
            cached: HashMap::new(),
        }
    }

    fn get(
        &mut self,
        value: &impl CodeGen,
        builder: &mut FunctionBuilder,
        defines: &Defines,
        external: &External<FuncRef>,
    ) -> Result<Value, ModuleError> {
        let hash = value.unordered_hash();
        if let Some(cached) = self.cached.get(&hash) {
            Ok(cached.clone())
        } else {
            let value = value.codegen(builder, defines, external, self)?;
            self.cached.insert(hash, value.clone());
            Ok(value)
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

        let mut cache = Cache::new();
        let val = expr.inline(&mut builder, &defines, &external, &mut cache)?;
        
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

pub trait CodeGen: UnorderedHash + Sized {
    fn codegen(
        &self,
        builder: &mut FunctionBuilder,
        defines: &Defines,
        external: &External<FuncRef>,
        cache: &mut Cache,
    ) -> Result<Value, ModuleError>;

    fn inline(
        &self,
        builder: &mut FunctionBuilder,
        defines: &Defines,
        external: &External<FuncRef>,
        cache: &mut Cache,
    ) -> Result<Value, ModuleError> {
        cache.get(self, builder, defines, external)
    }
}

impl CodeGen for Fraction {
    fn codegen(
        &self,
        builder: &mut FunctionBuilder,
        defines: &Defines,
        external: &External<FuncRef>,
        cache: &mut Cache,
    ) -> Result<Value, ModuleError> {
        Ok(builder.ins().f64const(f64::from(*self)))
    }
}

impl CodeGen for Factor {
    fn codegen(
        &self,
        builder: &mut FunctionBuilder,
        defines: &Defines,
        external: &External<FuncRef>,
        cache: &mut Cache,
    ) -> Result<Value, ModuleError> {
        Ok(match self {
            Factor::Number(n) => builder.ins().f64const(*n),
            Factor::Identifier(id) => defines
                .get(id)
                .ok_or_else(|| ModuleError::Undeclared(format!("{}", id)))?
                .clone(),
            Factor::Group(g) => g.inline(builder, defines, external, cache)?,
            Factor::Func(f, arg) => {
                let arg = arg.inline(builder, defines, external, cache)?;
                match f {
                    Func::Abs => builder.ins().fabs(arg),
                    f => {
                        let c = builder.ins().call(external.functions[index_of(f)], &[arg]);
                        builder.func.dfg.first_result(c)
                    }
                }
            }
            Factor::Pow(a, b) => {
                if let Some(num) = b.as_num() && num.is_integer() {
                    fn pow(builder: &mut FunctionBuilder, defines: &Defines, external: &External<FuncRef>, cache: &mut Cache, a: &Expr, exp: u64) -> Result<Value, ModuleError> {
                        Ok(if exp == 0 {
                            cache.get(&frac!(1), builder, defines, external)?
                        } else if exp == 1 {
                            a.inline(builder, defines, external, cache)?
                        } else {
                            let hash = a.pow_hash(&Fraction::whole(exp));
                            if let Some(n) = cache.cached.get(&hash) {
                                *n
                            } else {
                                let n = 1 << (size_of_val(&exp) as u64 * 8 - 1 - exp.leading_zeros() as u64);
                                let rest = exp & !n;
                                let res = if rest == 0 {
                                    let n = pow(builder, defines, external, cache, a, n / 2)?;
                                    builder.ins().fmul(n, n)
                                } else {
                                    let rest = pow(builder, defines, external, cache, a, rest)?;
                                    let n = pow(builder, defines, external, cache, a, n)?;
                                    builder.ins().fmul(rest, n)
                                };
                                cache.cached.insert(hash, res);
                                res
                            }
                        })
                    }
                    pow(builder, defines, external, cache, a, num.n())?
                } else {
                    let a = a.inline(builder, defines, external, cache)?;
                    let b = b.inline(builder, defines, external, cache)?;
                    let c = builder.ins().call(external.libc_pow, &[a, b]);
                    builder.func.dfg.first_result(c)
                }
            }
            Factor::Call(i, _) => return Err(ModuleError::Undeclared(format!("{}", i))),
        })
    }
}

impl CodeGen for Term {
    fn codegen(
        &self,
        builder: &mut FunctionBuilder,
        defines: &Defines,
        external: &External<FuncRef>,
        cache: &mut Cache,
    ) -> Result<Value, ModuleError> {
        if self.consts.is_zero() {
            return cache.get(&frac!(0), builder, defines, external);
        }
        let factors = self
                .factors
                .iter()
                .map(|f| f.inline(builder, defines, external, cache))
                .try_collect::<Vec<_>>()?;
        let n = if self.consts.n() == 1 {
            if let Some(factors) = factors.into_iter().reduce(|acc, x| builder.ins().fmul(acc, x)) {
                if self.consts.is_pos() {
                    factors
                } else {
                    builder.ins().fneg(factors)
                }
            } else if self.consts.is_pos() {
                builder.ins().f64const(1.0)
            } else {
                builder.ins().f64const(-1.0)
            }
        } else {
            factors
                .into_iter()
                .fold(self.consts.codegen(builder, defines, external, cache)?, |acc, x| builder.ins().fmul(acc, x))
        };

        let v = if let Some(d) = self
            .denominators
            .iter()
            .map(|d| d.inline(builder, defines, external, cache))
            .try_collect::<Vec<_>>()?
            .into_iter()
            .reduce(|acc, x| builder.ins().fmul(acc, x))
        {
            builder.ins().fdiv(n, d)
        } else {
            n
        };

        Ok(v)
    }
}

impl CodeGen for Expr {
    fn codegen(
        &self,
        builder: &mut FunctionBuilder,
        defines: &Defines,
        external: &External<FuncRef>,
        cache: &mut Cache,
    ) -> Result<Value, ModuleError> {
        Ok(self
            .terms
            .iter()
            .map(|t| t.inline(builder, defines, external, cache))
            .try_collect::<Vec<_>>()?
            .into_iter()
            .reduce(|acc, x| builder.ins().fadd(acc, x))
            .unwrap_or_else(|| builder.ins().f64const(0.0)))
    }
}

pub type CFunc<const N: usize> = fn(&[f64; N]) -> f64;

pub fn compile<E: CodeGen, const N: usize>(
    expr: &E,
    args: &[Identifier; N],
) -> Result<CFunc<N>, ModuleError> {
    let mut context = JITContext::new()?;

    context
        .get_addr(expr, args)
        .map(|addr| unsafe { std::mem::transmute(addr) })
}

#[cfg(test)]
mod tests {
    use std::{time::Instant, hint::black_box};

    use interpreter::{Expr, Expression, Identifier, IntoDefines};

    use super::compile;

    #[test]
    fn simple_jit() {
        let inst = Instant::now();
        // let o_expr = Expr::try_from("2 * x + ((x + 3 * (x)) ^ 4) ^ 2 + 2 + x ^ (3 * (x) + x) + x - x + x ^ 2 + x * (1 + 2 + 3) + 1 + 2 + 3 + 1x + 2x + 3x + 1^x + 1^x + 1^x + 1 + 1 + 1 + 1 + 1^x + 1 + 1 + 1 + 1 + 1 + 1^x + 1 + 1 + 1 + 1 + 1 + 1^(x ^ 1 ^ x ^ 1 ^ x ^ 1 ^ x ^ 1 ^ x ^ 1 ^ x ^ 1 ^ x)")
        //     .unwrap();
        let o_expr = Expr::try_from("x^(1000)+x^3+x^2+x^22+x^11+x^69").unwrap();
        let parse_time = inst.elapsed();

        let inst = Instant::now();
        let expr = o_expr.simplify();
        let simplify_time = inst.elapsed();

        let inst = Instant::now();
        let func = compile(&expr, &[Identifier::from('x')]).unwrap();
        let compile_time = inst.elapsed();
        let defines = &('x', 3.0).def();

        // Heat up the cache
        black_box(o_expr.evaluate(defines));

        let inst = Instant::now();
        let raw = o_expr.evaluate(defines);
        let unsimified_time = inst.elapsed();

        let inst = Instant::now();
        let simplified = expr.evaluate(defines);
        let interpret_time = inst.elapsed();

        let inst = Instant::now();
        let jited = func(&[3.0]);
        let jit_time = inst.elapsed();


        println!("Expression: {o_expr}");
        println!("Simplified expression: {expr}");
        println!("Parse time: {parse_time:?}",);
        println!("Simplify time: {simplify_time:?}");
        println!("Compile time: {compile_time:?}");
        println!("Interpret unsimplified time: {unsimified_time:?}");
        println!("Interpret time: {interpret_time:?}");
        println!("JIT time: {jit_time:?}");
        assert_eq!(simplified, jited);

        assert_eq!(simplified, raw);
    }
}
