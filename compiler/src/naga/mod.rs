use interpreter::{Func, Identifier};
use naga::*;

fn map_mf(func: Func) -> MathFunction {
    use MathFunction::*;
    match func {
        Func::Abs => Abs,
        Func::Sin => Sin,
        Func::Cos => Cos,
        Func::Tan => Tan,
        Func::Asin => Asin,
        Func::Acos => Acos,
        Func::Atan => Atan,
        Func::Ln => Log,
    }
}

pub trait CodeGen {
    fn inline(&self);
}

pub fn compile<E: CodeGen, const N: usize>(expr: &E, args: [Identifier; N]) -> Function {
    let t = TypeInner::Scalar {
        kind: ScalarKind::Float,
        width: 4,
    };
    let mut m = Module {
        types: todo!(),
        constants: todo!(),
        global_variables: todo!(),
        functions: todo!(),
        entry_points: todo!(),
    };

    let mut f = Function {
        name: Some("f".to_string()),
        arguments: args
            .iter()
            .map(|arg| FunctionArgument {
                name: Some(format!("{}", arg)),
                ty: todo!(),
                binding: None,
            })
            .collect(),
        result: todo!(),
        local_variables: todo!(),
        expressions: todo!(),
        named_expressions: todo!(),
        body: todo!(),
    };
}
