#![feature(int_log, let_chains, if_let_guard, let_else)]

use std::collections::HashMap;

use value::Define;

mod identifier;
pub mod parser;
mod value;

pub use identifier::Identifier;
pub use value::{Expr, Expression, Fragment, Func, IntoDefines, Term};

struct Interpreter {
    pub defines: HashMap<Identifier, Define>,
}

#[cfg(test)]
mod tests {

    #[test]
    fn interpreter() {}
}
