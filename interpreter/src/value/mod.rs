use std::{collections::HashMap, fmt::Display};

use crate::{identifier::Identifier, parser::EqualityOperator};

mod define;
mod expr;
mod factor;
mod func;
mod term;

pub use define::{Define, IntoDefines};
pub use expr::Expr;
pub use factor::Factor;
pub use func::Func;
pub use term::{Term, ToTerm};

pub trait Expression: Sized + Clone {
    fn evaluate(&self, defines: &HashMap<Identifier, Define>) -> f64;
    fn derivative(&self, by: Identifier) -> Expr;
    fn simplify_inner(&self) -> (f64, Option<Self>);
    fn as_num(&self) -> Option<f64>;
    fn find_identifier(&self) -> Option<Identifier>;
    fn depth(&self) -> usize;
}

pub struct Equation {
    left: Expr,
    right: Expr,
    operator: EqualityOperator,
}

impl Display for Equation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {} {}", self.left, self.operator, self.right)
    }
}

pub struct SetConstructor {
    variables: Vec<Identifier>,
    equations: Vec<Equation>,
}

pub enum Rhs {
    Expr(Expr),
    Equation(Equation),
    Set(SetConstructor),
}

pub enum Statement {
    Single(Rhs),
    Assignment(Identifier, Rhs),
}

impl Statement {
    pub fn expr(expr: Expr) -> Self {
        Self::Single(Rhs::Expr(expr))
    }

    pub fn equation(eq: Equation) -> Self {
        Self::Single(Rhs::Equation(eq))
    }

    pub fn set(set: SetConstructor) -> Self {
        Self::Single(Rhs::Set(set))
    }

    pub fn as_expr(&self) -> Option<&Expr> {
        match self {
            Statement::Single(Rhs::Expr(expr)) => Some(expr),
            _ => None,
        }
    }
}

#[cfg(test)]
mod tests {

    use super::*;
    #[test]
    fn test_display() {
        let expr = Expr {
            terms: vec![
                1.0.term(),
                (true, 2.0).term(),
                (true, Identifier::from(('e', 1))).term(),
            ],
        };

        assert_eq!(format!("{}", expr), "1 - 2 - e_1");
    }

    #[test]
    fn temp() {
        let expr = Expr::try_from("x ^ (3x + x ^ 2)").unwrap();
        let defs = &('x', 2.0).def();
        let simplified = expr.simplify();
        println!("end result: {simplified}");
        assert_eq!(expr.evaluate(defs), simplified.evaluate(defs))
    }
}
