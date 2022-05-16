use std::{collections::HashMap, fmt::Display};

use crate::{identifier::Identifier, parser::EqualityOperator};

mod define;
mod expr;
mod factor;
mod fraction;
mod func;
mod term;

pub use define::{Define, IntoDefines};
pub use expr::{expr_from_str, Expr};
pub use factor::Factor;
pub use fraction::{Fraction, Sign};
pub use func::Func;
pub use term::{Term, ToTerm};

pub trait UnorderedHash {
    fn unordered_hash(&self) -> u64;

    fn unordered_eq(&self, other: &Self) -> bool {
        self.unordered_hash() == other.unordered_hash()
    }
}

pub trait Expression: Sized + Clone {
    fn evaluate(&self, defines: &HashMap<Identifier, Define>) -> f64;
    fn derivative(&self, by: Identifier) -> Expr;
    fn as_num(&self) -> Option<Fraction>;
    fn find_identifier(&self) -> Option<Identifier>;
    fn depth(&self) -> usize;
    fn sign(&self) -> Option<Sign>;
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

    use crate::frac;

    use super::*;
    #[test]
    fn test_display() {
        let expr = Expr {
            terms: vec![
                frac!(1).term(),
                frac!(2).term(),
                (Fraction::neg_one(), Identifier::from(('e', 1))).term(),
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

    #[test]
    fn unordered() {
        fn test(a: &str, b: &str) {
            let a = Expr::try_from(a).unwrap();
            let b = Expr::try_from(b).unwrap();

            assert_eq!(a.unordered_hash(), b.unordered_hash());
        }
        test("x + 1", "1 + x");
        test("2x", "x*2");
        test("x^(1+x)", "x^(x+1)");
        test("(x + 1) ^ (1 + x)", "(1 + x) ^ (x + 1)");
        test("((x + 1) ^ 2 + 1) ^ 2", "(1 + (1 + x) ^ 2) ^ 2");
        {
            let mut a = ('a'..='z').fold(String::new(), |mut acc: String, c| {
                acc.push(c);
                acc.push_str("+");
                acc
            });
            a.push('1');
            let mut b = ('a'..='z').rev().fold(String::new(), |mut acc: String, c| {
                acc.push(c);
                acc.push_str("+");
                acc
            });
            b.push('1');
            test(&a, &b);
        }
    }

    #[test]
    fn unordered_e() {
        let factor = Factor::identifier('x');
        let expr = Expr::from(factor.clone());
        let factor = Factor::Group(expr.clone());
        assert_eq!(factor.unordered_hash(), expr.unordered_hash());
    }
}
