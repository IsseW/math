use std::fmt::Display;

use crate::identifier::Identifier;

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub enum Operator {
    Add,
    Sub,
    Mul,
    Div,
    Pow,
}

impl Operator {
    pub fn from_char(c: char) -> Option<Self> {
        match c {
            '+' => Some(Operator::Add),
            '-' => Some(Operator::Sub),
            '*' => Some(Operator::Mul),
            '/' => Some(Operator::Div),
            '^' => Some(Operator::Pow),
            _ => None,
        }
    }
}

#[derive(Clone, Copy, PartialEq, Debug)]
pub enum EqualityOperator {
    Equal,
    NotEqual,
    Less,
    LessOrEqual,
    Greater,
    GreaterOrEqual,
}

impl Display for EqualityOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            EqualityOperator::Equal => write!(f, "="),
            EqualityOperator::NotEqual => write!(f, "!="),
            EqualityOperator::Less => write!(f, "<"),
            EqualityOperator::LessOrEqual => write!(f, "<="),
            EqualityOperator::Greater => write!(f, ">"),
            EqualityOperator::GreaterOrEqual => write!(f, ">="),
        }
    }
}

#[derive(Clone, Copy, PartialEq, Debug)]
pub enum Function {
    Abs,
    Sin,
    Cos,
    Tan,
    Asin,
    Acos,
    Atan,
    Defined(Identifier),
}

impl Function {
    pub fn from_str(string: &str) -> Option<Self> {
        match string {
            "abs" => Some(Function::Abs),
            "sin" => Some(Function::Sin),
            "cos" => Some(Function::Cos),
            "tan" => Some(Function::Tan),
            "asin" => Some(Function::Asin),
            "acos" => Some(Function::Acos),
            "atan" => Some(Function::Atan),
            _ => None,
        }
    }
}

impl Display for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Function::Abs => write!(f, "abs"),
            Function::Sin => write!(f, "sin"),
            Function::Cos => write!(f, "cos"),
            Function::Tan => write!(f, "tan"),
            Function::Asin => write!(f, "asin"),
            Function::Acos => write!(f, "acos"),
            Function::Atan => write!(f, "atan"),
            Function::Defined(identifier) => write!(f, "{}", identifier),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Fragment {
    Number(f64),
    Identifier(Identifier),
    Group(Expr),
    Function(Function, Vec<Expr>),
    Division(Expr, Expr),
    Pow(Expr, Expr),
}

impl Display for Fragment {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Fragment::Number(num) => write!(f, "{}", num),
            Fragment::Identifier(id) => write!(f, "{}", id),
            Fragment::Group(expr) => write!(f, "({})", expr),
            Fragment::Function(func, args) => match func {
                Function::Abs => write!(f, "|{}|", args[0]),
                _ => {
                    write!(f, "{}(", func)?;
                    for (i, arg) in args.iter().enumerate() {
                        if i > 0 {
                            write!(f, ", ")?;
                        }
                        write!(f, "{}", arg)?;
                    }
                    write!(f, ")")
                }
            },
            Fragment::Division(lhs, rhs) => write!(f, "({})/({})", lhs, rhs),
            Fragment::Pow(lhs, rhs) => write!(f, "({} ^ {})", lhs, rhs),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Expr {
    fragments: Vec<(bool, Vec<Fragment>)>,
}

impl Expr {
    fn simplify(&mut self) {
        
    }
}

impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut first = true;
        for (sign, factors) in self.fragments.iter() {
            if !first {
                if *sign {
                    write!(f, " - ")?;
                } else {
                    write!(f, " + ")?;
                }
            } else {
                first = false;
                if *sign {
                    write!(f, "-")?;
                }
            }
            let mut first = true;
            for factor in factors.iter() {
                if first {
                    first = false;
                } else {
                    if matches!(factor, Fragment::Number(_)) {
                        write!(f, " * ")?;
                    }
                }
                write!(f, "{}", factor)?;
            }
        }
        Ok(())
    }
}

impl From<Vec<(bool, Vec<Fragment>)>> for Expr {
    fn from(fragments: Vec<(bool, Vec<Fragment>)>) -> Self {
        Expr { fragments }
    }
}

impl From<Vec<Fragment>> for Expr {
    fn from(fragments: Vec<Fragment>) -> Self {
        Expr {
            fragments: vec![(false, fragments)],
        }
    }
}

impl From<Fragment> for Expr {
    fn from(fragment: Fragment) -> Self {
        Expr {
            fragments: vec![(false, vec![fragment])],
        }
    }
}

impl From<(bool, Fragment)> for Expr {
    fn from((sign, fragment): (bool, Fragment)) -> Self {
        Expr {
            fragments: vec![(sign, vec![fragment])],
        }
    }
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

pub enum Statement {
    Expr(Expr),
    Equation(Equation),
}

#[cfg(test)]
mod tests {
    use crate::identifier::{Alpha, AlphaNumerical};

    use super::*;
    #[test]
    fn test_display() {
        let expr = Expr {
            fragments: vec![
                (false, vec![Fragment::Number(1.0)]),
                (true, vec![Fragment::Number(2.0)]),
                (
                    true,
                    vec![Fragment::Identifier(Identifier::new(
                        Alpha::from_char('e').unwrap(),
                        Some(AlphaNumerical::Numerical(1)),
                    ))],
                ),
            ],
        };

        assert_eq!(format!("{}", expr), "1 - 2 - e_1");
    }
}
