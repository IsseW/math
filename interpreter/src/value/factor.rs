use std::{collections::HashMap, fmt::Display};

use num::Zero;

use crate::{Expr, Expression, Func, Identifier};

use super::{Define, ToTerm};

#[derive(Clone, Debug, PartialEq)]
pub enum Factor {
    Number(f64),
    Identifier(Identifier),
    Group(Expr),
    Func(Func, Expr),
    Pow(Expr, Expr),
    Call(Identifier, Vec<Expr>),
}

impl Factor {
    pub fn identifier(id: impl Into<Identifier>) -> Factor {
        Factor::Identifier(id.into())
    }
    pub fn group(expr: impl Into<Expr>) -> Factor {
        Factor::Group(expr.into())
    }
    pub fn function(func: Func, expr: impl Into<Expr>) -> Factor {
        Factor::Func(func, expr.into())
    }
    pub fn pow(base: impl Into<Expr>, exponent: impl Into<Expr>) -> Factor {
        Factor::Pow(base.into(), exponent.into())
    }
}

impl From<Identifier> for Factor {
    fn from(identifier: Identifier) -> Self {
        Factor::Identifier(identifier)
    }
}

impl From<f64> for Factor {
    fn from(number: f64) -> Self {
        Factor::Number(number)
    }
}

impl Expression for Factor {
    fn evaluate(&self, defines: &HashMap<Identifier, Define>) -> f64 {
        match self {
            Factor::Number(n) => *n,
            Factor::Identifier(id) => defines
                .get(id)
                .map(|e| e.expr.evaluate(defines))
                .unwrap_or(0.0),
            Factor::Group(expr) => expr.evaluate(defines),
            Factor::Func(f, arg) => f.evaluate(arg.evaluate(defines)),
            Factor::Pow(a, b) => a.evaluate(defines).powf(b.evaluate(defines)),
            Factor::Call(id, args) => {
                if let Some(def) = defines.get(id) {
                    if args.len() == def.args.len() {
                        let mut defs = defines.clone();
                        for (id, arg) in def
                            .args
                            .iter()
                            .zip(args.iter().map(|arg| arg.evaluate(defines)))
                        {
                            defs.insert(
                                *id,
                                Define {
                                    args: Vec::new(),
                                    expr: Expr::from(arg),
                                },
                            );
                        }
                        def.expr.evaluate(&defs)
                    } else {
                        panic!("Invalid number of arguments for function {}", id);
                    }
                } else {
                    0.0
                }
            }
        }
    }

    fn derivative(&self, by: Identifier) -> Expr {
        match self {
            Factor::Number(_) => Expr::empty(),
            Factor::Identifier(id) => {
                if by.eq(id) {
                    Expr::from(Factor::Number(1.0))
                } else {
                    Expr::empty()
                }
            }
            Factor::Group(group) => group.derivative(by),
            Factor::Func(f, arg) => {
                let inner = arg.derivative(by);
                if inner.is_empty() {
                    Expr::empty()
                } else {
                    match f {
                        // D(|f(x)|) = abs(f(x)) * f'(x) / f(x)
                        Func::Abs => Expr::from((
                            vec![Factor::Func(Func::Abs, arg.clone()), Factor::Group(inner)],
                            vec![Factor::Group(arg.clone())],
                        )),
                        // D(sin(f(x))) = f'(x) * cos(f(x))
                        Func::Sin => Expr::from(vec![
                            Factor::Group(inner),
                            Factor::Func(Func::Cos, arg.clone()),
                        ]),
                        // D(cos(f(x))) = -f'(x) * sin(f(x))
                        Func::Cos => Expr::from((
                            true,
                            vec![Factor::Group(inner), Factor::Func(Func::Sin, arg.clone())],
                        )),
                        // D(tan(f(x))) = f'(x) / (cos^2(f(x)))
                        Func::Tan => Expr::from((
                            vec![Factor::Group(inner)],
                            vec![Factor::pow(Factor::Func(Func::Cos, arg.clone()), 2.0)],
                        )),
                        // D(asin(f(x))) = f'(x) / sqrt(1 - f(x)^2)
                        Func::Asin => Expr::from((
                            vec![Factor::Group(inner)],
                            vec![Factor::pow(
                                vec![1.0.term(), (true, Factor::pow(arg.clone(), 2.0)).term()],
                                0.5,
                            )],
                        )),
                        // D(asin(f(x))) = -f'(x) / sqrt(1 - f(x)^2)
                        Func::Acos => Expr::from((
                            true,
                            vec![Factor::Group(inner)],
                            vec![Factor::pow(
                                vec![1.0.term(), (true, Factor::pow(arg.clone(), 2.0)).term()],
                                0.5,
                            )],
                        )),
                        // D(atan(f(x))) = f'(x) / (1 + f(x)^2)
                        Func::Atan => Expr::from((
                            true,
                            vec![Factor::Group(inner)],
                            vec![Factor::group(vec![
                                1.0.term(),
                                Factor::pow(arg.clone(), 2.0).term(),
                            ])],
                        )),
                        // D(ln(f(x))) = f'(x) / f(x)
                        Func::Ln => Expr::from((
                            vec![Factor::Group(inner)],
                            vec![Factor::Group(arg.clone())],
                        )),
                    }
                }
            }
            Factor::Pow(a, b) => {
                let a_der = a.derivative(by);
                let b_der = b.derivative(by);

                match (a_der.is_empty(), b_der.is_empty()) {
                    (true, true) => Expr::empty(),
                    (false, true) => {
                        let mut exp = b.clone();
                        exp.terms.push((true, 1.0).term());
                        Expr::from(vec![
                            Factor::Group(b.clone()),
                            Factor::Pow(a.clone(), exp),
                            Factor::Group(a_der),
                        ])
                    }
                    (true, false) => Expr::from(vec![
                        Factor::Func(Func::Ln, a.clone()),
                        Factor::Pow(a.clone(), b.clone()),
                        Factor::Group(b_der),
                    ]),
                    (false, false) => {
                        let mut exp = b.clone();
                        exp.terms.push((true, 1.0).term());
                        Expr::from(vec![
                            Factor::Pow(a.clone(), exp),
                            Factor::Group(Expr::from(vec![
                                vec![Factor::Group(b.clone()), Factor::Group(a_der)].term(),
                                vec![
                                    Factor::Group(a.clone()),
                                    Factor::Func(Func::Ln, a.clone()),
                                    Factor::Group(b_der),
                                ]
                                .term(),
                            ])),
                        ])
                    }
                }
            }
            Factor::Call(_, _) => todo!(),
        }
    }

    fn simplify_inner(&self) -> (f64, Option<Self>) {
        let number = |n: f64| (n, None);
        let just = |fragment| (1.0, Some(fragment));
        match &*self {
            Factor::Number(n) => number(*n),
            Factor::Identifier(id) => just(Factor::Identifier(*id)),
            Factor::Group(expr) => {
                let (consts, simplified) = expr.simplify_inner();

                if let Some(simplified) = &simplified && let Some(term) = simplified.as_term() && let Some((sign, fragment)) = term.as_factor() {
                    (if sign { -consts } else { consts }, Some(fragment))
                } else {
                    (consts, simplified.map(Factor::Group))
                }
            }
            Factor::Func(f, expr) => {
                let (consts, simplified) = expr.simplify_inner();
                if let Some(mut simplified) = simplified {
                    if let Some(arg) = simplified.as_num() {
                        number(f.evaluate(consts * arg))
                    } else {
                        let function = |c, s| (c, Some(Factor::Func(*f, s)));

                        fn remove_effectless(simplified: &mut Expr) {
                            if let Some(mut term) = simplified.as_term() {
                                term.factors
                                    .iter_mut()
                                    .chain(term.denominators.iter_mut())
                                    .for_each(|factor| match factor {
                                        Factor::Func(Func::Abs, expr) => {
                                            *factor = expr.into_factor()
                                        }
                                        Factor::Pow(a, _) => {
                                            remove_effectless(a);
                                        }
                                        _ => {}
                                    });
                                *simplified = Expr::from(vec![term]);
                            }
                        }

                        match *f {
                            Func::Abs => {
                                remove_effectless(&mut simplified);
                                function(consts.abs(), simplified)
                            }
                            Func::Sin | Func::Tan | Func::Asin | Func::Acos | Func::Atan => {
                                if consts.abs() != 1.0 {
                                    simplified.mul(consts.abs());
                                }
                                function(consts.signum(), simplified)
                            }
                            Func::Cos => {
                                remove_effectless(&mut simplified);
                                simplified.mul(consts.abs());
                                function(1.0, simplified)
                            }
                            _ => {
                                simplified.mul(consts);
                                function(1.0, simplified)
                            }
                        }
                    }
                } else {
                    number(f.evaluate(consts))
                }
            }
            Factor::Pow(a, b) => {
                let (mut a_consts, mut a_simplified) = a.simplify_inner();
                let (mut b_consts, mut b_simplified) = b.simplify_inner();
                if a_consts.is_zero() {
                    return number(0.0);
                }
                if b_consts.is_zero() {
                    return number(1.0);
                }
                if let Some((sign, Factor::Pow(a_a, mut a_b))) =
                                a_simplified.as_ref().and_then(|a| a.as_term()).and_then(|term| term.as_factor()) && !sign {
                                    b_simplified.map(|b| a_b.mul_expr(b));
                                    a_b.mul(b_consts);
                                    (b_consts, b_simplified) = a_b.simplify_inner();
                                    (a_consts, a_simplified) = a_a.simplify_inner();
                                }
                match (a_simplified, b_simplified) {
                    (Some(a), Some(mut b)) => {
                        let p = a_consts.powf(b_consts);
                        if p == 0.0 {
                            number(0.0)
                        } else if p == 1.0 {
                            if b_consts != 1.0 {
                                b.mul(b_consts);
                            }

                            just(Factor::Pow(a, b))
                        } else {
                            just(Factor::group(vec![
                                Factor::pow(p, b.clone()),
                                Factor::Pow(a, b),
                            ]))
                        }
                    }
                    (Some(a), None) => {
                        if b_consts == 0.0 {
                            number(1.0)
                        } else if b_consts == 1.0 {
                            (a_consts, Some(Factor::Group(a)))
                        } else {
                            (a_consts.powf(b_consts), Some(Factor::pow(a, b_consts)))
                        }
                    }
                    (None, Some(b)) => (a_consts.powf(b_consts), Some(Factor::pow(a_consts, b))),
                    (None, None) => number(a_consts.powf(b_consts)),
                }
            }
            Factor::Call(id, call) => {
                let call = call
                    .iter()
                    .map(|arg| {
                        let (consts, arg) = arg.simplify_inner();
                        if let Some(mut arg) = arg {
                            arg.mul(consts);
                            arg
                        } else {
                            Expr::empty()
                        }
                    })
                    .collect();
                (1.0, Some(Factor::Call(*id, call)))
            }
        }
    }

    fn as_num(&self) -> Option<f64> {
        match self {
            Factor::Number(n) => Some(*n),
            _ => None,
        }
    }

    fn find_identifier(&self) -> Option<Identifier> {
        match self {
            Factor::Identifier(id) => Some(*id),
            Factor::Group(e) | Factor::Func(_, e) => e.find_identifier(),
            Factor::Pow(a, b) => a.find_identifier().or_else(|| b.find_identifier()),
            Factor::Call(_, e) => e.iter().find_map(|f| f.find_identifier()),
            _ => None,
        }
    }

    fn depth(&self) -> usize {
        match self {
            Factor::Group(e) | Factor::Func(_, e) => e.depth() + 1,
            Factor::Pow(a, b) => a.depth().max(b.depth()) + 1,
            Factor::Call(_, e) => e.iter().map(|f| f.depth() + 1).max().unwrap_or(0),
            _ => 0,
        }
    }
}

impl Display for Factor {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Factor::Number(num) => write!(f, "{}", num),
            Factor::Identifier(id) => write!(f, "{}", id),
            Factor::Group(expr) => write!(f, "({})", expr),
            Factor::Func(func, arg) => match func {
                Func::Abs => write!(f, "|{}|", arg),
                _ => {
                    write!(f, "{}({})", func, arg)
                }
            },
            Factor::Pow(lhs, rhs) => write!(f, "({}) ^ ({})", lhs, rhs),
            Factor::Call(id, args) => {
                write!(f, "{}(", id)?;
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", arg)?;
                }
                write!(f, ")")
            }
        }
    }
}
