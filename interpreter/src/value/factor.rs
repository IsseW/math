use std::{collections::HashMap, fmt::Display, iter::once};

use crate::{
    frac,
    identifier::{Alpha, AlphaKind},
    Expr, Expression, Fraction, Func, Identifier, Sign, Term,
};

use super::{fraction::Signed, Define, ToTerm, UnorderedHash};

#[derive(Clone, Debug, PartialEq)]
pub enum Factor {
    Number(f64),
    Identifier(Identifier),
    Group(Expr),
    Func(Func, Expr),
    Pow(Expr, Expr),
    Call(Identifier, Vec<Expr>),
}

impl UnorderedHash for Factor {
    fn unordered_hash(&self) -> u64 {
        #[cfg(feature = "tracy")]
        profiling::scope!("Factor::unordered_hash");
        match self {
            // TODO: Is this good enoguh?
            Factor::Number(n) => unsafe { std::mem::transmute(*n) },
            Factor::Identifier(i) => i.simple_hash(),
            Factor::Group(e) => e.unordered_hash(),
            Factor::Func(f, e) => {
                (*f as u64).wrapping_add(1).rotate_left(37) ^ e.unordered_hash().rotate_right(*f as u32)
            }
            Factor::Pow(a, b) => a.pow_hash(b),
            Factor::Call(id, e) => {
                let func = id.simple_hash().rotate_left(29);
                let args = e
                    .iter()
                    .map(|e| e.unordered_hash())
                    .enumerate()
                    .fold(0, |a, (i, b)| a ^ b ^ !i as u64);
                args ^ func
            }
        }
    }
}

impl Factor {
    pub fn to_expr(self) -> Expr {
        match self {
            Factor::Group(e) => e,
            _ => Expr::from(self),
        }
    }

    pub fn inner_hash(&self) -> u64 {
        match self {
            Factor::Pow(a, _) => a.unordered_hash(),
            _ => self.unordered_hash(),
        }
    }

    pub fn inner(self) -> Expr {
        match self {
            Factor::Pow(a, _) => a,
            Factor::Group(e) => e,
            _ => Expr::from(self),
        }
    }

    pub fn inner_const(&self) -> Option<Fraction> {
        match self {
            Factor::Pow(a, _) => a.as_num(),
            _ => self.as_num(),
        }
    }

    pub fn exponent(&self) -> Option<Expr> {
        match self {
            Factor::Pow(_, e) => Some(e.clone()),
            _ => None,
        }
    }

    pub fn identifier(id: impl Into<Identifier>) -> Factor {
        Factor::Identifier(id.into())
    }
    pub fn group(expr: impl Into<Expr>) -> Factor {
        Factor::Group(expr.into())
    }

    pub fn from_expr(expr: impl Into<Expr>) -> (Fraction, Option<Factor>) {
        let expr: Expr = expr.into();
        expr.as_factor()
    }

    pub fn function(func: Func, expr: impl Into<Expr>) -> Factor {
        Factor::Func(func, expr.into())
    }
    pub fn pow(base: impl Into<Expr>, exponent: impl Into<Expr>) -> Factor {
        Factor::Pow(base.into(), exponent.into())
    }

    pub(super) fn simplify_inner(&self) -> (Fraction, Option<Self>) {
        #[cfg(feature = "tracy")]
        profiling::scope!("Factor::simplify_innter");
        let number = |n: Fraction| (n, None);
        let just = |fragment| (Fraction::one(), Some(fragment));
        match &*self {
            Factor::Number(f) => Fraction::from_f64(*f)
                .map(|f| (f, None))
                .unwrap_or((Fraction::one(), Some(Factor::Number(*f)))),
            Factor::Identifier(id) => just(Factor::Identifier(*id)),
            Factor::Group(expr) => {
                let simplified = expr.simplify_inner();

                simplified
                    .map(|e| e.as_factor())
                    .unwrap_or(number(Fraction::zero()))
            }
            Factor::Func(f, expr) => {
                let simplified = expr.simplify_inner();
                if let Some(mut simplified) = simplified {
                    let function = |c, s: Expr| (c, Some(Factor::Func(*f, s.unwrap())));

                    if let Some(t) = simplified.as_term() {
                        let sin = |consts: Fraction| match consts.d() {
                            1 => Some(number(Fraction::zero())),
                            2 => Some(number(match consts.n() % 4 {
                                1 => frac!(1),
                                3 => frac!(-1),
                                _ => frac!(),
                            })),
                            3 => Some((
                                match consts.n() % 6 {
                                    1 | 2 => frac!(1 / 2),
                                    4 | 5 => frac!(-1 / 2),
                                    _ => unreachable!(),
                                },
                                Some(Factor::pow(3, frac!(1 / 2))),
                            )),
                            4 => Some((
                                frac!(1),
                                Some(Factor::group((
                                    match consts.n() % 8 {
                                        1 | 3 => frac!(1),
                                        5 | 7 => frac!(-1),
                                        _ => unreachable!(),
                                    },
                                    Vec::new(),
                                    vec![Factor::pow(2, frac!(1 / 2))],
                                ))),
                            )),
                            5 => {
                                let first = || {
                                    Some(Factor::pow(
                                        (
                                            frac!(1 / 8),
                                            Factor::group(vec![
                                                5.term(),
                                                (frac!(-1), Factor::pow(5, frac!(1 / 2))).term(),
                                            ]),
                                        ),
                                        frac!(1 / 2),
                                    ))
                                };
                                let second = || {
                                    Some(Factor::pow(
                                        (
                                            frac!(1 / 8),
                                            Factor::group(vec![
                                                5.term(),
                                                Factor::pow(5, frac!(1 / 2)).term(),
                                            ]),
                                        ),
                                        frac!(1 / 2),
                                    ))
                                };
                                Some(match consts.n() % 10 {
                                    1 | 4 => (frac!(1), first()),
                                    2 | 3 => (frac!(1), second()),
                                    7 | 8 => (frac!(-1), second()),
                                    6 | 9 => (frac!(-1), first()),
                                    _ => unreachable!(),
                                })
                            }
                            6 => Some(number(match consts.n() % 12 {
                                2 | 4 => frac!(1 / 2),
                                8 | 12 => frac!(-1 / 2),
                                _ => unreachable!(),
                            })),
                            8 => {
                                let first = |f| {
                                    (
                                        f / 2,
                                        Some(Factor::pow(
                                            vec![
                                                2.term(),
                                                (frac!(-1), Factor::pow(2, frac!(1 / 2))).term(),
                                            ],
                                            frac!(1 / 2),
                                        )),
                                    )
                                };
                                let second = |f| {
                                    (
                                        f / 2,
                                        Some(Factor::pow(
                                            vec![2.term(), Factor::pow(2, frac!(1 / 2)).term()],
                                            frac!(1 / 2),
                                        )),
                                    )
                                };
                                Some(match consts.n() % 16 {
                                    1 | 7 => first(frac!(1)),
                                    3 | 5 => second(frac!(1)),
                                    9 | 15 => first(frac!(-1)),
                                    11 | 13 => second(frac!(-1)),
                                    _ => unreachable!(),
                                })
                            }
                            10 => {
                                let first = |f| {
                                    (
                                        f / 4,
                                        Some(Factor::group(vec![
                                            Factor::pow(5, frac!(1 / 2)).term(),
                                            1.term(),
                                        ])),
                                    )
                                };
                                let second = |f| {
                                    (
                                        f / 4,
                                        Some(Factor::group(vec![
                                            Factor::pow(5, frac!(1 / 2)).term(),
                                            frac!(-1).term(),
                                        ])),
                                    )
                                };
                                Some(match consts.n() % 20 {
                                    1 | 9 => first(frac!(1)),
                                    3 | 7 => second(frac!(1)),
                                    11 | 19 => first(frac!(-1)),
                                    13 | 17 => second(frac!(-1)),
                                    _ => unreachable!(),
                                })
                            }
                            12 => {
                                let first = |f| {
                                    (
                                        f / 4,
                                        Some(Factor::group(vec![
                                            Factor::pow(6, frac!(1 / 2)).term(),
                                            (frac!(-1), Factor::pow(2, frac!(1 / 2))).term(),
                                        ])),
                                    )
                                };
                                let second = |f| {
                                    (
                                        f / 4,
                                        Some(Factor::group(vec![
                                            Factor::pow(6, frac!(1 / 2)).term(),
                                            Factor::pow(2, frac!(1 / 2)).term(),
                                        ])),
                                    )
                                };
                                Some(match consts.n() % 24 {
                                    1 | 11 => first(frac!(1)),
                                    3 | 9 => second(frac!(1)),
                                    13 | 23 => first(frac!(-1)),
                                    15 | 21 => second(frac!(-1)),
                                    _ => unreachable!(),
                                })
                            }
                            _ => None,
                        };
                        match f {
                            Func::Abs => match t.sign() {
                                Some(Sign::Pos) => {
                                    return t.as_factor();
                                }
                                Some(Sign::Neg) => {
                                    let (c, f) = t.as_factor();
                                    return (-c, f);
                                }
                                _ => {}
                            },
                            Func::Sin => {
                                if t.is('π') {
                                    if let Some(t) = sin(t.consts) {
                                        return t;
                                    }
                                } else if t.is('τ') {
                                    if let Some(t) = sin(t.consts * 2) {
                                        return t;
                                    }
                                }
                            }
                            Func::Cos => {
                                if t.is('π') {
                                    if let Some(t) = sin(t.consts - frac!(1 / 2)) {
                                        return t;
                                    }
                                } else if t.is('τ') {
                                    if let Some(t) = sin(t.consts * 2 - frac!(1 / 2)) {
                                        return t;
                                    }
                                }
                            }
                            Func::Ln => {
                                if t.is('e') && t.consts == frac!(1) {
                                    return number(frac!(1));
                                }
                            }
                            _ => {}
                        }
                    }
                    fn remove_effectless(simplified: &mut Expr) {
                        if let Some(mut term) = simplified.as_term() {
                            let mut consts = term.consts;
                            fn filter(
                                mut consts: impl FnMut(Fraction),
                            ) -> impl FnMut(Factor) -> Option<Factor> {
                                move |factor| match factor {
                                    Factor::Func(Func::Abs, expr) => {
                                        let (c, f) = expr.as_factor();
                                        consts(c);
                                        f
                                    }
                                    Factor::Pow(mut a, b) => {
                                        remove_effectless(&mut a);
                                        Some(Factor::Pow(a, b))
                                    }
                                    f => Some(f),
                                }
                            }
                            let factors = term
                                .factors
                                .into_iter()
                                .filter_map(filter(|c| consts *= c))
                                .collect::<Vec<_>>();
                            let denominators = term
                                .denominators
                                .into_iter()
                                .filter_map(filter(|c| consts /= c))
                                .collect::<Vec<_>>();
                            term.consts = consts;
                            *simplified = Expr::from((consts, factors, denominators));
                        }
                    }
                    let consts = simplified
                        .as_term()
                        .map(|t| t.consts)
                        .unwrap_or(Fraction::one());
                    match *f {
                        Func::Abs => {
                            remove_effectless(&mut simplified);
                            simplified.mul(consts.inverse());
                            function(consts.abs(), simplified)
                        }
                        Func::Sin | Func::Tan | Func::Asin | Func::Acos | Func::Atan => {
                            if consts.is_neg() {
                                simplified.mul_const(Fraction::neg_one());
                            }
                            function(consts.signum(), simplified)
                        }
                        Func::Cos => {
                            remove_effectless(&mut simplified);
                            if consts.is_neg() {
                                simplified.mul_const(Fraction::neg_one());
                            }
                            function(Fraction::one(), simplified)
                        }
                        Func::Ln if let Some(term) = simplified.as_term() && term.as_num().is_none() => {
                            let terms = term.factors.into_iter().map(|f| {
                                let mut consts = frac!(1);
                                let mut factors = Vec::new();
                                if let Some(e) = f.exponent() {
                                    let (c, factor) = Factor::from_expr(e);
                                    consts *= c;
                                    if let Some(factor) = factor {
                                        factors.push(factor);
                                    }
                                }
                                factors.push(Factor::function(Func::Ln, f.inner()));
                                Term {
                                    consts,
                                    factors,
                                    denominators: Vec::new(),
                                }
                            }).chain(term.denominators.into_iter().map(|f| {
                                let mut consts = frac!(-1);
                                let mut factors = Vec::new();
                                if let Some(e) = f.exponent() {
                                    let (c, factor) = Factor::from_expr(e);
                                    consts *= c;
                                    if let Some(factor) = factor {
                                        factors.push(factor);
                                    }
                                }
                                factors.push(Factor::function(Func::Ln, f.inner()));
                                Term {
                                    consts,
                                    factors,
                                    denominators: Vec::new(),
                                }
                            }));

                            if term.consts == frac!(1) {
                                Factor::from_expr(terms.collect::<Vec<_>>())
                            } else {
                                Factor::from_expr(terms.chain(once(Factor::function(Func::Ln, term.consts).term())).collect::<Vec<_>>())
                            }
                        }
                        _ => function(Fraction::one(), simplified),
                    }
                } else {
                    match f {
                        Func::Abs | Func::Sin | Func::Tan | Func::Asin | Func::Atan => {
                            number(Fraction::zero())
                        }
                        Func::Cos => number(Fraction::one()),
                        Func::Acos => (
                            frac!(1 / 2),
                            Some(Factor::Identifier(Identifier::new(
                                Alpha {
                                    kind: AlphaKind::Pi,
                                    upper: false,
                                },
                                None,
                            ))),
                        ),
                        Func::Ln => panic!("Ln(0) is neg inf"),
                    }
                }
            }
            Factor::Pow(a, b) => {
                let Some(a_simplified) = a.simplify_inner() else {
                    return (Fraction::zero(), None);
                };
                let Some(b_simplified) = b.simplify_inner() else {
                    return (Fraction::one(), None);
                };
                if b_simplified
                    .as_num()
                    .map(|f| f == Fraction::one())
                    .unwrap_or_default()
                {
                    return a_simplified.as_factor();
                }
                match (a_simplified.as_term(), b_simplified.as_term()) {
                    (Some(a), Some(mut b)) => {
                        if a.consts.is_zero() {
                            return number(Fraction::zero());
                        }
                        if b.consts.is_zero() {
                            return number(Fraction::one());
                        }
                        let mut consts = Fraction::one();

                        let Some(mut factors) = a.factors.into_iter().map(|base| {
                            if let Factor::Pow(base, mut exp) = base {
                                exp.mul(b.clone());
                                let (f, p) = Factor::Pow(base, exp).simplify_inner();
                                consts *= f;
                                p
                            } else {
                                Some(Factor::pow(base, b.clone()))
                            }
                        }).try_collect::<Vec<Factor>>() else {
                            return number(Fraction::zero())
                        };

                        let Some(denominators) = a.denominators.into_iter().map(|base| {
                            if let Factor::Pow(base, mut exp) = base {
                                exp.mul(b.clone());
                                let (f, p) = Factor::Pow(base, exp).simplify_inner();
                                consts /= f;
                                p
                            } else {
                                Some(Factor::pow(base, b.clone()))
                            }
                        }).try_collect::<Vec<Factor>>() else {
                            return number(Fraction::one())
                        };

                        consts *= if let Some(c) = b.as_num().and_then(|e| a.consts.pow(e)) {
                            c
                        } else {
                            if let Some(c) = a.consts.pow(b.consts) {
                                b.consts = Fraction::one();
                                if c != Fraction::one() {
                                    factors.insert(0, Factor::pow(c, b))
                                }
                            } else {
                                if a.consts != Fraction::one() {
                                    factors.insert(0, Factor::pow(a.consts, b))
                                }
                            }
                            Fraction::one()
                        };

                        if factors.is_empty() && denominators.is_empty() {
                            number(consts)
                        } else {
                            let (c, f) = Expr::from((factors, denominators)).as_factor();
                            (consts * c, f)
                        }
                    }
                    _ => just(Factor::Pow(a_simplified, b_simplified)),
                }
            }
            Factor::Call(id, call) => {
                let call = call
                    .iter()
                    .map(|arg| arg.simplify_inner().unwrap_or_default())
                    .collect();
                (Fraction::one(), Some(Factor::Call(*id, call)))
            }
        }
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
                .or_else(|| id.associated_value())
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
        #[cfg(feature = "tracy")]
        profiling::scope!("Factor::derivative");
        match self {
            Factor::Number(_) => Expr::empty(),
            Factor::Identifier(id) => {
                if by.eq(id) {
                    Expr::from(1)
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
                            Fraction::neg_one(),
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
                                vec![
                                    Fraction::one().term(),
                                    (
                                        Fraction::neg_one(),
                                        Factor::pow(arg.clone(), Fraction::whole(2)),
                                    )
                                        .term(),
                                ],
                                frac!(1 / 2),
                            )],
                        )),
                        // D(asin(f(x))) = -f'(x) / sqrt(1 - f(x)^2)
                        Func::Acos => Expr::from((
                            Fraction::neg_one(),
                            vec![Factor::Group(inner)],
                            vec![Factor::pow(
                                vec![
                                    Fraction::one().term(),
                                    (
                                        Fraction::neg_one(),
                                        Factor::pow(arg.clone(), Fraction::whole(2)),
                                    )
                                        .term(),
                                ],
                                frac!(1 / 2),
                            )],
                        )),
                        // D(atan(f(x))) = f'(x) / (1 + f(x)^2)
                        Func::Atan => Expr::from((
                            Fraction::one(),
                            vec![Factor::Group(inner)],
                            vec![Factor::group(vec![
                                Fraction::one().term(),
                                Factor::pow(arg.clone(), Fraction::whole(2)).term(),
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
                        exp.sub(Fraction::one());
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
                        exp.sub(Fraction::one());
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

    fn as_num(&self) -> Option<Fraction> {
        match self {
            Factor::Group(e) => e.as_num(),
            _ => None,
        }
    }

    fn find_identifier(&self) -> Option<Identifier> {
        match self {
            Factor::Identifier(id) => {
                if id.associated_value().is_none() {
                    Some(*id)
                } else {
                    None
                }
            }
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

    fn sign(&self) -> Option<Sign> {
        match self {
            Factor::Group(e) => e.sign(),
            Factor::Func(f, e) => match f {
                Func::Abs | Func::Acos => Some(Sign::Pos),
                _ => None,
            },
            Factor::Number(n) => Some(n.sign()),
            Factor::Identifier(id) => id.associated_value().map(|v| v.sign()),
            _ => None,
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
            Factor::Pow(lhs, rhs) => {
                let parenth_check = |e: &Expr| {
                    e.terms.len() == 1
                        && e.terms[0].denominators.len() == 0
                        && ((e.terms[0].factors.len() == 1 && e.terms[0].consts == Fraction::one())
                            || e.terms[0].factors.is_empty())
                };
                if rhs.as_num() == Some(frac!(1 / 2)) {
                    if parenth_check(&lhs) {
                        write!(f, "√{}", lhs)
                    } else {
                        write!(f, "√({})", lhs)
                    }
                } else {
                    if parenth_check(&lhs) {
                        write!(f, "{lhs} ^ ")?;
                    } else {
                        write!(f, "({lhs}) ^ ")?;
                    }
                    if parenth_check(&rhs) {
                        write!(f, "{rhs}")
                    } else {
                        write!(f, "({rhs})")
                    }
                }
            }
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
