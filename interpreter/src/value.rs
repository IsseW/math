use std::{collections::HashMap, fmt::Display};

use num::Zero;

use crate::{
    identifier::Identifier,
    parser::{Error, Full, TextParser},
};

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

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum Func {
    Abs,
    Sin,
    Cos,
    Tan,
    Asin,
    Acos,
    Atan,
    Ln,
}

impl Func {
    pub const fn get_func(&self) -> fn(f64) -> f64 {
        match self {
            Func::Abs => f64::abs,
            Func::Sin => f64::sin,
            Func::Cos => f64::cos,
            Func::Tan => f64::tan,
            Func::Asin => f64::asin,
            Func::Acos => f64::acos,
            Func::Atan => f64::atan,
            Func::Ln => f64::ln,
        }
    }

    pub fn from_str(string: &str) -> Option<Self> {
        match string {
            "abs" => Some(Func::Abs),
            "sin" => Some(Func::Sin),
            "cos" => Some(Func::Cos),
            "tan" => Some(Func::Tan),
            "asin" => Some(Func::Asin),
            "acos" => Some(Func::Acos),
            "atan" => Some(Func::Atan),
            "ln" => Some(Func::Ln),
            _ => None,
        }
    }

    fn evaluate(&self, num: f64) -> f64 {
        self.get_func()(num)
    }

    pub const fn as_str(&self) -> &'static str {
        match self {
            Func::Abs => "abs",
            Func::Sin => "sin",
            Func::Cos => "cos",
            Func::Tan => "tan",
            Func::Asin => "asin",
            Func::Acos => "acos",
            Func::Atan => "atan",
            Func::Ln => "ln",
        }
    }
}

impl Display for Func {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.as_str())
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Fragment {
    Number(f64),
    Identifier(Identifier),
    Group(Expr),
    Func(Func, Expr),
    Pow(Expr, Expr),
    Call(Identifier, Vec<Expr>),
}

impl Fragment {
    pub fn identifier(id: impl Into<Identifier>) -> Fragment {
        Fragment::Identifier(id.into())
    }
    pub fn group(expr: impl Into<Expr>) -> Fragment {
        Fragment::Group(expr.into())
    }
    pub fn function(func: Func, expr: impl Into<Expr>) -> Fragment {
        Fragment::Func(func, expr.into())
    }
    pub fn pow(base: impl Into<Expr>, exponent: impl Into<Expr>) -> Fragment {
        Fragment::Pow(base.into(), exponent.into())
    }
}

impl From<Identifier> for Fragment {
    fn from(identifier: Identifier) -> Self {
        Fragment::Identifier(identifier)
    }
}

impl From<f64> for Fragment {
    fn from(number: f64) -> Self {
        Fragment::Number(number)
    }
}

impl Expression for Fragment {
    fn evaluate(&self, defines: &HashMap<Identifier, Define>) -> f64 {
        match self {
            Fragment::Number(n) => *n,
            Fragment::Identifier(id) => defines
                .get(id)
                .map(|e| e.expr.evaluate(defines))
                .unwrap_or(0.0),
            Fragment::Group(expr) => expr.evaluate(defines),
            Fragment::Func(f, arg) => f.evaluate(arg.evaluate(defines)),
            Fragment::Pow(a, b) => a.evaluate(defines).powf(b.evaluate(defines)),
            Fragment::Call(id, args) => {
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
            Fragment::Number(_) => Expr::empty(),
            Fragment::Identifier(id) => {
                if by.eq(id) {
                    Expr::from(Fragment::Number(1.0))
                } else {
                    Expr::empty()
                }
            }
            Fragment::Group(group) => group.derivative(by),
            Fragment::Func(f, arg) => {
                let inner = arg.derivative(by);
                if inner.is_empty() {
                    Expr::empty()
                } else {
                    match f {
                        // D(|f(x)|) = abs(f(x)) * f'(x) / f(x)
                        Func::Abs => Expr::from((
                            vec![
                                Fragment::Func(Func::Abs, arg.clone()),
                                Fragment::Group(inner),
                            ],
                            vec![Fragment::Group(arg.clone())],
                        )),
                        // D(sin(f(x))) = f'(x) * cos(f(x))
                        Func::Sin => Expr::from(vec![
                            Fragment::Group(inner),
                            Fragment::Func(Func::Cos, arg.clone()),
                        ]),
                        // D(cos(f(x))) = -f'(x) * sin(f(x))
                        Func::Cos => Expr::from((
                            true,
                            vec![
                                Fragment::Group(inner),
                                Fragment::Func(Func::Sin, arg.clone()),
                            ],
                        )),
                        // D(tan(f(x))) = f'(x) / (cos^2(f(x)))
                        Func::Tan => Expr::from((
                            vec![Fragment::Group(inner)],
                            vec![Fragment::pow(Fragment::Func(Func::Cos, arg.clone()), 2.0)],
                        )),
                        // D(asin(f(x))) = f'(x) / sqrt(1 - f(x)^2)
                        Func::Asin => Expr::from((
                            vec![Fragment::Group(inner)],
                            vec![Fragment::pow(
                                vec![1.0.term(), (true, Fragment::pow(arg.clone(), 2.0)).term()],
                                0.5,
                            )],
                        )),
                        // D(asin(f(x))) = -f'(x) / sqrt(1 - f(x)^2)
                        Func::Acos => Expr::from((
                            true,
                            vec![Fragment::Group(inner)],
                            vec![Fragment::pow(
                                vec![1.0.term(), (true, Fragment::pow(arg.clone(), 2.0)).term()],
                                0.5,
                            )],
                        )),
                        // D(atan(f(x))) = f'(x) / (1 + f(x)^2)
                        Func::Atan => Expr::from((
                            true,
                            vec![Fragment::Group(inner)],
                            vec![Fragment::group(vec![
                                1.0.term(),
                                Fragment::pow(arg.clone(), 2.0).term(),
                            ])],
                        )),
                        // D(ln(f(x))) = f'(x) / f(x)
                        Func::Ln => Expr::from((
                            vec![Fragment::Group(inner)],
                            vec![Fragment::Group(arg.clone())],
                        )),
                    }
                }
            }
            Fragment::Pow(a, b) => {
                let a_der = a.derivative(by);
                let b_der = b.derivative(by);

                match (a_der.is_empty(), b_der.is_empty()) {
                    (true, true) => Expr::empty(),
                    (false, true) => {
                        let mut exp = b.clone();
                        exp.terms.push((true, 1.0).term());
                        Expr::from(vec![
                            Fragment::Group(b.clone()),
                            Fragment::Pow(a.clone(), exp),
                            Fragment::Group(a_der),
                        ])
                    }
                    (true, false) => Expr::from(vec![
                        Fragment::Func(Func::Ln, a.clone()),
                        Fragment::Pow(a.clone(), b.clone()),
                        Fragment::Group(b_der),
                    ]),
                    (false, false) => {
                        let mut exp = b.clone();
                        exp.terms.push((true, 1.0).term());
                        Expr::from(vec![
                            Fragment::Pow(a.clone(), exp),
                            Fragment::Group(Expr::from(vec![
                                vec![Fragment::Group(b.clone()), Fragment::Group(a_der)].term(),
                                vec![
                                    Fragment::Group(a.clone()),
                                    Fragment::Func(Func::Ln, a.clone()),
                                    Fragment::Group(b_der),
                                ]
                                .term(),
                            ])),
                        ])
                    }
                }
            }
            Fragment::Call(_, _) => todo!(),
        }
    }

    fn simplify_inner(&self) -> (f64, Option<Self>) {
        let number = |n: f64| (n, None);
        let just = |fragment| (1.0, Some(fragment));
        match &*self {
            Fragment::Number(n) => number(*n),
            Fragment::Identifier(id) => just(Fragment::Identifier(*id)),
            Fragment::Group(expr) => {
                let (consts, simplified) = expr.simplify_inner();

                if let Some(simplified) = &simplified && let Some(term) = simplified.as_term() && let Some((sign, fragment)) = term.as_fragment() {
                    (if sign { -consts } else { consts }, Some(fragment))
                } else {
                    (consts, simplified.map(Fragment::Group))
                }
            }
            Fragment::Func(f, expr) => {
                let (consts, simplified) = expr.simplify_inner();
                if let Some(mut simplified) = simplified {
                    if let Some(arg) = simplified.as_num() {
                        number(f.evaluate(consts * arg))
                    } else {
                        let function = |c, s| (c, Some(Fragment::Func(*f, s)));
                        match *f {
                            Func::Abs => function(consts.abs(), simplified),
                            Func::Sin | Func::Tan | Func::Asin | Func::Acos | Func::Atan => {
                                if consts.abs() != 1.0 {
                                    simplified.mul(consts.abs());
                                }
                                function(consts.signum(), simplified)
                            }
                            Func::Cos => {
                                if consts.abs() != 1.0 {
                                    simplified.mul(consts.abs());
                                }
                                function(1.0, simplified)
                            }
                            _ => {
                                if consts != 1.0 {
                                    simplified.mul(consts.abs());
                                }
                                function(1.0, simplified)
                            }
                        }
                    }
                } else {
                    number(f.evaluate(consts))
                }
            }
            Fragment::Pow(a, b) => {
                let (a_consts, a_simplified) = a.simplify_inner();
                let (b_consts, b_simplified) = b.simplify_inner();
                match (a_simplified, b_simplified) {
                    (Some(a), Some(b)) => {
                        let p = a_consts.powf(b_consts);
                        if p == 0.0 {
                            number(0.0)
                        } else if p == 1.0 {
                            just(Fragment::Pow(a, b))
                        } else {
                            (
                                1.0,
                                Some(Fragment::group(vec![
                                    Fragment::pow(p, b.clone()),
                                    Fragment::Pow(a, b),
                                ])),
                            )
                        }
                    }
                    (Some(a), None) => {
                        if b_consts == 0.0 {
                            number(1.0)
                        } else if b_consts == 1.0 {
                            (a_consts, Some(Fragment::Group(a)))
                        } else {
                            (a_consts.powf(b_consts), Some(Fragment::pow(a, b_consts)))
                        }
                    }
                    (None, Some(b)) => (a_consts.powf(b_consts), Some(Fragment::pow(a_consts, b))),
                    (None, None) => number(a_consts.powf(b_consts)),
                }
            }
            Fragment::Call(id, call) => {
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
                (1.0, Some(Fragment::Call(*id, call)))
            }
        }
    }

    fn as_num(&self) -> Option<f64> {
        match self {
            Fragment::Number(n) => Some(*n),
            _ => None,
        }
    }
}

impl Display for Fragment {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Fragment::Number(num) => write!(f, "{}", num),
            Fragment::Identifier(id) => write!(f, "{}", id),
            Fragment::Group(expr) => write!(f, "({})", expr),
            Fragment::Func(func, arg) => match func {
                Func::Abs => write!(f, "|{}|", arg),
                _ => {
                    write!(f, "{}({})", func, arg)
                }
            },
            Fragment::Pow(lhs, rhs) => write!(f, "({}) ^ ({})", lhs, rhs),
            Fragment::Call(id, args) => {
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

pub trait Expression: Sized + Clone {
    fn evaluate(&self, defines: &HashMap<Identifier, Define>) -> f64;
    fn derivative(&self, by: Identifier) -> Expr;
    fn simplify_inner(&self) -> (f64, Option<Self>);
    fn as_num(&self) -> Option<f64>;
}

#[derive(Clone, Debug, PartialEq, Default)]
pub struct Term {
    pub sign: bool,
    pub factors: Vec<Fragment>,
    pub dividends: Vec<Fragment>,
}

impl Display for Term {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.sign {
            write!(f, "-")?;
        }
        for (i, factor) in self.factors.iter().enumerate() {
            if i > 0 {
                write!(f, " * ")?;
            }
            write!(f, "{}", factor)?;
        }
        if !self.dividends.is_empty() {
            write!(f, " / (")?;
            for (i, dividend) in self.dividends.iter().enumerate() {
                if i > 0 {
                    write!(f, " * ")?;
                }
                write!(f, "{}", dividend)?;
            }
            write!(f, ")")
        } else {
            Ok(())
        }
    }
}

impl Term {
    fn as_fragment(&self) -> Option<(bool, Fragment)> {
        if self.dividends.is_empty() {
            match self.factors.len() {
                0 => Some((false, Fragment::Number(0.0))),
                1 => Some((self.sign, self.factors[0].clone())),
                _ => None,
            }
        } else {
            None
        }
    }

    fn mul(&mut self, other: Term) {
        self.sign = self.sign ^ other.sign;
        self.factors.extend(other.factors);
        self.dividends.extend(other.dividends);
    }
}

pub trait ToTerm {
    fn term(self) -> Term;
}

impl<T> ToTerm for T
where
    Fragment: From<T>,
{
    fn term(self) -> Term {
        Term {
            factors: vec![Fragment::from(self)],
            ..Default::default()
        }
    }
}

impl<T> ToTerm for (bool, T)
where
    Fragment: From<T>,
{
    fn term(self) -> Term {
        Term {
            sign: self.0,
            factors: vec![Fragment::from(self.1)],
            ..Default::default()
        }
    }
}

impl ToTerm for Vec<Fragment> {
    fn term(self) -> Term {
        Term {
            factors: self,
            ..Default::default()
        }
    }
}

impl ToTerm for (Vec<Fragment>, Vec<Fragment>) {
    fn term(self) -> Term {
        let (factors, dividends) = self;
        Term {
            factors,
            dividends,
            ..Default::default()
        }
    }
}

impl ToTerm for (bool, Vec<Fragment>) {
    fn term(self) -> Term {
        let (sign, factors) = self;
        Term {
            sign,
            factors,
            ..Default::default()
        }
    }
}

impl ToTerm for (bool, &Vec<Fragment>) {
    fn term(self) -> Term {
        let (sign, factors) = self;
        Term {
            sign,
            factors: factors.clone(),
            ..Default::default()
        }
    }
}

impl ToTerm for (bool, Vec<Fragment>, Vec<Fragment>) {
    fn term(self) -> Term {
        let (sign, factors, dividends) = self;
        Term {
            sign,
            factors,
            dividends,
        }
    }
}

impl ToTerm for (bool, &Vec<Fragment>, &Vec<Fragment>) {
    fn term(self) -> Term {
        let (sign, factors, dividends) = self;
        Term {
            sign,
            factors: factors.clone(),
            dividends: dividends.clone(),
        }
    }
}

impl Expression for Term {
    fn evaluate(&self, defines: &HashMap<Identifier, Define>) -> f64 {
        (self
            .factors
            .iter()
            .map(|f| f.evaluate(defines))
            .reduce(|acc, x| acc * x)
            .unwrap_or(0.0)
            / self
                .dividends
                .iter()
                .map(|f| f.evaluate(defines))
                .reduce(|acc, x| acc * x)
                .unwrap_or(1.0))
            * if self.sign { -1.0 } else { 1.0 }
    }

    fn derivative(&self, by: Identifier) -> Expr {
        let mut terms = vec![];
        for (i, der) in self.factors.iter().map(|f| f.derivative(by)).enumerate() {
            let mut factors = self
                .factors
                .iter()
                .enumerate()
                .filter(|(j, _)| j != &i)
                .map(|(_, f)| f.clone())
                .chain(self.dividends.iter().cloned())
                .collect::<Vec<_>>();
            factors.push(Fragment::Group(der));
            terms.push(factors.term());
        }
        for (i, der) in self.dividends.iter().map(|f| f.derivative(by)).enumerate() {
            let mut factors = self
                .dividends
                .iter()
                .enumerate()
                .filter(|(j, _)| j != &i)
                .map(|(_, f)| f.clone())
                .chain(self.factors.iter().cloned())
                .collect::<Vec<_>>();
            factors.push(Fragment::Group(der));
            terms.push((true, factors).term());
        }

        Expr::from((
            vec![Fragment::group(terms)],
            self.dividends
                .iter()
                .map(|f| Fragment::pow(f.clone(), 2.0))
                .collect(),
        ))
    }

    fn simplify_inner(&self) -> (f64, Option<Self>) {
        let mut consts = 1.0;
        let mut factors = Vec::new();
        let mut dividends = Vec::new();

        let mut extra_div = Vec::new();

        for factor in &self.factors {
            let (c, simplified) = factor.simplify_inner();
            if c.is_zero() {
                return (0.0, None);
            }
            consts *= c;

            let Some(simplified) = simplified else {
                continue;
            };

            match simplified {
                Fragment::Number(n) => {
                    consts *= n;
                }
                Fragment::Group(t) if let Some(t) = t.as_term() => {
                    if t.factors.is_empty() {
                        return (0.0, None);
                    }
                    for factor in t.factors {
                        match factor {
                            Fragment::Number(n) => consts *= n,
                            s => {
                                factors.push(s);
                            }
                        }
                    }
                    extra_div.extend(t.dividends);
                    if t.sign {
                        consts = -consts;
                    }
                }
                s => {
                    let mut mul = true;
                    for i in 0..factors.len() {
                        match (&s, &factors[i]) {
                            (Fragment::Pow(b_a, e_a), Fragment::Pow(b_b, e_b)) if b_a.eq(b_b) => {
                                let mut e_b = e_b.clone();
                                e_b.add_expr(e_a.clone());
                                let Fragment::Pow(b_b, _) = factors.swap_remove(i) else {
                                        unreachable!()
                                    };

                                let (c, simplified) = Fragment::Pow(b_b, e_b).simplify_inner();
                                if c.is_zero() {
                                    return (0.0, None);
                                }
                                consts *= c;

                                if let Some(simplified) = simplified {
                                    factors.push(simplified);
                                };

                                mul = false;
                                break;
                            }
                            (b_a, Fragment::Pow(b_b, e_b)) | (Fragment::Pow(b_b, e_b), b_a) if match b_a {
                                Fragment::Group(e) => e.eq(b_b),
                                e => b_b.as_term().and_then(|e| e.as_fragment()).map_or(false, |(sign, b)| !sign && b.eq(e)),
                            } => {
                                let mut e_b = e_b.clone();
                                e_b.add(1.0);

                                let (c, simplified) = Fragment::Pow(b_b.clone(), e_b).simplify_inner();
                                if c.is_zero() {
                                    return (0.0, None);
                                }
                                consts *= c;

                                if let Some(simplified) = simplified {
                                    factors.push(simplified);
                                };

                                mul = false;
                                break;
                            }
                            (s, f) if s.eq(f) => {
                                let t = factors.swap_remove(i);
                                factors.push(Fragment::pow(t, 2.0));
                                mul = false;
                                break;
                            }
                            _ => {}
                        }
                    }
                    if mul {
                        factors.push(s);
                    }
                }
            }
        }

        for dividend in self.dividends.iter().chain(extra_div.iter()) {
            let (c, simplified) = dividend.simplify_inner();
            if c.is_zero() {
                panic!("Division by zero");
            }
            consts /= c;
            let Some(simplified) = simplified else {
                continue;
            };
            match simplified {
                Fragment::Number(n) => {
                    consts /= n;
                }
                s => {
                    let mut divide = true;
                    for i in 0..factors.len() {
                        match (&s, &factors[i]) {
                            (Fragment::Pow(b_a, e_a), Fragment::Pow(b_b, e_b)) if b_a.eq(b_b) => {
                                let mut e_b = e_b.clone();
                                e_b.sub_expr(e_a.clone());
                                let Fragment::Pow(b_b, _) = factors.swap_remove(i) else {
                                        unreachable!()
                                    };

                                let (c, simplified) = Fragment::Pow(b_b, e_b).simplify_inner();
                                if c.is_zero() {
                                    return (0.0, None);
                                }
                                consts *= c;

                                let Some(simplified) = simplified else {
                                    continue;
                                };

                                factors.push(simplified);

                                divide = false;
                                break;
                            }
                            (b_a, Fragment::Pow(b_b, e_b)) | (Fragment::Pow(b_b, e_b), b_a)
                                if match b_a {
                                    Fragment::Group(e) => e.eq(b_b),
                                    e => b_b
                                        .as_term()
                                        .and_then(|e| e.as_fragment())
                                        .map_or(false, |(sign, b)| !sign && b.eq(e)),
                                } =>
                            {
                                let mut e_b = e_b.clone();
                                e_b.sub(1.0);

                                let (c, simplified) =
                                    Fragment::Pow(b_b.clone(), e_b).simplify_inner();
                                if c.is_zero() {
                                    return (0.0, None);
                                }
                                consts *= c;

                                if let Some(simplified) = simplified {
                                    factors.push(simplified);
                                };

                                divide = false;
                                break;
                            }
                            (s, f) if s.eq(f) => {
                                factors.swap_remove(i);
                                divide = false;
                                break;
                            }
                            _ => {}
                        }
                    }
                    if divide {
                        dividends.push(s);
                    }
                }
            }
        }

        if self.sign {
            consts = -consts;
        }
        match (factors.is_empty(), dividends.is_empty()) {
            (false, _) => (
                consts,
                Some(Self {
                    sign: false,
                    factors,
                    dividends,
                }),
            ),
            (true, false) => (
                consts,
                Some(Self {
                    sign: false,
                    factors: vec![Fragment::Number(1.0)],
                    dividends,
                }),
            ),
            (true, true) => (consts, None),
        }
    }

    fn as_num(&self) -> Option<f64> {
        if self.dividends.is_empty() {
            match self.factors.len() {
                0 => Some(0.0),
                1 => {
                    if self.sign {
                        self.factors[0].as_num().map(|x| -x)
                    } else {
                        self.factors[0].as_num()
                    }
                }
                _ => None,
            }
        } else {
            None
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Expr {
    pub terms: Vec<Term>,
}

#[derive(Clone, Debug)]
pub struct Define {
    args: Vec<Identifier>,
    expr: Expr,
}

impl<T: ToTerm> From<T> for Define {
    fn from(term: T) -> Self {
        Define {
            args: vec![],
            expr: Expr::from(term),
        }
    }
}

impl From<Expr> for Define {
    fn from(expr: Expr) -> Define {
        Define { args: vec![], expr }
    }
}

impl From<(Identifier, Expr)> for Define {
    fn from((id, expr): (Identifier, Expr)) -> Define {
        Define {
            args: vec![id],
            expr,
        }
    }
}

impl From<(Vec<Identifier>, Expr)> for Define {
    fn from((args, expr): (Vec<Identifier>, Expr)) -> Define {
        Define { args, expr }
    }
}

impl Expression for Expr {
    fn evaluate(&self, defines: &HashMap<Identifier, Define>) -> f64 {
        // Assume undefined defines are 0 for now.
        self.terms
            .iter()
            .map(|term| term.evaluate(defines))
            .sum::<f64>()
    }

    fn derivative(&self, by: Identifier) -> Expr {
        let d = self
            .terms
            .iter()
            .map(|term| term.derivative(by))
            .reduce(|mut acc, x| {
                acc.add_expr(x);
                acc
            })
            .unwrap_or(Expr::empty());
        let res = d.simplify();
        res
    }

    fn simplify_inner(&self) -> (f64, Option<Self>) {
        let mut terms: Vec<(f64, Term)> = Vec::new();
        let mut consts = 0.0;
        for term in &self.terms {
            let (c, simplified) = term.simplify_inner();

            if c.is_zero() {
                continue;
            }

            let Some(simplified) = simplified else {
                consts += c;
                continue;
            };

            terms
                .iter_mut()
                .find(|(_, t)| t.eq(&simplified))
                .map(|(consts, _)| *consts += c)
                .unwrap_or_else(|| terms.push((c, simplified)));
        }
        let mut terms = terms
            .into_iter()
            .filter_map(|(c, mut term)| {
                if c == 0.0 {
                    None
                } else if c == 1.0 {
                    Some(term)
                } else {
                    term.factors.insert(0, Fragment::Number(c));
                    Some(term)
                }
            })
            .collect::<Vec<_>>();
        if consts != 0.0 {
            terms.push((consts < 0.0, consts.abs()).term());
        }
        match terms.len() {
            0 => (0.0, None),
            1 => {
                if let Some((sign, fragment)) = terms[0].as_fragment() {
                    let (mut consts, fragment) = fragment.simplify_inner();
                    if sign {
                        consts = -consts;
                    }
                    (consts, fragment.map(|f| Expr::from(f)))
                } else {
                    let (consts, term) = terms[0].simplify_inner();
                    (consts, term.map(|t| Expr { terms: vec![t] }))
                }
            }
            _ => (1.0, Some(Expr { terms })),
        }
    }

    fn as_num(&self) -> Option<f64> {
        match self.terms.len() {
            0 => Some(0.0),
            1 => self.terms[0].as_num(),
            _ => None,
        }
    }
}

impl Expr {
    pub fn empty() -> Expr {
        Expr { terms: Vec::new() }
    }

    pub fn is_empty(&self) -> bool {
        self.terms.is_empty()
    }

    pub fn add(&mut self, term: impl ToTerm) {
        self.terms.push(term.term());
    }

    pub fn sub(&mut self, term: impl ToTerm) {
        let mut term = term.term();
        term.sign = !term.sign;
        self.terms.push(term);
    }

    pub fn mul(&mut self, term: impl ToTerm) {
        let mut term = term.term();
        if let Some(t) = self.as_term() {
            term.mul(t);
            self.terms = vec![term];
        } else {
            let g = Fragment::Group(std::mem::replace(self, Expr::from(vec![term])));
            self.terms[0].factors.push(g);
        }
    }

    pub fn add_expr(&mut self, expr: Expr) {
        self.terms.extend(expr.terms);
    }

    pub fn sub_expr(&mut self, mut expr: Expr) {
        expr.terms
            .iter_mut()
            .for_each(|term| term.sign = !term.sign);
        self.terms.extend(expr.terms);
    }

    fn as_term(&self) -> Option<Term> {
        match self.terms.len() {
            0 => Some(Term::default()),
            1 => Some(self.terms[0].clone()),
            _ => None,
        }
    }

    pub fn simplify(&self) -> Expr {
        let (consts, expr) = self.simplify_inner();
        expr.map(|mut e| {
            if consts == 0.0 {
                Expr::empty()
            } else if consts == 1.0 {
                e
            } else {
                e.mul(consts);
                e
            }
        })
        .unwrap_or_else(|| Expr::from(consts))
    }
}

impl TryFrom<&str> for Expr {
    type Error = Vec<Error<Full>>;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        let statement = TextParser::new(value)
            .parse()
            .map_err(|e| {
                e.into_iter()
                    .map(|e| e.map(Full::Syntax))
                    .collect::<Vec<_>>()
            })?
            .parse()
            .map_err(|e| {
                e.into_iter()
                    .map(|e| e.map(Full::Parse))
                    .collect::<Vec<_>>()
            })?;
        Ok(statement
            .as_expr()
            .ok_or_else(|| vec![Error::new(Full::NotExpression, 0..=value.len() - 1)])?
            .clone())
    }
}

impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.is_empty() {
            return write!(f, "0");
        }
        let mut first = true;
        for term in self.terms.iter() {
            if !first {
                if term.sign {
                    write!(f, " - ")?;
                } else {
                    write!(f, " + ")?;
                }
            } else {
                first = false;
                if term.sign {
                    write!(f, "-")?;
                }
            }
            let mut first = true;
            for factor in term.factors.iter() {
                if first {
                    first = false;
                } else {
                    if matches!(factor, Fragment::Number(_)) {
                        write!(f, " * ")?;
                    }
                }
                write!(f, "{}", factor)?;
            }
            if !term.dividends.is_empty() {
                write!(f, " / (")?;
                let mut first = true;
                for factor in term.dividends.iter() {
                    if first {
                        first = false;
                    } else {
                        if matches!(factor, Fragment::Number(_)) {
                            write!(f, " * ")?;
                        }
                    }
                    write!(f, "{}", factor)?;
                }
                write!(f, ")")?;
            }
        }
        Ok(())
    }
}

impl From<Vec<Term>> for Expr {
    fn from(terms: Vec<Term>) -> Self {
        Expr { terms }
    }
}

impl<T: ToTerm> From<T> for Expr {
    fn from(term: T) -> Self {
        Expr {
            terms: vec![term.term()],
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

pub trait IntoDefines {
    fn def(self) -> HashMap<Identifier, Define>;
}

impl<I: Into<Identifier>, D: Into<Define>> IntoDefines for (I, D) {
    fn def(self) -> HashMap<Identifier, Define> {
        let (ident, def) = self;
        let ident = ident.into();
        let def = def.into();
        let mut map = HashMap::new();
        map.insert(ident, def);
        map
    }
}

impl<I: Into<Identifier> + Clone, D: Into<Define> + Clone> IntoDefines for &[(I, D)] {
    fn def(self) -> HashMap<Identifier, Define> {
        self.into_iter()
            .map(|(i, d)| (i.clone().into(), d.clone().into()))
            .collect()
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
        let expr = Expr::try_from("2(3(x^2))^2 + (x^5)/x").unwrap();
        let id = Identifier::from('x');
        let der = expr.derivative(id);
        println!("end result: {der}");
    }
}
