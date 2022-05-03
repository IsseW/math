use std::{collections::HashMap, fmt::Display};

use num::Zero;

use crate::{Expr, Expression, Factor, Identifier};

use super::Define;

#[derive(Clone, Debug, PartialEq, Default)]
pub struct Term {
    pub sign: bool,
    pub factors: Vec<Factor>,
    pub denominators: Vec<Factor>,
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
        if !self.denominators.is_empty() {
            write!(f, " / (")?;
            for (i, dividend) in self.denominators.iter().enumerate() {
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
    pub fn as_factor(&self) -> Option<(bool, Factor)> {
        if self.denominators.is_empty() {
            match self.factors.len() {
                0 => Some((false, Factor::Number(0.0))),
                1 => Some((self.sign, self.factors[0].clone())),
                _ => None,
            }
        } else {
            None
        }
    }

    pub fn mul(&mut self, other: Term) {
        self.sign = self.sign ^ other.sign;
        let mut filter = |f| match f {
            Factor::Number(mut f) => {
                if f < 0.0 {
                    f = -f;
                    self.sign = !self.sign;
                }
                if f == 1.0 {
                    None
                } else {
                    Some(Factor::Number(f))
                }
            }
            f => Some(f),
        };
        self.factors
            .extend(other.factors.into_iter().filter_map(&mut filter));
        self.denominators
            .extend(other.denominators.into_iter().filter_map(filter));
    }
}

pub trait ToTerm {
    fn term(self) -> Term;
}

impl<T> ToTerm for T
where
    Factor: From<T>,
{
    fn term(self) -> Term {
        let factor = Factor::from(self);
        match factor {
            Factor::Number(f) if f < 0.0 => Term {
                sign: true,
                factors: vec![Factor::Number(-f)],
                denominators: vec![],
            },
            factor => Term {
                factors: vec![factor],
                ..Default::default()
            },
        }
    }
}

impl<T> ToTerm for (bool, T)
where
    Factor: From<T>,
{
    fn term(self) -> Term {
        Term {
            sign: self.0,
            factors: vec![Factor::from(self.1)],
            ..Default::default()
        }
    }
}

impl ToTerm for Vec<Factor> {
    fn term(self) -> Term {
        Term {
            factors: self,
            ..Default::default()
        }
    }
}

impl ToTerm for (Vec<Factor>, Vec<Factor>) {
    fn term(self) -> Term {
        let (factors, dividends) = self;
        Term {
            factors,
            denominators: dividends,
            ..Default::default()
        }
    }
}

impl ToTerm for (bool, Vec<Factor>) {
    fn term(self) -> Term {
        let (sign, factors) = self;
        Term {
            sign,
            factors,
            ..Default::default()
        }
    }
}

impl ToTerm for (bool, &Vec<Factor>) {
    fn term(self) -> Term {
        let (sign, factors) = self;
        Term {
            sign,
            factors: factors.clone(),
            ..Default::default()
        }
    }
}

impl ToTerm for (bool, Vec<Factor>, Vec<Factor>) {
    fn term(self) -> Term {
        let (sign, factors, dividends) = self;
        Term {
            sign,
            factors,
            denominators: dividends,
        }
    }
}

impl ToTerm for (bool, &Vec<Factor>, &Vec<Factor>) {
    fn term(self) -> Term {
        let (sign, factors, dividends) = self;
        Term {
            sign,
            factors: factors.clone(),
            denominators: dividends.clone(),
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
                .denominators
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
                .chain(self.denominators.iter().cloned())
                .collect::<Vec<_>>();
            factors.push(Factor::Group(der));
            terms.push(factors.term());
        }
        for (i, der) in self
            .denominators
            .iter()
            .map(|f| f.derivative(by))
            .enumerate()
        {
            let mut factors = self
                .denominators
                .iter()
                .enumerate()
                .filter(|(j, _)| j != &i)
                .map(|(_, f)| f.clone())
                .chain(self.factors.iter().cloned())
                .collect::<Vec<_>>();
            factors.push(Factor::Group(der));
            terms.push((true, factors).term());
        }

        Expr::from((
            self.sign,
            vec![Factor::group(terms)],
            self.denominators
                .iter()
                .map(|f| Factor::pow(f.clone(), 2.0))
                .collect(),
        ))
    }

    fn simplify_inner(&self) -> (f64, Option<Self>) {
        let mut consts = 1.0;
        let mut factors = Vec::new();
        let mut denominators = Vec::new();

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
                Factor::Number(n) => {
                    consts *= n;
                }
                Factor::Group(t) if let Some(t) = t.as_term() => {
                    if t.factors.is_empty() {
                        return (0.0, None);
                    }
                    for factor in t.factors {
                        match factor {
                            Factor::Number(n) => consts *= n,
                            s => {
                                factors.push(s);
                            }
                        }
                    }
                    extra_div.extend(t.denominators);
                    if t.sign {
                        consts = -consts;
                    }
                }
                s => {
                    let mut mul = true;
                    for i in 0..factors.len() {
                        match (&s, &factors[i]) {
                            (Factor::Pow(b_a, e_a), Factor::Pow(b_b, e_b)) if b_a.eq(b_b) => {
                                let mut e_b = e_b.clone();
                                e_b.add_expr(e_a.clone());
                                let Factor::Pow(b_b, _) = factors.swap_remove(i) else {
                                        unreachable!()
                                    };

                                let (c, simplified) = Factor::Pow(b_b, e_b).simplify_inner();
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
                            (b_a, Factor::Pow(b_b, e_b)) | (Factor::Pow(b_b, e_b), b_a) if match b_a {
                                Factor::Group(e) => e.eq(b_b),
                                e => b_b.as_term().and_then(|e| e.as_factor()).map_or(false, |(sign, b)| !sign && b.eq(e)),
                            } => {
                                let mut e_b = e_b.clone();
                                e_b.add(1.0);

                                let (c, simplified) = Factor::Pow(b_b.clone(), e_b).simplify_inner();
                                if c.is_zero() {
                                    return (0.0, None);
                                }
                                consts *= c;

                                factors.swap_remove(i);
                                if let Some(simplified) = simplified {
                                    factors.push(simplified);
                                };

                                mul = false;
                                break;
                            }
                            (s, f) if s.eq(f) => {
                                let t = factors.swap_remove(i);
                                factors.push(Factor::pow(t, 2.0));
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

        for denominator in self.denominators.iter().chain(extra_div.iter()) {
            let (c, simplified) = denominator.simplify_inner();
            if c.is_zero() {
                panic!("Division by zero");
            }
            consts /= c;
            let Some(simplified) = simplified else {
                continue;
            };
            match simplified {
                Factor::Number(n) => {
                    consts /= n;
                }
                s => {
                    let mut divide = true;
                    for i in 0..factors.len() {
                        match (&s, &factors[i]) {
                            (Factor::Pow(b_a, e_a), Factor::Pow(b_b, e_b)) if b_a.eq(b_b) => {
                                let mut e_b = e_b.clone();
                                e_b.sub_expr(e_a.clone());
                                let Factor::Pow(b_b, _) = factors.swap_remove(i) else {
                                        unreachable!()
                                    };

                                let (c, simplified) = Factor::Pow(b_b, e_b).simplify_inner();
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
                            (b_a, Factor::Pow(b_b, e_b)) | (Factor::Pow(b_b, e_b), b_a)
                                if match b_a {
                                    Factor::Group(e) => e.eq(b_b),
                                    e => b_b
                                        .as_term()
                                        .and_then(|e| e.as_factor())
                                        .map_or(false, |(sign, b)| !sign && b.eq(e)),
                                } =>
                            {
                                let mut e_b = e_b.clone();
                                e_b.sub(1.0);

                                let (c, simplified) =
                                    Factor::Pow(b_b.clone(), e_b).simplify_inner();
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
                        denominators.push(s);
                    }
                }
            }
        }

        if self.sign {
            consts = -consts;
        }

        match (factors.is_empty(), denominators.is_empty()) {
            (false, _) => (
                consts,
                Some(Self {
                    sign: false,
                    factors,
                    denominators,
                }),
            ),
            (true, false) => (
                consts,
                Some(Self {
                    sign: false,
                    factors: vec![Factor::Number(1.0)],
                    denominators,
                }),
            ),
            (true, true) => (consts, None),
        }
    }

    fn as_num(&self) -> Option<f64> {
        if self.denominators.is_empty() {
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

    fn find_identifier(&self) -> Option<Identifier> {
        self.factors
            .iter()
            .chain(self.denominators.iter())
            .find_map(|f| f.find_identifier())
    }

    fn depth(&self) -> usize {
        self.factors
            .iter()
            .chain(self.denominators.iter())
            .map(|f| f.depth())
            .max()
            .unwrap_or(0)
    }
}
