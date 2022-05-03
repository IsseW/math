use std::{collections::HashMap, fmt::Display};

use num::Zero;

use crate::{
    parser::{Error, Full, TextParser, TokenParser},
    Expression, Factor, Identifier, Term,
};

use super::{Define, ToTerm};

#[derive(Clone, Debug, PartialEq)]
pub struct Expr {
    pub terms: Vec<Term>,
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
            .filter_map(|(mut c, mut term)| {
                if c < 0.0 {
                    term.sign = !term.sign;
                    c = -c;
                }
                if c.is_zero() {
                    None
                } else if c == 1.0 {
                    Some(term)
                } else {
                    term.factors.insert(0, Factor::Number(c));
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
                if let Some((sign, factor)) = terms[0].as_factor() {
                    let (mut consts, factor) = factor.simplify_inner();
                    if sign {
                        consts = -consts;
                    }
                    match factor {
                        Some(Factor::Group(expr)) => (consts, Some(expr)),
                        f => (consts, f.map(|f| Expr::from(f))),
                    }
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

    fn find_identifier(&self) -> Option<Identifier> {
        self.terms.iter().find_map(|t| t.find_identifier())
    }

    fn depth(&self) -> usize {
        self.terms.iter().map(|f| f.depth()).max().unwrap_or(0)
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
        match term.as_num() {
            Some(f) => {
                if f == 0.0 {
                    self.terms.clear();
                    return;
                }
                for term in self.terms.iter_mut() {
                    term.mul(f.term());
                }
            }
            _ => {
                if let Some(t) = self.as_term() {
                    term.mul(t);
                    self.terms = vec![term];
                } else {
                    let g = Factor::Group(std::mem::replace(self, Expr::from(vec![term])));
                    self.terms[0].factors.push(g);
                }
            }
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

    pub fn mul_expr(&mut self, expr: Expr) {
        if let (Some(mut a), Some(b)) = (self.as_term(), expr.as_term()) {
            a.mul(b);
            self.terms = vec![a];
        } else {
            let g = Factor::Group(std::mem::replace(self, Expr::from(Factor::Group(expr))));
            self.terms[0].factors.push(g);
        }
    }

    pub fn as_term(&self) -> Option<Term> {
        match self.terms.len() {
            0 => Some(Term::default()),
            1 => Some(self.terms[0].clone()),
            _ => None,
        }
    }

    pub fn into_factor(&self) -> Factor {
        if let Some((sign, factor)) = self.as_term().and_then(|t| t.as_factor()) && !sign {
            factor
        } else {
            Factor::Group(self.clone())
        }
    }

    pub fn simplify(&self) -> Expr {
        let (mut consts, expr) = self.simplify_inner();
        expr.map(|mut e| {
            if consts < 0.0 {
                consts = -consts;
                e.terms.iter_mut().for_each(|t| t.sign = !t.sign);
            }
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
        let tokens = TextParser::new(value).parse().map_err(|e| {
            e.into_iter()
                .map(|e| e.map(Full::Syntax))
                .collect::<Vec<_>>()
        })?;
        let mut errors = Vec::new();

        let mut parser = TokenParser::new(&tokens, &mut errors);
        let statement = parser.parse();
        if !errors.is_empty() {
            return Err(errors.into_iter().map(|e| e.map(Full::Parse)).collect());
        }

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
                    if matches!(factor, Factor::Number(_)) {
                        write!(f, " * ")?;
                    }
                }
                write!(f, "{}", factor)?;
            }
            if !term.denominators.is_empty() {
                write!(f, " / (")?;
                let mut first = true;
                for factor in term.denominators.iter() {
                    if first {
                        first = false;
                    } else {
                        if matches!(factor, Factor::Number(_)) {
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
