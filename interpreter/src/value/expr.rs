use std::{collections::HashMap, fmt::Display, mem};

use itertools::Itertools;
use rayon::iter::{IntoParallelRefIterator, ParallelIterator};

use crate::{
    parser::{Error, Full, TextParser, TokenParser},
    value::term::Factors,
    Expression, Factor, Fraction, Identifier, Term,
};

use super::{Define, ToTerm, UnorderedHash};

#[derive(Clone, Debug, PartialEq, Default)]
pub struct Expr {
    pub terms: Vec<Term>,
}

impl UnorderedHash for Expr {
    fn unordered_hash(&self) -> u64 {
        #[cfg(feature = "tracy")]
        profiling::scope!("Expr::unordered_hash");
        self.terms
            .iter()
            .map(|term| term.unordered_hash())
            .enumerate()
            .fold(0, |a, (i, b)| a ^ b ^ i as u64)
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
        #[cfg(feature = "tracy")]
        profiling::scope!("Expr::derivative");
        let d = self
            .terms
            .par_iter()
            .map(|term| term.derivative(by))
            .reduce(
                || Expr::empty(),
                |mut a, b| {
                    a.add_expr(b);
                    a
                },
            );
        let res = d.simplify();
        res
    }

    fn as_num(&self) -> Option<Fraction> {
        match self.terms.len() {
            0 => Some(Fraction::one()),
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

    fn sign(&self) -> Option<crate::Sign> {
        self.as_term().map(|t| t.sign()).flatten()
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
        term.consts = -term.consts;
        self.terms.push(term);
    }

    pub fn mul(&mut self, term: impl ToTerm) {
        let mut term = term.term();
        match term.as_num() {
            Some(f) => {
                if f.is_zero() {
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

    pub fn mul_const(&mut self, f: Fraction) {
        for term in self.terms.iter_mut() {
            term.mul_const(f);
        }
    }

    pub fn sub_expr(&mut self, mut expr: Expr) {
        expr.terms
            .iter_mut()
            .for_each(|term| term.consts = -term.consts);
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

    pub fn unwrap(self) -> Expr {
        if let (f, Some(Factor::Group(e))) = self.as_factor() && f == Fraction::one() {
            e
        } else {
            self
        }
    }

    pub fn as_term(&self) -> Option<Term> {
        match self.terms.len() {
            0 => Some(Term::default()),
            1 => Some(self.terms[0].clone()),
            _ => None,
        }
    }

    pub fn as_factor(&self) -> (Fraction, Option<Factor>) {
        self.as_term()
            .map(|t| t.as_factor())
            .unwrap_or_else(|| (Fraction::one(), Some(Factor::Group(self.clone()))))
    }

    pub fn simplify(&self) -> Expr {
        let expr = self.simplify_inner();
        expr.unwrap_or_default()
    }

    pub(super) fn simplify_inner(&self) -> Option<Self> {
        #[cfg(feature = "tracy")]
        profiling::scope!("Expr::simplify_inner");
        let simplified_terms = self
            .terms
            .par_iter()
            .filter_map(|term| term.simplify_inner())
            .map(|term| {
                let mut factors: Vec<(Fraction, Factors, Factors)> =
                    vec![(term.consts, Factors::default(), Factors::default())];
                for factor in term.factors {
                    match factor {
                        Factor::Group(expr) => {
                            factors = expr
                                .terms
                                .into_iter()
                                .cartesian_product(factors)
                                .map(|(term, mut t)| {
                                    t.0 *= term.consts;
                                    for factor in term.factors {
                                        if let Some(factor) = t.2.div_by(factor) {
                                            t.1.mul_by(factor)
                                        }
                                    }
                                    for denominator in term.denominators {
                                        if let Some(denominator) = t.1.div_by(denominator) {
                                            t.2.mul_by(denominator)
                                        }
                                    }
                                    t
                                })
                                .collect();
                        }
                        factor => factors.iter_mut().for_each(|f| {
                            if let Some(factor) = f.2.div_by(factor.clone()) {
                                f.1.mul_by(factor)
                            }
                        }),
                    }
                }
                for denominator in term.denominators {
                    factors.iter_mut().for_each(|f| {
                        if let Some(denominator) = f.1.div_by(denominator.clone()) {
                            f.2.mul_by(denominator)
                        }
                    })
                }
                factors
                    .into_iter()
                    .map(|(mut consts, f, d)| {
                        let t = Term {
                            factors: f.flatten(|c| consts *= c),
                            denominators: d.flatten(|c| consts /= c),
                            consts,
                        };

                        (t.hash_without_const(), t)
                    })
                    .collect::<Vec<_>>()
            })
            .collect::<Vec<_>>();

        let mut terms: HashMap<u64, Term> = HashMap::with_capacity(self.terms.len());
        for (hash, term) in simplified_terms
            .into_iter()
            .map(|inner| inner.into_iter())
            .flatten()
        {
            terms
                .entry(hash)
                .and_modify(|f| f.consts += term.consts)
                .or_insert(term);
        }
        let mut terms = terms
            .into_iter()
            .map(|(_, term)| term)
            .filter(|term| !term.consts.is_zero())
            .collect::<Vec<_>>();
        if terms.len() == 0 {
            return None;
        }
        if terms[0].consts.is_neg() {
            if let Some(i) =
                terms
                    .iter()
                    .enumerate()
                    .find_map(|(i, t)| if t.consts.is_pos() { Some(i) } else { None })
            {
                terms.swap(0, i);
            } else {
                terms.iter_mut().for_each(|t| t.consts = -t.consts);
                let mut t = mem::replace(&mut terms, vec![]);
                if t.len() == 1 {
                    let mut t = t.pop().unwrap();
                    t.consts = -t.consts;
                    terms.push(t);
                } else {
                    terms.push((Fraction::neg_one(), Factor::group(t)).term());
                }
            }
        }
        let expr = Expr::from(terms);
        Some(expr)
    }
}

pub fn expr_from_str(value: &str, macros: &HashMap<&str, Expr>) -> Result<Expr, Vec<Error<Full>>> {
    let tokens = TextParser::new(value).parse(macros).map_err(|e| {
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

impl TryFrom<&str> for Expr {
    type Error = Vec<Error<Full>>;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        expr_from_str(value, &HashMap::new())
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
                if term.consts.is_neg() {
                    write!(f, " - ")?;
                } else {
                    write!(f, " + ")?;
                }
            } else {
                first = false;
                if term.consts.is_neg() {
                    write!(f, "-")?;
                }
            }
            let mut first = true;
            if term.consts.n() != 1 || term.factors.is_empty() {
                write!(f, "{}", term.consts.n())?;
                first = false;
            }
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
            if !term.denominators.is_empty() || term.consts.d() != 1 {
                write!(f, " / ")?;
                let parenth =
                    term.denominators.len() + if term.consts.d() != 1 { 1 } else { 0 } > 1;
                if parenth {
                    write!(f, "(")?;
                }
                let mut first = true;
                if term.consts.d() != 1 {
                    write!(f, "{}", term.consts.d())?;
                    first = false;
                }
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
                if parenth {
                    write!(f, ")")?;
                }
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
