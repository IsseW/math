use std::{
    collections::{hash_map::DefaultHasher, HashMap},
    fmt::{self, Display},
    hash::{Hash, Hasher},
};

use crate::{Define, Expr, Expression, Factor, Fraction, Identifier, Sign};

use super::UnorderedHash;

#[derive(Clone, Debug, PartialEq, Default)]
pub struct Term {
    pub consts: Fraction,
    pub factors: Vec<Factor>,
    pub denominators: Vec<Factor>,
}

impl UnorderedHash for Term {
    fn unordered_hash(&self) -> u64 {
        let n = self.consts.n().wrapping_sub(1);
        let d = self.consts.d().wrapping_sub(1);
        let consts = ((self.consts.is_neg() as u64) << 63) ^ n.rotate_right(7) ^ d.rotate_left(11);

        self.hash_without_const() ^ consts
    }
}

impl Display for Term {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fn write_multiplication(
            f: &mut fmt::Formatter<'_>,
            consts: u64,
            factors: &[Factor],
        ) -> fmt::Result {
            let mut first = true;
            if consts != 1 {
                write!(f, "{}", consts)?;
                first = false;
            }
            for factor in factors {
                if !first {
                    write!(f, " * ")?;
                }
                write!(f, "{}", factor)?;
                first = false;
            }
            Ok(())
        }
        if self.consts.is_neg() {
            write!(f, "-")?;
        }
        write_multiplication(f, self.consts.n(), &self.factors)?;

        if !self.denominators.is_empty() || self.consts.d() != 1 {
            write!(f, " / (")?;
            write_multiplication(f, self.consts.d(), &self.denominators)?;
            write!(f, ")")
        } else {
            Ok(())
        }
    }
}

#[derive(Default, Clone, Debug)]
pub(crate) struct Factors {
    factors: HashMap<u64, Factor>,
}

impl Factors {
    pub fn with_capacity(capacity: usize) -> Self {
        Self {
            factors: HashMap::with_capacity(capacity),
        }
    }

    pub fn mul_by(&mut self, factor: Factor) {
        let hash = factor.inner_hash();
        self.factors
            .entry(hash)
            .and_modify(|f| match f {
                Factor::Pow(_, b) => {
                    if let Some(e) = factor.exponent() {
                        b.add(Factor::from_expr(e));
                    } else {
                        b.add(1);
                    }
                }
                f => {
                    *f = Factor::Pow(
                        Expr::from(f.clone()),
                        if let Some(mut e) = factor.exponent() {
                            e.add(1);
                            e
                        } else {
                            Expr::from(2)
                        },
                    );
                }
            })
            .or_insert(factor);
    }

    pub fn div_by(&mut self, factor: Factor) -> Option<Factor> {
        let hash = factor.inner_hash();
        if let Some(f) = self.factors.get_mut(&hash) {
            match f {
                Factor::Pow(_, b) => {
                    if let Some(e) = factor.exponent() {
                        b.sub(Factor::from_expr(e));
                    } else {
                        b.sub(1);
                    }
                }
                f => {
                    if let Some(mut e) = factor.exponent() {
                        e.sub(1);
                        *f = Factor::Pow(Expr::from(f.clone()), e);
                    } else {
                        self.factors.remove(&hash);
                    }
                }
            }
            None
        } else {
            Some(factor)
        }
    }

    pub fn flatten(&self, mut consts: impl FnMut(Fraction)) -> Vec<Factor> {
        self.factors
            .values()
            .filter_map(|f| {
                let (c, f) = f.simplify_inner();
                consts(c);
                f
            })
            .collect()
    }
}

impl Term {
    pub fn as_factor(&self) -> (Fraction, Option<Factor>) {
        if self.denominators.is_empty() {
            if let Some((frac, factor)) = match self.factors.len() {
                0 => Some((self.consts, None)),
                1 => Some((self.consts, Some(self.factors[0].clone()))),
                _ => None,
            } {
                return match factor {
                    Some(Factor::Group(expr)) => {
                        let (f, e) = expr.as_factor();
                        (f * frac, e)
                    }
                    _ => (frac, factor),
                };
            }
        }
        (Fraction::one(), Some(Factor::group(self.clone())))
    }

    pub fn mul(&mut self, other: Term) {
        self.consts *= other.consts;
        self.factors.extend(other.factors.into_iter());
        self.denominators.extend(other.denominators.into_iter());
    }

    pub fn mul_const(&mut self, consts: Fraction) {
        self.consts *= consts;
    }

    pub fn hash_without_const(&self) -> u64 {
        let factors = self
            .factors
            .iter()
            .map(|f| f.unordered_hash())
            .reduce(|a, b| a ^ b)
            .unwrap_or(0);

        let denominators = self
            .denominators
            .iter()
            .map(|f| f.unordered_hash())
            .reduce(|a, b| a ^ b)
            .unwrap_or(0);

        factors ^ denominators.rotate_right(23)
    }

    pub fn identifier(&self) -> Option<Identifier> {
        if self.denominators.is_empty() && self.factors.len() == 1 && let Factor::Identifier(id) = &self.factors[0] {
            Some(id.clone())
        } else {
            None
        }
    }

    pub fn is(&self, id: impl Into<Identifier>) -> bool {
        self.identifier() == Some(id.into())
    }

    pub(super) fn simplify_inner(&self) -> Option<Self> {
        #[cfg(feature = "tracy")]
        profiling::scope!("Term::simplify_inner");
        if self.consts.is_zero() {
            return None;
        }
        let mut consts = self.consts;
        let mut factors = Factors::with_capacity(self.factors.len());
        let mut denominators = Factors::with_capacity(self.denominators.len());
        {
            #[cfg(feature = "tracy")]
            profiling::scope!("Simplifiy factors");
            for factor in &self.factors {
                let (c, simplified) = factor.simplify_inner();
                if c.is_zero() {
                    return None;
                }
                consts *= c;

                if let Some(f) = simplified {
                    factors.mul_by(f)
                }
            }
        }
        {
            #[cfg(feature = "tracy")]
            profiling::scope!("Simplifiy denominators");
            for denominator in &self.denominators {
                let (c, simplified) = denominator.simplify_inner();
                if c.is_zero() {
                    panic!("Division by zero");
                }
                consts /= c;

                if let Some(f) = simplified {
                    if let Some(c) = f.inner_const() && (consts % c).is_zero() {
                        consts /= c;
                        if let Some(f) = denominators.div_by(f) {
                            factors.mul_by(f);
                        }
                    } else if let Some(f) = factors.div_by(f) {
                        denominators.mul_by(f);
                    }
                }
            }
        }

        Some(Self {
            factors: factors.flatten(|c| consts *= c),
            denominators: denominators.flatten(|c| consts /= c),
            consts,
        })
    }
}

pub trait ToTerm {
    fn term(self) -> Term;
}

impl ToTerm for Term {
    fn term(self) -> Term {
        self
    }
}

fn term_from_factor(factor: Factor) -> Term {
    match factor {
        Factor::Number(f) => {
            if let Some(fraction) = Fraction::from_f64(f) {
                Term {
                    consts: fraction,
                    factors: vec![],
                    denominators: vec![],
                }
            } else {
                Term {
                    consts: if f < 0.0 {
                        Fraction::neg_one()
                    } else {
                        Fraction::one()
                    },
                    factors: vec![Factor::Number(f.abs())],
                    denominators: vec![],
                }
            }
        }
        factor => Term {
            consts: Fraction::one(),
            factors: vec![factor],
            ..Default::default()
        },
    }
}

impl ToTerm for (Fraction, Option<Factor>) {
    fn term(self) -> Term {
        let mut term = self
            .1
            .map(|t| term_from_factor(t))
            .unwrap_or_else(|| 1.term());
        term.consts *= self.0;
        term
    }
}

impl ToTerm for u64 {
    fn term(self) -> Term {
        Term {
            consts: Fraction::whole(self),
            ..Default::default()
        }
    }
}

impl ToTerm for Fraction {
    fn term(self) -> Term {
        Term {
            consts: self,
            ..Default::default()
        }
    }
}

impl<T> ToTerm for T
where
    Factor: From<T>,
{
    fn term(self) -> Term {
        term_from_factor(Factor::from(self))
    }
}

impl<T> ToTerm for (u64, T)
where
    Factor: From<T>,
{
    fn term(self) -> Term {
        term_from_factor(Factor::from(self.1))
    }
}

impl<T> ToTerm for (Fraction, T)
where
    Factor: From<T>,
{
    fn term(self) -> Term {
        let mut term = term_from_factor(Factor::from(self.1));
        term.consts *= self.0;
        term
    }
}

impl ToTerm for Vec<Factor> {
    fn term(self) -> Term {
        Term {
            factors: self,
            consts: Fraction::one(),
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
            consts: Fraction::one(),
            ..Default::default()
        }
    }
}

impl ToTerm for (Fraction, Vec<Factor>) {
    fn term(self) -> Term {
        Term {
            consts: self.0,
            factors: self.1,
            ..Default::default()
        }
    }
}

impl ToTerm for (Fraction, &Vec<Factor>) {
    fn term(self) -> Term {
        Term {
            consts: self.0,
            factors: self.1.clone(),
            ..Default::default()
        }
    }
}

impl ToTerm for (Fraction, Vec<Factor>, Vec<Factor>) {
    fn term(self) -> Term {
        Term {
            consts: self.0,
            factors: self.1,
            denominators: self.2,
        }
    }
}

impl ToTerm for (Fraction, &Vec<Factor>, &Vec<Factor>) {
    fn term(self) -> Term {
        Term {
            consts: self.0,
            factors: self.1.clone(),
            denominators: self.2.clone(),
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
            .unwrap_or(1.0)
            / self
                .denominators
                .iter()
                .map(|f| f.evaluate(defines))
                .reduce(|acc, x| acc * x)
                .unwrap_or(1.0))
            * f64::from(self.consts)
    }

    fn derivative(&self, by: Identifier) -> Expr {
        #[cfg(feature = "tracy")]
        profiling::scope!("Term::derivative");
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
            terms.push((Fraction::neg_one(), factors).term());
        }

        let (consts, factor) = Factor::from_expr(terms);
        let factors = factor.map(|f| vec![f]).unwrap_or_default();
        Expr::from((
            self.consts * consts,
            factors,
            self.denominators
                .iter()
                .map(|f| Factor::pow(f.clone(), 2.0))
                .collect(),
        ))
    }

    fn as_num(&self) -> Option<Fraction> {
        if self.denominators.is_empty() {
            match self.factors.len() {
                0 => Some(self.consts),
                1 => self.factors[0].as_num().map(|c| c * self.consts),
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

    fn sign(&self) -> Option<Sign> {
        self.factors
            .iter()
            .try_fold(Sign::Pos, |a, b| b.sign().map(|b| a * b)).map(|a| a * self.consts.sign())
    }
}
