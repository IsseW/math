use std::{
    cmp, fmt, mem,
    ops::{Add, AddAssign, Div, DivAssign, Mul, MulAssign, Neg, Rem, RemAssign, Sub, SubAssign},
};

use num::integer::Roots;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Sign {
    Pos,
    Neg,
}

impl Sign {
    pub fn pow(self, n: u64) -> Sign {
        match self {
            Sign::Pos => Sign::Pos,
            Sign::Neg => {
                if n % 2 == 0 {
                    Sign::Pos
                } else {
                    Sign::Neg
                }
            }
        }
    }
}

impl Neg for Sign {
    type Output = Sign;

    fn neg(self) -> Sign {
        match self {
            Sign::Pos => Sign::Neg,
            Sign::Neg => Sign::Pos,
        }
    }
}

impl Mul<Sign> for Sign {
    type Output = Sign;

    fn mul(self, rhs: Sign) -> Self::Output {
        if self == rhs {
            Sign::Pos
        } else {
            Sign::Neg
        }
    }
}

pub trait Signed {
    fn sign(&self) -> Sign;
    fn mul_sign(self, sign: Sign) -> Self;
}

impl<T: num::Signed> Signed for T {
    fn sign(&self) -> Sign {
        if self.is_negative() {
            Sign::Neg
        } else {
            Sign::Pos
        }
    }

    fn mul_sign(self, sign: Sign) -> Self {
        match sign {
            Sign::Pos => self,
            Sign::Neg => -self,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Fraction {
    sign: Sign,
    n: u64,
    d: u64,
}

impl Default for Fraction {
    fn default() -> Self {
        Self {
            sign: Sign::Pos,
            n: 0,
            d: 1,
        }
    }
}

const EULER: bool = false;

fn gcd(mut u: u64, mut v: u64) -> u64 {
    if EULER {
        while v != 0 {
            mem::swap(&mut u, &mut v);
            v %= u;
        }
        u
    } else {
        // STEIN
        if u == 0 {
            return v;
        }
        if v == 0 {
            return u;
        }
        let i = u.trailing_zeros();
        let j = v.trailing_zeros();
        u >>= i;
        v >>= j;
        let k = i.min(j);

        loop {
            if u > v {
                mem::swap(&mut u, &mut v);
            }

            v -= u;

            if v == 0 {
                break u << k;
            }

            v >>= v.trailing_zeros();
        }
    }
}

fn lcm(a: u64, b: u64) -> u64 {
    a / gcd(a, b) * b
}

impl Fraction {
    fn normalize(&mut self) {
        if self.n == 0 {
            self.sign = Sign::Pos;
            self.d = 1;
        } else {
            let gcd = gcd(self.n, self.d);
            self.n /= gcd;
            self.d /= gcd;
        }
    }
    fn normalized(mut self) -> Self {
        self.normalize();
        self
    }

    pub fn inverse(&self) -> Self {
        Self {
            sign: self.sign,
            n: self.d,
            d: self.n,
        }
    }

    pub fn is_zero(&self) -> bool {
        self.n == 0
    }
    pub fn as_whole(self) -> Option<(Sign, u64)> {
        if self.d == 1 {
            Some((self.sign, self.n))
        } else {
            None
        }
    }

    pub fn zero() -> Self {
        Self {
            sign: Sign::Pos,
            n: 0,
            d: 1,
        }
    }
    pub fn one() -> Self {
        Self {
            sign: Sign::Pos,
            n: 1,
            d: 1,
        }
    }
    pub fn neg_one() -> Self {
        Self {
            sign: Sign::Neg,
            n: 1,
            d: 1,
        }
    }
    pub fn whole(n: u64) -> Self {
        Self {
            sign: Sign::Pos,
            n,
            d: 1,
        }
    }
    pub fn new(sign: Sign, n: u64, d: u64) -> Self {
        if d == 0 {
            panic!("Division by zero");
        }
        Fraction { sign, n, d }.normalized()
    }
    pub fn from_f64(mut x: f64) -> Option<Self> {
        if x.is_nan() || x.is_infinite() {
            return None;
        }
        let sign = x.sign();
        x = x.abs();

        const MAX_DENOM: u64 = 1_000_000_000;
        let mut m = [[1, 0], [0, 1]];
        let start_x = x;
        let mut ai = x as u64;
        while m[1][0] * ai + m[1][1] <= MAX_DENOM {
            let r = m[0][1];
            m[0][1] = m[0][0];
            m[0][0] = m[0][0] * ai + r;

            let r = m[1][1];
            m[1][1] = m[1][0];
            m[1][0] = m[1][0] * ai + r;

            if x == ai as f64 {
                break;
            }
            x = 1.0 / (x - ai as f64);
            if x > u64::MAX as f64 {
                return None;
            }
            ai = x as u64;
        }
        let t = Self::new(sign, m[0][0], m[1][0]);
        if (f64::from(t) - start_x).abs() < 1e-10 {
            return Some(t);
        }

        ai = (MAX_DENOM - m[1][1]) / m[1][0];
        m[0][0] = m[0][0] * ai + m[0][1];
        m[1][0] = m[1][0] * ai + m[1][1];
        let t = Self::new(sign, m[0][0], m[1][0]);
        if (f64::from(t) - start_x).abs() < 1e-10 {
            return Some(t);
        } else {
            None
        }
    }

    pub fn pow(self, exp: Fraction) -> Option<Fraction> {
        #[cfg(feature = "tracy")]
        profiling::scope!("Fraction::pow");
        // 0 ^ (a / b)
        if self.is_zero() {
            return Some(Self::zero());
        }
        // 1 ^ (a / b)
        if self.is_pos() && self.n == 1 && self.d == 1 {
            return Some(Self::one());
        }
        // (a / b) ^ 0
        if exp.is_zero() {
            return Some(Fraction::one());
        }
        // (a / b) ^ n
        if let Some((s, v)) = exp.as_whole() {
            let n = self.n.checked_pow(v.try_into().ok()?)?;
            let d = self.d.checked_pow(v.try_into().ok()?)?;
            return Some(match s {
                Sign::Pos => Fraction {
                    sign: self.sign.pow(v),
                    n,
                    d,
                },
                Sign::Neg => Fraction {
                    sign: self.sign.pow(v),
                    n: d,
                    d: n,
                },
            });
        }
        // +(a / b) ^ (a / b)
        if self.is_pos() {
            fn root(n: u64, k: u64) -> Option<u64> {
                let r = n.nth_root(k as u32);
                if r.pow(k as u32) == n {
                    Some(r)
                } else {
                    None
                }
            }
            let n = root(self.n, exp.d)?.checked_pow(exp.n.try_into().ok()?)?;
            let d = root(self.d, exp.d)?.checked_pow(exp.n.try_into().ok()?)?;
            return Some(match exp.sign {
                Sign::Pos => Fraction {
                    sign: Sign::Pos,
                    n,
                    d,
                },
                Sign::Neg => Fraction {
                    sign: Sign::Pos,
                    n: d,
                    d: n,
                },
            });
        }

        None
    }

    pub fn n(&self) -> u64 {
        self.n
    }
    pub fn d(&self) -> u64 {
        self.d
    }
    pub fn is_neg(&self) -> bool {
        self.sign == Sign::Neg
    }
    pub fn is_pos(&self) -> bool {
        self.sign == Sign::Pos
    }
    pub fn abs(self) -> Self {
        Self {
            sign: Sign::Pos,
            n: self.n,
            d: self.d,
        }
    }
    pub fn signum(self) -> Self {
        Self {
            sign: self.sign,
            n: 1,
            d: 1,
        }
    }

    pub fn sign(&self) -> Sign {
        self.sign
    }
}

impl PartialOrd for Fraction {
    fn partial_cmp(&self, other: &Self) -> Option<cmp::Ordering> {
        let lcm = lcm(self.d, other.d);
        let a = lcm / self.d * self.n;
        let b = lcm / other.d * other.n;
        Some(match (self.is_neg(), other.is_neg()) {
            (false, false) => a.cmp(&b),
            (true, false) => cmp::Ordering::Less,
            (false, true) => cmp::Ordering::Greater,
            (true, true) => a.cmp(&b).reverse(),
        })
    }
}

impl Ord for Fraction {
    fn cmp(&self, other: &Self) -> cmp::Ordering {
        self.partial_cmp(other).unwrap()
    }
}

impl fmt::Display for Fraction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.is_neg() {
            write!(f, "-")?;
        }
        if self.d == 1 {
            write!(f, "{}", self.n)
        } else {
            write!(f, "{}/{}", self.n, self.d)
        }
    }
}

impl From<Fraction> for f64 {
    fn from(fraction: Fraction) -> Self {
        let f = fraction.n as f64 / fraction.d as f64;
        f.mul_sign(fraction.sign)
    }
}

impl Neg for Fraction {
    type Output = Self;
    fn neg(self) -> Self {
        Self {
            sign: -self.sign,
            n: self.n,
            d: self.d,
        }
    }
}
impl Add<u64> for Fraction {
    type Output = Self;
    fn add(self, rhs: u64) -> Self {
        if self.is_neg() {
            if self.n <= rhs * self.d {
                Self {
                    sign: Sign::Pos,
                    n: rhs * self.d - self.n,
                    d: self.d,
                }
            } else {
                Self {
                    sign: Sign::Neg,
                    n: self.n - rhs * self.d,
                    d: self.d,
                }
            }
        } else {
            Self {
                sign: Sign::Pos,
                n: self.n + rhs * self.d,
                d: self.d,
            }
        }
        .normalized()
    }
}
impl Add<Fraction> for u64 {
    type Output = Fraction;

    fn add(self, rhs: Fraction) -> Self::Output {
        rhs + self
    }
}
impl Sub<u64> for Fraction {
    type Output = Self;
    fn sub(self, rhs: u64) -> Self {
        if self.is_neg() {
            Self {
                sign: Sign::Neg,
                n: self.n + rhs * self.d,
                d: self.d,
            }
        } else {
            if self.n <= rhs * self.d {
                Self {
                    sign: Sign::Neg,
                    n: rhs * self.d - self.n,
                    d: self.d,
                }
            } else {
                Self {
                    sign: Sign::Pos,
                    n: self.n - rhs * self.d,
                    d: self.d,
                }
            }
        }
        .normalized()
    }
}
impl Sub<Fraction> for u64 {
    type Output = Fraction;

    fn sub(self, rhs: Fraction) -> Self::Output {
        (-rhs) + self
    }
}
impl Mul<u64> for Fraction {
    type Output = Self;
    fn mul(self, rhs: u64) -> Self {
        let gcd = gcd(self.d, rhs);
        Self {
            sign: self.sign,
            n: self.n * rhs / gcd,
            d: self.d / gcd,
        }
    }
}
impl Mul<Fraction> for u64 {
    type Output = Fraction;

    fn mul(self, rhs: Fraction) -> Self::Output {
        rhs * self
    }
}
impl Div<u64> for Fraction {
    type Output = Self;
    fn div(self, rhs: u64) -> Self {
        let gcd = gcd(self.n, rhs);
        Self {
            sign: self.sign,
            n: self.n / gcd,
            d: self.d * rhs / gcd,
        }
    }
}
impl Div<Fraction> for u64 {
    type Output = Fraction;
    fn div(self, rhs: Fraction) -> Fraction {
        let gcd = gcd(rhs.d, self);
        Fraction {
            sign: rhs.sign,
            n: rhs.n * self / gcd,
            d: rhs.d / gcd,
        }
    }
}
impl Rem<u64> for Fraction {
    type Output = Self;
    fn rem(self, rhs: u64) -> Self {
        Self {
            sign: self.sign,
            n: self.n % self.d,
            d: self.d,
        }
    }
}
impl Rem<Fraction> for u64 {
    type Output = Fraction;
    fn rem(self, rhs: Fraction) -> Fraction {
        let n = (self * rhs.d) % rhs.n;
        Fraction {
            sign: rhs.sign,
            n,
            d: rhs.d,
        }
        .normalized()
    }
}

impl Add<Fraction> for Fraction {
    type Output = Self;
    fn add(self, rhs: Self) -> Self {
        let d = lcm(self.d, rhs.d);
        let a = self.n * d / self.d;
        let b = rhs.n * d / rhs.d;
        match (self.sign, rhs.sign) {
            (s @ Sign::Pos, Sign::Pos) | (s @ Sign::Neg, Sign::Neg) => Self {
                sign: s,
                n: b + a,
                d,
            },
            (s @ Sign::Neg, Sign::Pos) | (s @ Sign::Pos, Sign::Neg) => {
                if a >= b {
                    Self {
                        sign: s,
                        n: a - b,
                        d,
                    }
                } else {
                    Self {
                        sign: -s,
                        n: b - a,
                        d,
                    }
                }
            }
        }
        .normalized()
    }
}
impl Sub<Fraction> for Fraction {
    type Output = Self;
    fn sub(self, rhs: Self) -> Self {
        self.add(-rhs)
    }
}
impl Mul<Fraction> for Fraction {
    type Output = Self;
    fn mul(self, rhs: Self) -> Self {
        let gcd = gcd(self.n, rhs.d) * gcd(self.d, rhs.n);
        Self {
            sign: self.sign * rhs.sign,
            n: self.n * rhs.n / gcd,
            d: self.d * rhs.d / gcd,
        }
    }
}
impl Div<Fraction> for Fraction {
    type Output = Self;
    fn div(self, rhs: Self) -> Self {
        let gcd = gcd(self.n, rhs.n) * gcd(self.d, rhs.d);
        Self {
            sign: self.sign * rhs.sign,
            n: self.n * rhs.d / gcd,
            d: self.d * rhs.n / gcd,
        }
    }
}
impl Rem<Fraction> for Fraction {
    type Output = Self;
    fn rem(self, rhs: Self) -> Self {
        let d = lcm(self.d, rhs.d);
        let a = self.n * d / self.d;
        let b = rhs.n * d / rhs.d;
        Self {
            sign: self.sign * rhs.sign,
            n: a % b,
            d,
        }
        .normalized()
    }
}

impl AddAssign<u64> for Fraction {
    fn add_assign(&mut self, rhs: u64) {
        *self = self.add(rhs);
    }
}
impl SubAssign<u64> for Fraction {
    fn sub_assign(&mut self, rhs: u64) {
        *self = self.sub(rhs);
    }
}
impl MulAssign<u64> for Fraction {
    fn mul_assign(&mut self, rhs: u64) {
        *self = self.mul(rhs);
    }
}
impl DivAssign<u64> for Fraction {
    fn div_assign(&mut self, rhs: u64) {
        *self = self.div(rhs);
    }
}
impl RemAssign<u64> for Fraction {
    fn rem_assign(&mut self, rhs: u64) {
        *self = self.rem(rhs);
    }
}

impl AddAssign<Fraction> for Fraction {
    fn add_assign(&mut self, rhs: Self) {
        *self = self.add(rhs);
    }
}
impl SubAssign<Fraction> for Fraction {
    fn sub_assign(&mut self, rhs: Self) {
        *self = self.sub(rhs);
    }
}
impl MulAssign<Fraction> for Fraction {
    fn mul_assign(&mut self, rhs: Self) {
        *self = self.mul(rhs);
    }
}
impl DivAssign<Fraction> for Fraction {
    fn div_assign(&mut self, rhs: Self) {
        *self = self.div(rhs);
    }
}
impl RemAssign<Fraction> for Fraction {
    fn rem_assign(&mut self, rhs: Fraction) {
        *self = self.rem(rhs);
    }
}

#[macro_export]
macro_rules! frac {
    (- $a:literal / $b:literal) => {
        Fraction::new(crate::Sign::Neg, $a, $b)
    };
    ($a:literal / $b:literal) => {
        Fraction::new(crate::Sign::Pos, $a, $b)
    };
    (- $c:literal) => {
        Fraction::new(crate::Sign::Neg, $c, 1)
    };
    ($c:literal) => {
        Fraction::whole($c)
    };
    () => {
        Fraction::zero()
    };
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_rem() {
        assert_eq!(frac!(2) % frac!(2), frac!());
        assert_eq!(frac!(4) % frac!(2), frac!());

        assert_eq!(frac!(2 / 3) % frac!(1 / 2), frac!(1 / 6));
        assert_eq!(frac!(2 / 3) % frac!(1 / 5), frac!(1 / 15));
    }
}
