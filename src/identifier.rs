use std::{f64::consts, fmt::Display};

use num::FromPrimitive;
use num_derive::FromPrimitive;

#[derive(PartialEq, Eq, Hash, Clone, Copy, FromPrimitive, Debug)]
#[repr(u32)]
enum AlphaKind {
    A = 0,
    B = 1,
    C = 2,
    D = 3,
    E = 4,
    F = 5,
    G = 6,
    H = 7,
    I = 8,
    J = 9,
    K = 10,
    L = 11,
    M = 12,
    N = 13,
    O = 14,
    P = 15,
    Q = 16,
    R = 17,
    S = 18,
    T = 19,
    U = 20,
    V = 21,
    W = 22,
    X = 23,
    Y = 24,
    Z = 25,
    Alpha = 26,
    Beta = 27,
    Gamma = 28,
    Delta = 29,
    Epsilon = 30,
    Zeta = 31,
    Eta = 32,
    Theta = 33,
    Iota = 34,
    Kappa = 35,
    Lambda = 36,
    Mu = 37,
    Nu = 38,
    Xi = 39,
    Omicron = 40,
    Pi = 41,
    Rho = 42,
    Sigma = 43,
    Tau = 44,
    Upsilon = 45,
    Phi = 46,
    Chi = 47,
    Psi = 48,
    Omega = 49,
}

#[derive(PartialEq, Eq, Hash, Clone, Copy, Debug)]
pub struct Alpha {
    kind: AlphaKind,
    upper: bool,
}

impl Display for Alpha {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.kind as u32 {
            0..=25 => {
                if self.upper {
                    write!(f, "{}", (self.kind as u8 + b'A') as char)
                } else {
                    write!(f, "{}", (self.kind as u8 + b'a') as char)
                }
            }
            _ => {
                let kind = self.kind as u32 - AlphaKind::Alpha as u32;
                if self.upper {
                    write!(f, "{}", char::from_u32(kind as u32 + 'Α' as u32).unwrap())
                } else {
                    write!(f, "{}", char::from_u32(kind as u32 + 'α' as u32).unwrap())
                }
            }
        }
    }
}

impl Alpha {
    pub fn associated_value(&self) -> Option<f64> {
        match (self.kind, self.upper) {
            (AlphaKind::Pi, false) => Some(consts::PI),
            (AlphaKind::E, false) => Some(consts::E),
            (AlphaKind::Tau, false) => Some(consts::TAU),
            // Golden ratio
            (AlphaKind::Phi, false) => Some(1.61803398875),
            _ => None,
        }
    }

    pub fn from_char(c: char) -> Option<Self> {
        match c {
            'a'..='z' => Some(Alpha {
                kind: FromPrimitive::from_u32(c as u32 - 'a' as u32).unwrap(),
                upper: false,
            }),
            'A'..='Z' => Some(Alpha {
                kind: FromPrimitive::from_u32(c as u32 - 'A' as u32).unwrap(),
                upper: true,
            }),
            'α'..='ω' => Some(Alpha {
                kind: FromPrimitive::from_u32(AlphaKind::Alpha as u32 + c as u32 - 'α' as u32)
                    .unwrap(),
                upper: false,
            }),
            'Α'..='Ω' => Some(Alpha {
                kind: FromPrimitive::from_u32(AlphaKind::Alpha as u32 + c as u32 - 'a' as u32)
                    .unwrap(),
                upper: true,
            }),
            _ => None,
        }
    }
}

#[derive(PartialEq, Eq, Hash, Clone, Copy, Debug)]
pub enum AlphaNumerical {
    Alpha(Alpha),
    Numerical(u64),
}

impl Display for AlphaNumerical {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AlphaNumerical::Alpha(alpha) => write!(f, "{}", alpha),
            AlphaNumerical::Numerical(n) => write!(f, "{}", n),
        }
    }
}

#[derive(PartialEq, Eq, Hash, Clone, Copy, Debug)]
pub struct Identifier {
    id: Alpha,
    index: Option<AlphaNumerical>,
}

impl Display for Identifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.id)?;
        if let Some(index) = self.index {
            write!(f, "_{}", index)?;
        }
        Ok(())
    }
}

impl Identifier {
    pub fn new(id: Alpha, index: Option<AlphaNumerical>) -> Identifier {
        Identifier { id, index }
    }
}
