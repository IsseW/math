use std::{f64::consts, fmt::Display};

use num::FromPrimitive;
use num_derive::FromPrimitive;

#[derive(PartialEq, Eq, Hash, Clone, Copy, FromPrimitive, Debug)]
#[repr(u32)]
pub enum AlphaKind {
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
    Sigma = 44,
    Tau = 45,
    Upsilon = 46,
    Phi = 47,
    Chi = 48,
    Psi = 49,
    Omega = 50,
}

#[derive(PartialEq, Eq, Hash, Clone, Copy, Debug)]
pub struct Alpha {
    pub(crate) kind: AlphaKind,
    pub(crate) upper: bool,
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
                kind: FromPrimitive::from_u32(AlphaKind::Alpha as u32 + c as u32 - 'α' as u32)?,
                upper: false,
            }),
            'Α'..='Ω' => Some(Alpha {
                kind: FromPrimitive::from_u32(AlphaKind::Alpha as u32 + c as u32 - 'a' as u32)?,
                upper: true,
            }),
            _ => None,
        }
    }

    pub fn from_str(s: &str) -> Option<Self> {
        let mut c = s.chars();
        if let Some(char) = c.next() && c.next().is_none() {
            Self::from_char(char)
        } else {
            Some(match s {
                "alpha" => Alpha {
                    kind: AlphaKind::Alpha,
                    upper: false,
                },
                "Alpha" => Alpha {
                    kind: AlphaKind::Alpha,
                    upper: true,
                },
                "beta" => Alpha {
                    kind: AlphaKind::Beta,
                    upper: false,
                },
                "Beta" => Alpha {
                    kind: AlphaKind::Beta,
                    upper: true,
                },
                "gamma" => Alpha {
                    kind: AlphaKind::Gamma,
                    upper: false,
                },
                "Gamma" => Alpha {
                    kind: AlphaKind::Gamma,
                    upper: true,
                },
                "delta" => Alpha {
                    kind: AlphaKind::Delta,
                    upper: false,
                },
                "Delta" => Alpha {
                    kind: AlphaKind::Delta,
                    upper: true,
                },
                "epsilon" => Alpha {
                    kind: AlphaKind::Epsilon,
                    upper: false,
                },
                "Epsilon" => Alpha {
                    kind: AlphaKind::Epsilon,
                    upper: true,
                },
                "zeta" => Alpha {
                    kind: AlphaKind::Zeta,
                    upper: false,
                },
                "Zeta" => Alpha {
                    kind: AlphaKind::Zeta,
                    upper: true,
                },
                "eta" => Alpha {
                    kind: AlphaKind::Eta,
                    upper: false,
                },
                "Eta" => Alpha {
                    kind: AlphaKind::Eta,
                    upper: true,
                },
                "theta" => Alpha {
                    kind: AlphaKind::Theta,
                    upper: false,
                },
                "Theta" => Alpha {
                    kind: AlphaKind::Theta,
                    upper: true,
                },
                "iota" => Alpha {
                    kind: AlphaKind::Iota,
                    upper: false,
                },
                "Iota" => Alpha {
                    kind: AlphaKind::Iota,
                    upper: true,
                },
                "kappa" => Alpha {
                    kind: AlphaKind::Kappa,
                    upper: false,
                },
                "Kappa" => Alpha {
                    kind: AlphaKind::Kappa,
                    upper: true,
                },
                "lambda" => Alpha {
                    kind: AlphaKind::Lambda,
                    upper: false,
                },
                "Lambda" => Alpha {
                    kind: AlphaKind::Lambda,
                    upper: true,
                },
                "mu" => Alpha {
                    kind: AlphaKind::Mu,
                    upper: false,
                },
                "Mu" => Alpha {
                    kind: AlphaKind::Mu,
                    upper: true,
                },
                "nu" => Alpha {
                    kind: AlphaKind::Nu,
                    upper: false,
                },
                "Nu" => Alpha {
                    kind: AlphaKind::Nu,
                    upper: true,
                },
                "xi" => Alpha {
                    kind: AlphaKind::Xi,
                    upper: false,
                },
                "Xi" => Alpha {
                    kind: AlphaKind::Xi,
                    upper: true,
                },
                "omicron" => Alpha {
                    kind: AlphaKind::Omicron,
                    upper: false,
                },
                "Omicron" => Alpha {
                    kind: AlphaKind::Omicron,
                    upper: true,
                },
                "pi" => Alpha {
                    kind: AlphaKind::Pi,
                    upper: false,
                },
                "Pi" => Alpha {
                    kind: AlphaKind::Pi,
                    upper: true,
                },
                "rho" => Alpha {
                    kind: AlphaKind::Rho,
                    upper: false,
                },
                "Rho" => Alpha {
                    kind: AlphaKind::Rho,
                    upper: true,
                },
                "sigma" => Alpha {
                    kind: AlphaKind::Sigma,
                    upper: false,
                },
                "Sigma" => Alpha {
                    kind: AlphaKind::Sigma,
                    upper: true,
                },
                "tau" => Alpha {
                    kind: AlphaKind::Tau,
                    upper: false,
                },
                "Tau" => Alpha {
                    kind: AlphaKind::Tau,
                    upper: true,
                },
                "upsilon" => Alpha {
                    kind: AlphaKind::Upsilon,
                    upper: false,
                },
                "Upsilon" => Alpha {
                    kind: AlphaKind::Upsilon,
                    upper: true,
                },
                "phi" => Alpha {
                    kind: AlphaKind::Phi,
                    upper: false,
                },
                "Phi" => Alpha {
                    kind: AlphaKind::Phi,
                    upper: true,
                },
                "chi" => Alpha {
                    kind: AlphaKind::Chi,
                    upper: false,
                },
                "Chi" => Alpha {
                    kind: AlphaKind::Chi,
                    upper: true,
                },
                "psi" => Alpha {
                    kind: AlphaKind::Psi,
                    upper: false,
                },
                "Psi" => Alpha {
                    kind: AlphaKind::Psi,
                    upper: true,
                },
                "omega" => Alpha {
                    kind: AlphaKind::Omega,
                    upper: false,
                },
                "Omega" => Alpha {
                    kind: AlphaKind::Omega,
                    upper: true,
                },
                _ => return None,
            })
        }
    }

    pub fn simple_hash(&self) -> u64 {
        ((self.upper as u64) << 57) ^ self.kind as u64
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
    pub(crate) id: Alpha,
    pub(crate) index: Option<AlphaNumerical>,
}

impl From<char> for Identifier {
    fn from(c: char) -> Self {
        Identifier {
            id: Alpha::from_char(c).unwrap(),
            index: None,
        }
    }
}

impl From<(char, char)> for Identifier {
    fn from((c1, c2): (char, char)) -> Self {
        Identifier {
            id: Alpha::from_char(c1).unwrap(),
            index: Some(AlphaNumerical::Alpha(Alpha::from_char(c2).unwrap())),
        }
    }
}

impl From<(char, u64)> for Identifier {
    fn from((c, n): (char, u64)) -> Self {
        Identifier {
            id: Alpha::from_char(c).unwrap(),
            index: Some(AlphaNumerical::Numerical(n)),
        }
    }
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

    pub fn from_str(s: &str) -> Option<Self> {
        let mut chars = s.chars();
        let id = Alpha::from_char(chars.next()?)?;
        if let Some(next) = chars.next() {
            if next == '_' {
                let n = chars.next()?;
                if let Some(alpha) = Alpha::from_char(n) {
                    return Some(Identifier::new(id, Some(AlphaNumerical::Alpha(alpha))));
                } else {
                    let mut num = n.to_digit(10)? as u64;
                    while let Some(n) = chars.next() {
                        let d = n.to_digit(10)? as u64;
                        if num > u64::MAX / 10 - d {
                            return None;
                        }
                        num = num * 10 + d;
                    }
                    return Some(Identifier::new(id, Some(AlphaNumerical::Numerical(num))));
                }
            } else {
                None
            }
        } else {
            Some(Identifier { id, index: None })
        }
    }

    pub fn simple_hash(&self) -> u64 {
        let hash = self.id.simple_hash();
        match self.index {
            Some(AlphaNumerical::Alpha(alpha)) => hash ^ alpha.simple_hash().rotate_right(13),
            Some(AlphaNumerical::Numerical(n)) => hash ^ n.rotate_left(13),
            None => !hash,
        }
    }

    pub fn associated_value(&self) -> Option<f64> {
        match (self.id.kind, self.id.upper, self.index) {
            (AlphaKind::Pi, false, None) => Some(consts::PI),
            (AlphaKind::E, false, None) => Some(consts::E),
            (AlphaKind::Tau, false, None) => Some(consts::TAU),
            _ => None,
        }
    }
}
