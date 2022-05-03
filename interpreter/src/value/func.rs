use std::fmt::Display;

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

    pub fn evaluate(&self, num: f64) -> f64 {
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
