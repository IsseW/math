use std::collections::HashMap;

use crate::{Expr, Identifier};

use super::ToTerm;

#[derive(Clone, Debug)]
pub struct Define {
    pub args: Vec<Identifier>,
    pub expr: Expr,
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
