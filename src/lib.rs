#![feature(int_log, let_chains)]

use std::{
    collections::HashMap,
    ops::{Deref, RangeInclusive},
    str::Chars,
};

use identifier::{Alpha, AlphaNumerical, Identifier};
use value::{EqualityOperator, Expr, Fragment, Function, Operator};

mod identifier;
mod value;

struct Interpreter {
    pub values: HashMap<Identifier, bool>,
}
#[derive(Clone, Copy, PartialEq, Debug)]
enum TokenKind {
    Number(f64),
    Identifier(Identifier),
    Operator(Operator),
    Eq(EqualityOperator),
    Ctrl(char),
    Function(Function),
}

struct Token {
    kind: TokenKind,
    span: RangeInclusive<usize>,
}

impl Deref for Token {
    type Target = TokenKind;

    fn deref(&self) -> &Self::Target {
        &self.kind
    }
}

impl From<Identifier> for TokenKind {
    fn from(id: Identifier) -> Self {
        TokenKind::Identifier(id)
    }
}

impl From<f64> for TokenKind {
    fn from(num: f64) -> Self {
        TokenKind::Number(num)
    }
}

impl From<Function> for TokenKind {
    fn from(func: Function) -> Self {
        TokenKind::Function(func)
    }
}

impl From<Operator> for TokenKind {
    fn from(op: Operator) -> Self {
        TokenKind::Operator(op)
    }
}

impl From<EqualityOperator> for TokenKind {
    fn from(op: EqualityOperator) -> Self {
        TokenKind::Eq(op)
    }
}

struct TextParser<'a> {
    pos: usize,
    start_pos: usize,
    current: Option<char>,
    iter: Chars<'a>,
    tokens: Vec<Token>,
}

impl<'a> TextParser<'a> {
    fn new(s: &'a str) -> Self {
        let mut iter = s.chars();
        let next = iter.next();
        TextParser {
            pos: 0,
            start_pos: 0,
            current: next,
            iter,
            tokens: Vec::new(),
        }
    }

    fn next_non_whitespace(&mut self) -> Option<char> {
        self.current = self.iter.find(|c| {
            self.pos += 1;
            !c.is_whitespace()
        });
        self.current
    }

    fn start(&mut self) {
        self.start_pos = self.pos;
    }

    fn next(&mut self) -> Option<char> {
        self.current = self.iter.next();
        if let Some(current) = self.current {
            self.pos += 1;
        }
        self.current
    }

    fn token(&mut self, token: impl Into<TokenKind>) {
        self.tokens.push(Token {
            kind: token.into(),
            span: self.start_pos..=self.pos,
        });
    }

    fn parse_unsigned(&mut self) -> Option<u64> {
        if let Some(c) = self.current && let Some(num) = c.to_digit(10) {
            let mut num = num as u64;
            while let Some(c) = self.next() {
                if let Some(digit) = c.to_digit(10) {
                    num = num * 10 + digit as u64;
                } else {
                    break;
                }
            }
            return Some(num);
        }
        None
    }

    fn parse_raw(&mut self) {
        while let Some(current) = self.current {
            if current.is_whitespace() {
                self.next_non_whitespace();
                continue;
            }
            self.start();
            if let Some(alpha) = Alpha::from_char(current) {
                if self.next() == Some('_') {
                    if let Some(index) = self.next().and_then(|next| Alpha::from_char(next)) {
                        self.token(Identifier::new(alpha, Some(AlphaNumerical::Alpha(index))));
                        self.next();
                    } else if let Some(index) = self.parse_unsigned() {
                        self.token(Identifier::new(
                            alpha,
                            Some(AlphaNumerical::Numerical(index)),
                        ));
                    } else {
                        panic!("Invalid identifier");
                    }
                } else if self.current.map(|c| c.is_alphabetic()).unwrap_or(false) {
                    let mut function = String::new();
                    function.push(current);
                    function.push(self.current.unwrap());
                    while let Some(c) = self.next() {
                        if c.is_alphanumeric() {
                            function.push(c);
                        } else {
                            break;
                        }
                    }
                    if let Some(func) = Function::from_str(&function) {
                        self.token(func);
                    } else {
                        panic!("Invalid function");
                    }
                } else {
                    self.token(Identifier::new(alpha, None));
                }
            } else if let Some(num) = self.parse_unsigned() {
                let num = if self.current == Some('.') {
                    self.next();
                    if let Some(decimals) = self.parse_unsigned() && decimals > 0 {
                        num as f64 + (decimals as f64 / 10.0f64.powi(decimals.log10() as i32 + 1))
                    } else {
                        num as f64
                    }
                } else {
                    num as f64
                };
                self.token(num);
            } else if let Some(op) = Operator::from_char(current) {
                self.token(op);
                self.next();
            } else if let Some(op) = match current {
                '=' => Some(EqualityOperator::Equal),
                '!' => {
                    if self.next() == Some('=') {
                        Some(EqualityOperator::NotEqual)
                    } else {
                        panic!("Expected =");
                    }
                }
                '<' => {
                    if self.next() == Some('=') {
                        Some(EqualityOperator::LessOrEqual)
                    } else {
                        Some(EqualityOperator::Less)
                    }
                }
                '>' => {
                    if self.next() == Some('=') {
                        Some(EqualityOperator::GreaterOrEqual)
                    } else {
                        Some(EqualityOperator::Greater)
                    }
                }
                _ => None,
            } {
                self.token(op);
                self.next();
            } else if let Some(ctrl) = match current {
                '(' | ')' | '{' | '}' | ',' => Some(current),
                _ => None,
            } {
                self.token(TokenKind::Ctrl(ctrl));
                self.next();
            } else {
                panic!("Invalid character {} at {}", current, self.pos);
            }
        }
    }

    fn parse(&mut self) {
        self.parse_raw();
    }
}

struct TokenParser<'a> {
    tokens: &'a [Token],
    pos: usize,
}

impl<'a> TokenParser<'a> {
    fn new(tokens: &'a [Token]) -> Self {
        Self { tokens, pos: 0 }
    }

    fn current(&self) -> Option<&Token> {
        self.tokens.get(self.pos)
    }

    fn next(&mut self) -> Option<&Token> {
        self.pos += 1;
        self.tokens.get(self.pos)
    }

    fn peek(&mut self) -> Option<&Token> {
        self.tokens.get(self.pos + 1)
    }

    fn end_of_paren(&mut self, end_char: char) -> Result<usize, (usize, char)> {
        let mut depth = 0;
        Ok(loop {
            match self.next() {
                Some(Token {
                    kind: TokenKind::Ctrl(c @ ')' | c @ '}'),
                    ..
                }) => {
                    let c = *c;
                    if depth == 0 {
                        if c == end_char {
                            break self.pos;
                        } else {
                            return Err((self.pos, c));
                        }
                    } else {
                        depth -= 1;
                    }
                }

                Some(Token {
                    kind: TokenKind::Ctrl('(' | '{'),
                    ..
                }) => depth += 1,

                None => break self.pos,
                _ => {}
            }
        })
    }

    fn parse_args(&mut self) -> Vec<Expr> {
        let mut args = Vec::new();
        let mut start = self.pos + 1;
        loop {
            match self.end_of_paren(',') {
                Ok(end) => {
                    args.push(TokenParser::new(&self.tokens[start..end]).parse_expr());
                    start = end + 1;
                }
                Err((end, ')')) => {
                    args.push(TokenParser::new(&self.tokens[start..end]).parse_expr());
                    break;
                }
                _ => {
                    panic!("Expected ')' as ender");
                }
            }
        }
        args
    }

    fn parse_value(&mut self) -> Option<(bool, Fragment)> {
        let mut neg = false;
        while let Some(current) = self.current() {
            match current.kind {
                TokenKind::Operator(Operator::Sub) => {
                    neg = !neg;
                }
                TokenKind::Ctrl('(') => {
                    let start = self.pos + 1;
                    if let Ok(end) = self.end_of_paren(')') {
                        return Some((
                            neg,
                            Fragment::Group(
                                TokenParser::new(&self.tokens[start..end]).parse_expr(),
                            ),
                        ));
                    } else {
                        panic!("Expected ')' as ender");
                    }
                }
                TokenKind::Function(function) => {
                    if let Some(Token {
                        kind: TokenKind::Ctrl('('),
                        ..
                    }) = self.next()
                    {
                        return Some((neg, Fragment::Function(function, self.parse_args())));
                    } else {
                        panic!("Expected '(' after function");
                    }
                }
                TokenKind::Number(num) => {
                    return Some((neg, Fragment::Number(num)));
                }
                TokenKind::Identifier(ident) => {
                    if matches!(
                        self.peek(),
                        Some(Token {
                            kind: TokenKind::Ctrl('('),
                            ..
                        })
                    ) {
                        self.next();
                        return Some((
                            neg,
                            Fragment::Function(Function::Defined(ident), self.parse_args()),
                        ));
                    } else {
                        return Some((neg, Fragment::Identifier(ident)));
                    }
                }
                _ => {
                    return None;
                }
            }
            self.next();
        }
        unreachable!()
    }

    fn parse_expr(&mut self) -> Expr {
        let mut fragments = Vec::new();
        let mut exp_op = false;
        let mut sign = false;
        let mut factors = Vec::new();
        let mut div_factors = Vec::new();
        let mut div_last = false;
        fn create_factors(factors: &Vec<Fragment>, div_factors: &Vec<Fragment>) -> Vec<Fragment> {
            match (factors.is_empty(), div_factors.is_empty()) {
                (true, _) => vec![Fragment::Number(0.0)],
                (false, false) => factors.clone(),
                (false, true) => vec![Fragment::Division(
                    Expr::from(factors.clone()),
                    Expr::from(div_factors.clone()),
                )],
            }
        }
        while let Some(current) = self.current() {
            if exp_op {
                match current.kind {
                    TokenKind::Operator(Operator::Add) => {
                        fragments.push((sign, create_factors(&factors, &div_factors)));
                        sign = false;
                        factors.clear();
                        div_factors.clear();
                        exp_op = false;
                        div_last = false;
                    }
                    TokenKind::Operator(Operator::Sub) => {
                        fragments.push((sign, create_factors(&factors, &div_factors)));
                        sign = true;
                        factors.clear();
                        div_factors.clear();
                        exp_op = false;
                        div_last = false;
                    }
                    TokenKind::Operator(Operator::Mul) => {
                        exp_op = false;
                        div_last = false;
                    }
                    TokenKind::Operator(Operator::Div) => {
                        self.next();
                        if let Some((s, dividend)) = self.parse_value() {
                            div_factors.push(dividend);
                            sign ^= s;
                            exp_op = true;
                            div_last = true;
                        } else {
                            panic!("Unexpected token");
                        }
                    }
                    TokenKind::Operator(Operator::Pow) => {
                        let factors = if div_last {
                            &mut div_factors
                        } else {
                            &mut factors
                        };
                        if let Some(base) = factors.pop() {
                            self.next();
                            if let Some(exp) = self.parse_value() {
                                factors.push(Fragment::Pow(Expr::from(base), Expr::from(exp)));
                                exp_op = true;
                            } else {
                                panic!("Unexpected token");
                            }
                        } else {
                            unreachable!()
                        }
                    }
                    _ => {
                        if let Some((s, fragment)) = self.parse_value() {
                            factors.push(fragment);
                            sign ^= s;
                            exp_op = true;
                            div_last = false;
                        } else {
                            panic!("Unexpected token");
                        }
                    }
                }
            } else if let Some((s, fragment)) = self.parse_value() {
                factors.push(fragment);
                sign ^= s;
                exp_op = true;
                div_last = false;
            } else {
                panic!("Unexpected token");
            }
            self.next();
        }

        Expr::from(fragments)
    }

    fn parse(&mut self) {}
}

#[cfg(test)]
mod tests {
    use crate::TextParser;

    #[test]
    fn simple_statements() {
        let mut parser = TextParser::new("a + b_2 * b_e(E_13 * 22 - 33.44 ^ 44.22) < 0.0");
        parser.parse();
    }
}
