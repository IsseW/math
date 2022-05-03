use std::{
    collections::HashMap,
    fmt::Display,
    ops::{Deref, RangeInclusive},
    str::Chars,
};

use crate::{
    identifier::{Alpha, AlphaNumerical, Identifier},
    value::{Define, Expr, Expression, Factor, Func, Statement, ToTerm},
};

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub enum Operator {
    Add,
    Sub,
    Mul,
    Div,
    Pow,
}

impl Operator {
    pub fn from_char(c: char) -> Option<Self> {
        match c {
            '+' => Some(Operator::Add),
            '-' => Some(Operator::Sub),
            '*' => Some(Operator::Mul),
            '/' => Some(Operator::Div),
            '^' => Some(Operator::Pow),
            _ => None,
        }
    }
}

#[derive(Clone, Copy, PartialEq, Debug)]
pub enum EqualityOperator {
    Equal,
    NotEqual,
    Less,
    LessOrEqual,
    Greater,
    GreaterOrEqual,
}

impl Display for EqualityOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            EqualityOperator::Equal => write!(f, "="),
            EqualityOperator::NotEqual => write!(f, "!="),
            EqualityOperator::Less => write!(f, "<"),
            EqualityOperator::LessOrEqual => write!(f, "<="),
            EqualityOperator::Greater => write!(f, ">"),
            EqualityOperator::GreaterOrEqual => write!(f, ">="),
        }
    }
}

#[derive(Clone, Copy, PartialEq, Debug)]
pub enum TokenKind {
    Number(f64),
    Identifier(Identifier),
    Operator(Operator),
    Eq(EqualityOperator),
    Ctrl(char),
    Function(Func),
    Assign,
}

#[derive(Debug)]
pub struct Token {
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

impl From<Func> for TokenKind {
    fn from(func: Func) -> Self {
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

#[derive(Debug, Clone)]
pub enum Syntax {
    InvalidIdentifier,
    InvalidFunction,
    ExpectedChar(char),
    UnexpectedChar,
}

#[derive(Debug, Clone)]
pub struct Error<T> {
    pub err: T,
    pub span: RangeInclusive<usize>,
}

impl<T> Error<T> {
    pub fn new(err: T, span: RangeInclusive<usize>) -> Self {
        Error { err, span }
    }

    pub fn map<U>(self, f: impl FnOnce(T) -> U) -> Error<U> {
        Error {
            err: f(self.err),
            span: self.span,
        }
    }
}

pub struct TextParser<'a> {
    pos: usize,
    start_pos: usize,
    current: Option<char>,
    iter: Chars<'a>,
    tokens: Vec<Token>,
    errors: Vec<Error<Syntax>>,
}

impl<'a> TextParser<'a> {
    pub fn new(s: &'a str) -> Self {
        let mut iter = s.chars();
        let next = iter.next();
        TextParser {
            pos: 0,
            start_pos: 0,
            current: next,
            iter,
            tokens: Vec::new(),
            errors: Vec::new(),
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
        if self.current.is_some() {
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

    fn error(&mut self, kind: Syntax) {
        self.errors.push(Error {
            err: kind,
            span: self.start_pos..=self.pos,
        });
    }

    fn parse_unsigned(&mut self) -> Option<u64> {
        if let Some(c) = self.current && let Some(num) = c.to_digit(10) {
            let mut num = num as u64;
            while let Some(c) = self.next() {
                if let Some(digit) = c.to_digit(10) {
                    let digit = digit as u64;
                    if num < u64::MAX / 10 - digit {
                        num = num * 10 + digit;
                    }
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
                        self.error(Syntax::InvalidIdentifier);
                        self.next();
                    }
                } else if self.current.map(|c| c.is_alphabetic()).unwrap_or(false) {
                    let mut function = String::new();
                    function.push(current);
                    function.push(self.current.unwrap());
                    while let Some(c) = self.next() && c.is_alphanumeric() {
                        function.push(c);
                    }
                    if let Some(func) = Func::from_str(&function) {
                        self.token(func);
                    } else {
                        self.error(Syntax::InvalidFunction);
                        self.next();
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
            } else if current == ':' {
                if self.next() == Some('=') {
                    self.token(TokenKind::Assign);
                    self.next();
                } else {
                    self.error(Syntax::ExpectedChar('='));
                }
            } else if let Some(op) = match current {
                '=' => Some(EqualityOperator::Equal),
                '!' => {
                    if self.next() == Some('=') {
                        Some(EqualityOperator::NotEqual)
                    } else {
                        self.error(Syntax::ExpectedChar('='));
                        continue;
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
                self.error(Syntax::UnexpectedChar);
                self.next();
            }
        }
    }

    pub fn parse(mut self) -> Result<Vec<Token>, Vec<Error<Syntax>>> {
        self.parse_raw();
        if self.errors.is_empty() {
            Ok(self.tokens)
        } else {
            Err(self.errors)
        }
    }
}

#[derive(Debug, Clone)]
pub enum Parse {
    ExpectedToken(TokenKind),
    ExpectedValue,
}

#[derive(Debug, Clone)]
pub enum Full {
    Syntax(Syntax),
    Parse(Parse),
    NotExpression,
}

#[derive(Debug)]
pub struct TokenParser<'a> {
    tokens: &'a [Token],
    pos: usize,
    errors: &'a mut Vec<Error<Parse>>,
}

impl<'a> TokenParser<'a> {
    pub fn new(tokens: &'a [Token], errors: &'a mut Vec<Error<Parse>>) -> Self {
        Self {
            tokens,
            pos: 0,
            errors,
        }
    }

    fn error(&mut self, kind: Parse) {
        self.errors.push(Error {
            err: kind,
            span: self.tokens[self.pos.min(self.tokens.len() - 1)]
                .span
                .clone(),
        });
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

                None => return Err((self.pos, '\0')),
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
                    args.push(TokenParser::new(&self.tokens[start..end], self.errors).parse_expr());
                    start = end + 1;
                }
                Err((end, ')')) => {
                    args.push(TokenParser::new(&self.tokens[start..end], self.errors).parse_expr());
                    break;
                }
                _ => {
                    self.error(Parse::ExpectedToken(TokenKind::Ctrl(')')));
                    break;
                }
            }
        }
        args
    }

    fn parse_value(&mut self) -> Option<(bool, Factor)> {
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
                            Factor::Group(
                                TokenParser::new(&self.tokens[start..end], self.errors)
                                    .parse_expr(),
                            ),
                        ));
                    } else {
                        self.error(Parse::ExpectedToken(TokenKind::Ctrl(')')));
                    }
                }
                TokenKind::Function(function) => {
                    if let Some(Token {
                        kind: TokenKind::Ctrl('('),
                        ..
                    }) = self.next()
                    {
                        let start = self.pos + 1;
                        if let Ok(end) = self.end_of_paren(')') {
                            return Some((
                                neg,
                                Factor::Func(
                                    function,
                                    TokenParser::new(&self.tokens[start..end], self.errors)
                                        .parse_expr(),
                                ),
                            ));
                        } else {
                            self.error(Parse::ExpectedToken(TokenKind::Ctrl(')')));
                        }
                    } else {
                        self.error(Parse::ExpectedToken(TokenKind::Ctrl('(')));
                    }
                }
                TokenKind::Number(num) => {
                    return Some((neg, Factor::Number(num)));
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
                        return Some((neg, Factor::Call(ident, self.parse_args())));
                    } else {
                        return Some((neg, Factor::Identifier(ident)));
                    }
                }
                _ => {
                    return None;
                }
            }
            self.next();
        }
        None
    }

    fn parse_expr(&mut self) -> Expr {
        let mut terms = Vec::new();
        let mut exp_op = false;
        let mut sign = false;
        let mut factors = Vec::new();
        let mut div_factors = Vec::new();
        let mut div_last = false;
        while let Some(current) = self.current() {
            if exp_op {
                match current.kind {
                    TokenKind::Operator(Operator::Add) => {
                        terms.push((sign, &factors, &div_factors).term());
                        sign = false;
                        factors.clear();
                        div_factors.clear();
                        exp_op = false;
                        div_last = false;
                    }
                    TokenKind::Operator(Operator::Sub) => {
                        terms.push((sign, &factors, &div_factors).term());
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
                            self.error(Parse::ExpectedValue);
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
                                factors.push(Factor::Pow(Expr::from(base), Expr::from(exp)));
                                exp_op = true;
                            } else {
                                self.error(Parse::ExpectedValue);
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
                            self.error(Parse::ExpectedValue);
                        }
                    }
                }
            } else if let Some((s, fragment)) = self.parse_value() {
                factors.push(fragment);
                sign ^= s;
                exp_op = true;
                div_last = false;
            } else {
                self.error(Parse::ExpectedValue);
            }
            self.next();
        }

        terms.push((sign, factors, div_factors).term());

        Expr::from(terms)
    }

    pub fn parse(&mut self) -> Statement {
        let expr = self.parse_expr();
        Statement::expr(expr)
    }
}

pub fn calculate_expr(
    string: &str,
    defines: &HashMap<Identifier, Define>,
) -> Result<f64, Vec<Error<Full>>> {
    let tokens = TextParser::new(string).parse().map_err(|e| {
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
    let expr = statement.as_expr().ok_or_else(|| {
        vec![Error {
            err: Full::NotExpression,
            span: 0..=string.len() - 1,
        }]
    })?;
    Ok(expr.evaluate(defines))
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use super::*;

    #[test]
    fn simple_statements() {
        assert_eq!(
            calculate_expr("2 * cos(0.5) + 2 * 3 ^ 2", &HashMap::new()).unwrap(),
            2.0 * 0.5f64.cos() + 2.0 * 3.0f64.powf(2.0)
        );
        for x in -10..=10 {
            let x = x as f64 / 10.0;
            assert_eq!(
                calculate_expr(
                    "2 * cos(x) + 2 * x ^ 2",
                    &HashMap::from([(Identifier::from('x'), Expr::from(x).into())])
                )
                .unwrap(),
                2.0 * x.cos() + 2.0 * x.powf(2.0)
            );
        }

        let defines = &HashMap::from([(
            Identifier::from('f'),
            Define::from((
                Identifier::from('x'),
                Expr::try_from("2x ^ 2 - 3x + 1").unwrap(),
            )),
        )]);
        let f = |f: f64| 2.0 * f.powf(2.0) - 3.0 * f + 1.0;

        assert_eq!(
            calculate_expr("f(10) + f(2) ^ f(3)", defines).unwrap(),
            f(10.0) + f(2.0).powf(f(3.0))
        );
    }

    #[test]
    fn derivative() {
        let id = Identifier::from('x');
        let expr = Expr::try_from("x^3 + x^2 + x + 1 + ln(x^2)").unwrap();
        let derivative = expr.derivative(id);
        println!("{derivative}");
        let d = |x: f64| 3.0 * x.powf(2.0) + 2.0 * x + 1.0 + 2.0 / x;
        for x in -10..=10 {
            let x = x as f64 / 10.0;
            let expr = Expr::from(x);
            let correct = d(x);
            if !correct.is_nan() {
                let value = derivative.evaluate(&HashMap::from([(id, Define::from(expr))]));
                assert!((value - correct).abs() < 0.00001);
            }
        }
    }
}
