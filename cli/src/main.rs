#![feature(let_chains, let_else)]
use std::{collections::HashMap, env, io};

use interpreter::{expr_from_str, Expr, Expression, Identifier, IntoDefines};

fn draw_expr(expr: &Expr) {
    let expr = expr.simplify();
    let id = expr.find_identifier().unwrap_or(Identifier::from('x'));
    const WIDTH: usize = 100;
    const HEIGHT: usize = 20;
    const SAMPLE_WIDTH: f64 = 10.0;
    const SAMPLES: usize = 10;
    let values: Vec<(f64, f64)> = (0..WIDTH + 1)
        .map(|i| {
            (0..SAMPLES)
                .map(|j| {
                    let x = SAMPLE_WIDTH * (i as f64 + (j as f64 / SAMPLES as f64))
                        / (WIDTH as f64 + 0.5)
                        - SAMPLE_WIDTH / 2.0;
                    expr.evaluate(&(id, x).def())
                })
                .fold((f64::INFINITY, f64::NEG_INFINITY), |(min, max), v| {
                    (min.min(v), max.max(v))
                })
        })
        .collect();

    let min = values.iter().fold(f64::INFINITY, |a, (b, _)| a.min(*b));
    let max = values.iter().fold(f64::NEG_INFINITY, |a, (_, b)| a.max(*b));

    for i in 0..HEIGHT {
        for j in 0..WIDTH {
            let last = values[j];
            let current = values[j + 1];
            let (s, e) = (last.0.min(current.0), last.1.max(current.1));
            let y_0 = max - (max - min) * (i + 1) as f64 / (HEIGHT - 1) as f64;
            let y_1 = max - (max - min) * (i) as f64 / (HEIGHT - 1) as f64;
            print!("{}", if s <= y_1 && e >= y_0 { '#' } else { ' ' });
        }
        println!();
    }
    println!(
        "[{:.2},{:.2}]x[{:.2},{:.2}]",
        -SAMPLE_WIDTH / 2.0,
        SAMPLE_WIDTH / 2.0,
        min,
        max
    );
}

fn read_expr(input: &str, macros: &HashMap<&str, Expr>) -> Option<Expr> {
    match expr_from_str(input, &macros) {
        Ok(e) => Some(e),
        Err(err) => {
            let mut errs = " ".repeat(input.len());
            for e in &err {
                unsafe {
                    errs.as_bytes_mut()[e.span.clone()]
                        .iter_mut()
                        .for_each(|b| *b = b'^');
                }
            }
            eprintln!("{errs}\n{:?}", err);
            None
        }
    }
    .map(|e| e.simplify())
}

#[derive(Default)]
struct Context {
    ans: Option<Expr>,
    macros: HashMap<&'static str, Expr>,
}

impl Context {
    fn update_ans(&mut self, expr: Expr) {
        self.ans = Some(expr.clone());
        self.macros.insert("ans", expr);
    }
}

fn main() {
    let mut ctx = Context::default();
    if env::args().len() > 1 {
        for e in env::args().skip(1) {
            println!("{e}");
            if let Some(expr) = read_expr(&e, &ctx.macros) {
                println!("= {expr}")
            }
        }
        return;
    }
    let mut input = String::new();
    loop {
        input.clear();
        io::stdin().read_line(&mut input).unwrap();
        let input = input.trim();
        match input {
            "quit" => break,
            "draw" => {
                if let Some(ref expr) = ctx.ans {
                    draw_expr(expr);
                } else {
                    eprintln!("No expression to draw");
                }
            }
            "diff" => {
                if let Some(ref expr) = ctx.ans {
                    let id = expr.find_identifier().unwrap_or(Identifier::from('x'));
                    let derivative = expr.derivative(id);
                    println!("= {derivative}");
                    ctx.update_ans(derivative);
                } else {
                    eprintln!("No expression to differentiate");
                }
            }
            "eval" => {
                if let Some(ref expr) = ctx.ans {
                    println!("= {}", expr.evaluate(&HashMap::new()));
                } else {
                    eprintln!("No expression to differentiate");
                }
            }
            _ if input.starts_with("draw(") => {
                if input.ends_with(')') {
                    if let Some(expr) = read_expr(&input[5..input.len() - 1], &ctx.macros) {
                        draw_expr(&expr);
                    }
                } else {
                    eprintln!("Missing closing parenthesis");
                }
            }
            _ => {
                if let Some(expr) = read_expr(input, &ctx.macros) {
                    println!("= {expr}");
                    ctx.update_ans(expr);
                }
            }
        }
    }
}
