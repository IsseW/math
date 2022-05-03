#![feature(let_chains, let_else)]
use std::io;

use interpreter::{Expr, Expression, Identifier, IntoDefines};

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

fn main() {
    let mut input = String::new();
    let mut last: Option<Expr> = None;
    loop {
        input.clear();
        io::stdin().read_line(&mut input).unwrap();
        let input = input.trim();
        match input {
            "quit" => break,
            "draw" => {
                if let Some(ref expr) = last {
                    draw_expr(expr);
                } else {
                    eprintln!("No expression to draw");
                }
            }
            "diff" => {
                if let Some(ref expr) = last {
                    let id = expr.find_identifier().unwrap_or(Identifier::from('x'));
                    let derivative = expr.derivative(id);
                    println!("= {derivative}");
                    last = Some(derivative);
                } else {
                    eprintln!("No expression to differentiate");
                }
            }
            _ if input.starts_with("draw(") => {
                if input.ends_with(')') {
                    match Expr::try_from(&input[5..input.len() - 1]) {
                        Ok(expr) => {
                            draw_expr(&expr);
                            last = Some(expr);
                        }
                        Err(err) => {
                            let mut errs = " ".repeat(input.len());
                            for e in &err {
                                unsafe {
                                    errs.as_bytes_mut()[e.span.clone()]
                                        .iter_mut()
                                        .for_each(|b| *b = b'^');
                                }
                            }
                            eprintln!("     {errs}\n{:?}", err);
                            continue;
                        }
                    }
                } else {
                    eprintln!("Missing closing parenthesis");
                }
            }
            _ => {
                let expr = match Expr::try_from(input) {
                    Ok(e) => e,
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
                        continue;
                    }
                }
                .simplify();
                println!("= {expr}");
                last = Some(expr);
            }
        }
    }
}
