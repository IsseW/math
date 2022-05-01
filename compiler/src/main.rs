#![feature(let_else, array_try_map)]

fn main() {
    println!("Hello, world!");
}

#[cfg(feature = "cranelift")]
pub mod cranelift;
