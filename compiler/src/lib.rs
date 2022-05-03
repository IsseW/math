#![feature(let_else, array_try_map)]

#[cfg(feature = "cranelift")]
pub mod cranelift;

#[cfg(feature = "naga")]
pub mod naga;
