#![feature(let_else, array_try_map, iterator_try_collect, let_chains, bench_black_box)]

#[cfg(feature = "cranelift")]
pub mod cranelift;

#[cfg(feature = "naga")]
pub mod naga;
