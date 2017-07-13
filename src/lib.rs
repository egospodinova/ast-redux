#![feature(rustc_private)]
#![feature(set_stdio)]

extern crate syntax;
extern crate libc;

mod api;
mod diagnostics;
mod types;
mod parser;
mod visit;

pub use api::*;
