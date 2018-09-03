// Copyright 2017  Emma Gospodinova <emma.gospodinova@gmail.com>
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

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
