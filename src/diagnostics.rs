// Copyright 2017  Emma Gospodinova <emma.gospodinova@gmail.com>
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

use types::Span;

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum Level {
    Info,
    Note,
    Warning,
    Error,
    Fatal
}

#[derive(Clone, Debug)]
pub struct Diagnostic {
    pub level: Level,
    pub primary_spans: Vec<Span>,
    pub message: String,
}

