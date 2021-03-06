// Copyright 2017  Emma Gospodinova <emma.gospodinova@gmail.com>
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

use std::slice::Iter;

use diagnostics;
use api::types::{RSRange, RSLocation};

#[repr(C)]
pub enum RSDiagnosticLevel {
    Info,
    Note,
    Warning,
    Error,
    Fatal
}

pub struct RSDiagnostic<'a>(pub &'a diagnostics::Diagnostic);

impl<'a> RSDiagnostic<'a> {
    pub fn new(diagnostic: &'a diagnostics::Diagnostic) -> RSDiagnostic<'a> {
        RSDiagnostic(diagnostic)
    }

    pub fn get_level(&self) -> RSDiagnosticLevel {
        match self.0.level {
            diagnostics::Level::Info    => RSDiagnosticLevel::Info,
            diagnostics::Level::Note    => RSDiagnosticLevel::Note,
            diagnostics::Level::Warning => RSDiagnosticLevel::Warning,
            diagnostics::Level::Error   => RSDiagnosticLevel::Error,
            diagnostics::Level::Fatal   => RSDiagnosticLevel::Fatal,
        }
    }

    pub fn get_message(&self) -> &str {
        &self.0.message
    }

    pub fn get_primary_range(&self) -> RSRange {
        if let Some(ref span) = self.0.primary_spans.first() {
            RSRange::from_span(span)
        } else {
            RSRange::invalid()
        }
    }
}

pub struct RSDiagnosticIterator<'a>(pub Iter<'a, diagnostics::Diagnostic>);
