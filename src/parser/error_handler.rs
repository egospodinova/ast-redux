// Copyright 2017  Emma Gospodinova <emma.gospodinova@gmail.com>
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

use std::rc::Rc;
use std::sync::{Arc, Mutex};

use syntax::errors;
use syntax::errors::emitter;
use syntax::codemap::{self, CodeMap};

use diagnostics::{Diagnostic, Level};
use types::{Location, Span};

pub struct DiagnosticTransformer {
    diagnostics: Vec<Diagnostic>
}

impl DiagnosticTransformer {
    pub fn new() -> DiagnosticTransformer {
        DiagnosticTransformer {
            diagnostics: vec![]
        }
    }

    pub fn get_diagnostics(&self) -> &Vec<Diagnostic> {
        &self.diagnostics
    }

    pub fn add_diagnostic(&mut self, diagnostic: Diagnostic) {
        self.diagnostics.push(diagnostic)
    }
}

pub struct TransformingEmitter {
    transformer: Arc<Mutex<DiagnosticTransformer>>,
    codemap: Rc<CodeMap>
}

impl TransformingEmitter {
    pub fn new(transformer: Arc<Mutex<DiagnosticTransformer>>, codemap: Rc<CodeMap>) -> TransformingEmitter {
        TransformingEmitter {
            transformer: transformer,
            codemap: codemap
        }
    }

    pub fn transform_diagnostic(&mut self, db: &errors::DiagnosticBuilder) -> Diagnostic {
        Diagnostic {
            level: self.transform_level(&(*db).level),
            primary_spans: (*db).span.primary_spans().iter().map(|s| self.transform_span(&s)).collect(),
            message: (*db).message(),
        }
    }

    fn transform_level(&mut self, level: &errors::Level) -> Level {
        match *level {
            errors::Level::Bug         => Level::Fatal,
            errors::Level::Fatal       => Level::Fatal,
            errors::Level::PhaseFatal  => Level::Fatal,
            errors::Level::Error       => Level::Error,
            errors::Level::Warning     => Level::Warning,
            errors::Level::Note        => Level::Note,
            errors::Level::Help        => Level::Info,
            errors::Level::Cancelled   => Level::Info,
        }
    }

    // Copied from transform.rs; probably possible to remove duplication
    fn transform_span(&mut self, span: &codemap::Span) -> Span {
        let lo = self.codemap.lookup_char_pos(span.lo());
        let hi = self.codemap.lookup_char_pos(span.hi());
        Span {
            start: self.transform_loc(&lo),
            end: self.transform_loc(&hi)
        }
    }

    fn transform_loc(&mut self, loc: &codemap::Loc) -> Location {
        Location {
            line: loc.line as i32,
            column: loc.col.0 as i32
        }
    }
}

impl emitter::Emitter for TransformingEmitter {
    fn emit(&mut self, db: &errors::DiagnosticBuilder) {
        println!("{:?}", *db);
        let diagnostic = self.transform_diagnostic(db);

        self.transformer.lock().unwrap().add_diagnostic(diagnostic);
    }
}
