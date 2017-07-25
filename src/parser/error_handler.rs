use std::sync::{Arc, Mutex};

use syntax::errors;
use syntax::errors::emitter;

use diagnostics::{Diagnostic, Level};

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
    transformer: Arc<Mutex<DiagnosticTransformer>>
}

impl TransformingEmitter {
    pub fn new(transformer: Arc<Mutex<DiagnosticTransformer>>) -> TransformingEmitter {
        TransformingEmitter {
            transformer: transformer
        }
    }

    pub fn transform_diagnostic(&mut self, db: &errors::DiagnosticBuilder) -> Diagnostic {
        Diagnostic {
            level: Level::Error
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
