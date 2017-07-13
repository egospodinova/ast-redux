use std::sync::Arc;

use syntax::errors::DiagnosticBuilder;
use syntax::errors::emitter::Emitter;

use diagnostics::Diagnostic;

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
}

pub struct TransformingEmitter {
    transformer: Arc<DiagnosticTransformer>
}

impl TransformingEmitter {
    pub fn new(transformer: Arc<DiagnosticTransformer>) -> TransformingEmitter {
        TransformingEmitter {
            transformer: transformer
        }
    }
}

impl Emitter for TransformingEmitter {
    fn emit(&mut self, db: &DiagnosticBuilder) {
        println!("{:?}", *db);
    }
}
