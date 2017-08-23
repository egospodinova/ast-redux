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

