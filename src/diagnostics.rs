#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum Level {
    Info,
    Debug,
    Warn,
    Error,
    Fatal
}

#[derive(Copy, Clone, Debug)]
pub struct Diagnostic {
    pub level: Level
}
