use std::error::Error;
use std::fmt;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LangErrorKind {
    Parse,
    Runtime,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LangError {
    pub kind: LangErrorKind,
    pub message: String,
}

impl LangError {
    pub fn parse(message: String) -> Self {
        Self {
            kind: LangErrorKind::Parse,
            message,
        }
    }

    pub fn runtime(message: String) -> Self {
        Self {
            kind: LangErrorKind::Runtime,
            message,
        }
    }
}

impl fmt::Display for LangError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?} error: {}", self.kind, self.message)
    }
}

impl Error for LangError {}

pub fn panic_payload_to_message(payload: Box<dyn std::any::Any + Send>) -> String {
    if let Some(msg) = payload.downcast_ref::<&str>() {
        (*msg).to_string()
    } else if let Some(msg) = payload.downcast_ref::<String>() {
        msg.clone()
    } else {
        "unknown panic".to_string()
    }
}
