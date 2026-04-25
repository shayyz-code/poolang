use std::error::Error;
use std::fmt;
use std::cell::Cell;
use std::panic::{catch_unwind, set_hook, take_hook, UnwindSafe};
use std::sync::{Mutex, OnceLock};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LangErrorKind {
    Io,
    Parse,
    Runtime,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LangError {
    pub kind: LangErrorKind,
    pub message: String,
}

impl LangError {
    pub fn io(message: String) -> Self {
        Self {
            kind: LangErrorKind::Io,
            message,
        }
    }

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

fn panic_hook_lock() -> &'static Mutex<()> {
    static LOCK: OnceLock<Mutex<()>> = OnceLock::new();
    LOCK.get_or_init(|| Mutex::new(()))
}

thread_local! {
    static PANIC_HOOK_DEPTH: Cell<usize> = const { Cell::new(0) };
}

pub fn catch_unwind_silent<F, T>(f: F) -> std::thread::Result<T>
where
    F: FnOnce() -> T + UnwindSafe,
{
    let previous_depth = PANIC_HOOK_DEPTH.with(|depth| {
        let current = depth.get();
        depth.set(current + 1);
        current
    });

    let result = if previous_depth > 0 {
        catch_unwind(f)
    } else {
        let _guard = panic_hook_lock()
            .lock()
            .expect("failed to lock panic hook mutex");
        let previous_hook = take_hook();
        set_hook(Box::new(|_| {}));
        let result = catch_unwind(f);
        set_hook(previous_hook);
        result
    };

    PANIC_HOOK_DEPTH.with(|depth| {
        let current = depth.get();
        depth.set(current.saturating_sub(1));
    });

    result
}
