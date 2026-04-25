pub mod ast;
pub mod errors;
pub mod interpreter;
pub mod lexer;
pub mod parser;
pub mod type_inference;
pub mod visitor;

use errors::LangError;
use interpreter::{Interpreter, Value};
use lexer::Lexer;
use parser::Parser;
use std::panic::{AssertUnwindSafe, catch_unwind};

pub fn run_source(input: String) -> Option<Value> {
    run_source_checked(input).unwrap_or_else(|error| panic!("{error}"))
}

pub fn run_source_checked(input: String) -> Result<Option<Value>, LangError> {
    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    let ast = catch_unwind(AssertUnwindSafe(|| parser.parse()))
        .map_err(|payload| LangError::parse(extract_panic_message(payload)))?;

    let mut interpreter = Interpreter::new();
    catch_unwind(AssertUnwindSafe(|| interpreter.interpret(&ast)))
        .map_err(|payload| LangError::runtime(extract_panic_message(payload)))
}

fn extract_panic_message(payload: Box<dyn std::any::Any + Send>) -> String {
    if let Some(msg) = payload.downcast_ref::<&str>() {
        (*msg).to_string()
    } else if let Some(msg) = payload.downcast_ref::<String>() {
        msg.clone()
    } else {
        "unknown panic".to_string()
    }
}
