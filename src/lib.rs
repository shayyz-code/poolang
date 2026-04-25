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
use std::fs;

pub fn run_source(input: String) -> Option<Value> {
    run_source_checked(input).unwrap_or_else(|error| panic!("{error}"))
}

pub fn run_source_checked(input: String) -> Result<Option<Value>, LangError> {
    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    let ast = parser.parse_checked()?;

    let mut interpreter = Interpreter::new();
    interpreter.interpret_checked(&ast)
}

pub fn run_file_checked(file_path: &str) -> Result<Option<Value>, LangError> {
    let input = fs::read_to_string(file_path)
        .map_err(|error| LangError::io(format!("failed to read '{file_path}': {error}")))?;
    run_source_checked(input)
}
