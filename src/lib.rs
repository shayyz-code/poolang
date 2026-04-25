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
