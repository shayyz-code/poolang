pub mod ast;
pub mod errors;
pub mod interpreter;
pub mod lexer;
pub mod parser;
pub mod type_inference;
pub mod visitor;

use interpreter::{Interpreter, Value};
use lexer::Lexer;
use parser::Parser;

pub fn run_source(input: String) -> Option<Value> {
    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    let ast = parser.parse();

    let mut interpreter = Interpreter::new();
    interpreter.interpret(&ast)
}
