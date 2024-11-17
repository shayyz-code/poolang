// main.rs
mod lexer;
mod parser;
mod ast;
mod interpreter;

use lexer::Lexer;
use parser::Parser;
use interpreter::Interpreter;
use std::env;
use std::fs;
use std::process;
// use interpreter::Interpreter;

fn main() {
    // Get the file path from command line arguments
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        eprintln!("Usage: <program> <file_path>");
        process::exit(1);
    }

    let file_path = &args[1];

    // Read the file content
    let input = match fs::read_to_string(file_path) {
        Ok(content) => content,
        Err(error) => {
            eprintln!("Error reading file: {}", error);
            process::exit(1);
        }
    };

    // Create Lexer, Parser, and Interpreter
    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    let ast = parser.parse();

    for stmt in &ast {
        println!("{:?}\n",stmt)
    }

    let mut interpreter = Interpreter::new();
    let result = interpreter.interpret(&ast);

    match result {
        Some(value) => println!("Result: {}", value),
        None => println!("No return value"),
    }

}