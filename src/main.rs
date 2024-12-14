mod ast;
mod interpreter;
mod lexer;
mod parser;
mod type_inference;
mod visitor;

use interpreter::Interpreter;
use lexer::Lexer;
use parser::Parser;
use std::env;
use std::fs;
use std::process;

// speed
use std::time::Instant;

fn main() {
    let start = Instant::now();
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

    let mut interpreter = Interpreter::new();
    let result = interpreter.interpret(&ast);

    match result {
        Some(value) => println!("Result: {:?}", value),
        None => print!(""),
    }

    let duration = start.elapsed();

    if args.contains(&"--speed".to_string()) {
        println!("\nInterpreted within: {:?}", duration);
    }
}
