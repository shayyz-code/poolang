// main.rs
mod ast;
// mod compiler;
mod interpreter;
mod lexer;
mod parser;
mod type_inference;
mod visitor;

// use compiler::Codegen;
use interpreter::Interpreter;
use lexer::Lexer;
use parser::Parser;
use std::env;
use std::fs;
use std::process;

// use inkwell::context::Context;

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
        println!("{:?}\n", stmt)
    }

    // let context = Context::create();
    // let mut codegen = Codegen::new(&context, "my_module");

    // for stmt in &ast {
    //     codegen.compile_stmt(&stmt);
    // }

    // codegen.print_ir();
    // let mut analyzer = visitor::SemanticAnalyzer::new();
    // analyzer.analyze(&ast);

    let mut interpreter = Interpreter::new();
    let result = interpreter.interpret(&ast);

    match result {
        Some(value) => println!("Result: {:?}", value),
        None => println!("No return value"),
    }
}
