//! # PooLang
//!
//! A tiny interpreted language written in Rust.
//!
//! This crate provides the core pipeline for PooLang, including lexical analysis,
//! parsing, and execution through an AST-walking interpreter.
//!
//! ## Example
//!
//! ```rust
//! use poo::{run_source_checked, interpreter::Value};
//!
//! let source = "poo x << 10; return x;".to_string();
//! let result = run_source_checked(source).unwrap();
//! assert_eq!(result, Some(Value::Integer(10)));
//! ```

pub mod ast;
pub mod errors;
pub mod interpreter;
pub mod lexer;
pub mod parser;
pub mod type_inference;
pub mod visitor;

use errors::{LangError, LangErrorKind};
use interpreter::{Interpreter, Value};
use lexer::Lexer;
use parser::Parser;
use std::fs;

/// Executes the given PooLang source code.
///
/// # Panics
///
/// Panics if there is a syntax or runtime error. Use [`run_source_checked`]
/// for a version that returns a `Result`.
pub fn run_source(input: String) -> Option<Value> {
    run_source_checked(input).unwrap_or_else(|error| panic!("{error}"))
}

/// Executes the PooLang code from a file.
///
/// # Panics
///
/// Panics if the file cannot be read, or if there is a syntax or runtime error.
/// Use [`run_file_checked`] for a version that returns a `Result`.
pub fn run_file(file_path: &str) -> Option<Value> {
    run_file_checked(file_path).unwrap_or_else(|error| panic!("{error}"))
}

/// Executes the given PooLang source code and returns the result.
///
/// Returns a [`LangError`] if parsing or execution fails.
pub fn run_source_checked(input: String) -> Result<Option<Value>, LangError> {
    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    let ast = parser.parse_checked()?;

    let mut interpreter = Interpreter::new();
    interpreter.interpret_checked(&ast)
}

/// Executes the PooLang code from a file and returns the result.
///
/// Returns a [`LangError`] if the file cannot be read, or if parsing/execution fails.
pub fn run_file_checked(file_path: &str) -> Result<Option<Value>, LangError> {
    let input = fs::read_to_string(file_path)
        .map_err(|error| LangError::io(format!("failed to read '{file_path}': {error}")))?;
    run_source_checked(input).map_err(|error| match error.kind {
        LangErrorKind::Parse => LangError::parse(format!("in '{file_path}': {}", error.message)),
        LangErrorKind::Runtime => {
            LangError::runtime(format!("in '{file_path}': {}", error.message))
        }
        LangErrorKind::Io => error,
    })
}
