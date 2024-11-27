// src/ast.rs
use crate::lexer::Token;

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Void,
    Bool,
    Int,
    Float,
    String,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
    Int(i64),
    Float(f64),
    Boolean(bool),
    String(String),
    Identifier(String),
    BinaryOp(Box<Expr>, Token, Box<Expr>),
    UnaryOp(Token, Box<Expr>),
    FunctionCall(String, Vec<Expr>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Stmt {
    Use(String),
    Expression(Expr),
    Assignment(String, Expr, bool),
    Reassignment(String, Expr),
    FunctionDeclaration(String, Vec<String>, Vec<Stmt>, Type),
    If(Expr, Vec<Stmt>, Vec<Stmt>), // If (condition, if-body, else-body)
    While(Expr, Vec<Stmt>),
    For(String, Expr, Expr, Vec<Stmt>),
    Return(Option<Expr>),
}

