// src/ast.rs
use crate::lexer::Token;

#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
    Number(i64),
    Identifier(String),
    BinaryOp(Box<Expr>, Token, Box<Expr>),
    UnaryOp(Token, Box<Expr>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Stmt {
    Expression(Expr),
    Assignment(String, Expr),
    If(Expr, Vec<Stmt>, Vec<Stmt>),  // If (condition, if-body, else-body)
    While(Expr, Vec<Stmt>),
    Return(Expr),
}
