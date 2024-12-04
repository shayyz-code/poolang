// src/ast.rs
use crate::lexer::Token;
use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Void,
    Bool,
    Int,
    Float,
    Char,
    String,
    Vector(Box<Type>),
    Object(HashMap<String, Type>),
    Function(Vec<Type>, Box<Type>),
    BuiltinFunction,
    Module,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
    Null,
    Int(i64),
    Float(f64),
    Boolean(bool),
    Char(char),
    String(String),
    Vector(Vec<Expr>), // TODO Vector and its inner type
    VectorIndex(Box<Expr>, Box<Expr>),
    Identifier(String),
    BinaryOp(Box<Expr>, Token, Box<Expr>),
    UnaryOp(Token, Box<Expr>),
    FunctionCall(String, Vec<Expr>),
    MethodCall(Box<Expr>, String, Vec<Expr>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Stmt {
    Use(String),
    Expression(Expr),
    Assignment(String, Expr, bool, Type),
    Reassignment(String, Expr),
    FunctionDeclaration(String, Vec<(String, Type)>, Vec<Stmt>, Type),
    If(Expr, Vec<Stmt>, Vec<Stmt>), // If (condition, if-body, else-body)
    While(Expr, Vec<Stmt>),
    ForRange(String, Expr, Expr, Expr, Vec<Stmt>),
    ForVector(String, Expr, Vec<Stmt>),
    Return(Option<Expr>),
}
