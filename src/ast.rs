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
    Map(HashMap<String, Type>),
    Function(Vec<Type>, Box<Type>),
    BuiltinFunction,
    Module,
}

impl Type {
    pub fn get_methods(&mut self) -> Option<HashMap<String, Type>> {
        match self {
            Self::Void => None,
            Self::Bool => None,
            Self::Int => {
                let mut methods = HashMap::new();
                methods.insert(
                    "_f".to_string(),
                    Self::Function(Vec::new(), Box::new(Self::Float)),
                );
                Some(methods)
            }
            Self::Float => {
                let mut methods = HashMap::new();
                methods.insert("_i".to_string(), Self::Int);
                Some(methods)
            }
            Self::Char => None,
            Self::String => {
                let mut methods = HashMap::new();
                methods.insert("chars".to_string(), Self::Vector(Box::new(Self::Char)));
                Some(methods)
            }
            Self::Function(_, _) => None,
            Self::BuiltinFunction => None,
            Self::Vector(vec_type) => {
                let mut methods = HashMap::new();
                methods.insert("filter".to_string(), Self::Vector(vec_type.clone()));
                Some(methods)
            }
            Self::Map(_) => {
                let mut methods = HashMap::new();
                methods.insert("insert".to_string(), Self::Void);
                Some(methods)
            }
            Self::Module => None,
        }
    }
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
    VectorIndex(Box<Expr>, Vec<Expr>),
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
    Assignment(String, Expr, bool, Option<Type>),
    Reassignment(String, Option<Expr>, Expr), // Reassignment (identifier, vector_index_indentifier, value)
    FunctionDeclaration(String, Vec<(String, Type)>, Vec<Stmt>, Type),
    If(Expr, Vec<Stmt>, Vec<Stmt>), // If (condition, if-body, else-body)
    While(Expr, Vec<Stmt>),
    ForRange(String, Expr, Expr, Expr, Vec<Stmt>),
    ForVector(String, Expr, Vec<Stmt>),
    Return(Option<Expr>),
}
