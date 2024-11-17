// src/interpreter.rs
use std::collections::HashMap;
use crate::ast::{Expr, Stmt};
use crate::lexer::Token;

pub struct Interpreter {
    variables: HashMap<String, i64>,
    return_value: Option<i64>,  // To handle return statements
}

impl Interpreter {
    pub fn new() -> Self {
        Interpreter {
            variables: HashMap::new(),
            return_value: None,
        }
    }

    // Evaluate an expression and return its value
    fn eval_expr(&mut self, expr: &Expr) -> i64 {
        match expr {
            Expr::Number(value) => *value,
            Expr::Identifier(name) => {
                *self.variables.get(name).unwrap_or_else(|| {
                    panic!("Undefined variable: {}", name);
                })
            }
            Expr::BinaryOp(left, op, right) => {
                let left_val = self.eval_expr(left);
                let right_val = self.eval_expr(right);

                match op {
                    Token::Plus => left_val + right_val,
                    Token::Minus => left_val - right_val,
                    Token::Multiply => left_val * right_val,
                    Token::Divide => {
                        if right_val == 0 {
                            panic!("Division by zero");
                        }
                        left_val / right_val
                    }
                    Token::LessThan => (left_val < right_val) as i64,
                    Token::GreaterThan => (left_val > right_val) as i64,
                    Token::Equal => (left_val == right_val) as i64,
                    Token::NotEqual => (left_val != right_val) as i64,
                    _ => panic!("Unexpected operator: {:?}", op),
                }
            }
            Expr::UnaryOp(op, expr) => {
                let value = self.eval_expr(expr);
                match op {
                    Token::Minus => -value,
                    _ => panic!("Unexpected unary operator: {:?}", op),
                }
            }
        }
    }

    // Execute a statement
    fn exec_stmt(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::Expression(expr) => {
                self.eval_expr(expr);
            }
            Stmt::Assignment(name, expr) => {
                let value = self.eval_expr(expr);
                self.variables.insert(name.clone(), value);
            }
            Stmt::If(condition, if_body, else_body) => {
                let condition_value = self.eval_expr(condition);
                if condition_value != 0 {
                    for stmt in if_body {
                        self.exec_stmt(stmt);
                        if self.return_value.is_some() {
                            break;
                        }
                    }
                } else {
                    for stmt in else_body {
                        self.exec_stmt(stmt);
                        if self.return_value.is_some() {
                            break;
                        }
                    }
                }
            }
            Stmt::While(condition, body) => {
                while self.eval_expr(condition) != 0 {
                    for stmt in body {
                        self.exec_stmt(stmt);
                        if self.return_value.is_some() {
                            return;  // Exit loop on return statement
                        }
                    }
                }
            }
            Stmt::Return(expr) => {
                let value = self.eval_expr(expr);
                self.return_value = Some(value);
            }
        }
    }

    // Execute a list of statements
    pub fn interpret(&mut self, statements: &[Stmt]) -> Option<i64> {
        self.return_value = None;
        for stmt in statements {
            self.exec_stmt(stmt);
            if self.return_value.is_some() {
                break;
            }
        }
        self.return_value
    }
}
