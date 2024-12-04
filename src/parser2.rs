use crate::ast::{Expr, Stmt, Type};
use crate::lexer::{Lexer, Token};
use crate::type_inference::infer_expr_type;
use crate::visitor::ScopedSymbolTable;
use std::collections::HashMap;

pub struct Parser {
    lexer: Lexer,
    current_token: Token,
}

impl Parser {
    pub fn new(lexer: Lexer) -> Self {
        let mut parser = Self {
            lexer,
            current_token: Token::EOF,
        };
        parser.advance();
        parser
    }

    fn advance(&mut self) {
        self.current_token = self.lexer.next_token();
    }

    fn peek_token(&mut self) -> Token {
        self.lexer.peek_next_token()
    }

    fn eat(&mut self, expected: Token) {
        if self.current_token == expected {
            self.advance();
        } else {
            panic!(
                "Unexpected token: {:?}, expected: {:?}",
                self.current_token, expected
            );
        }
    }

    fn parse_primary(&mut self) -> Expr {
        match &self.current_token {
            Token::Int(value) => {
                let expr = Expr::Int(*value);
                self.advance();
                expr
            }
            Token::Float(value) => {
                let expr = Expr::Float(*value);
                self.advance();
                expr
            }
            Token::Identifier(name) => {
                let identifier = name.clone();
                self.advance();
                match self.current_token {
                    Token::LeftParen => self.parse_function_call(identifier),
                    Token::LeftBracket => self.parse_vector_index(identifier),
                    _ => Expr::Identifier(identifier),
                }
            }
            Token::Char(value) => {
                let expr = Expr::Char(value.clone());
                self.advance();
                expr
            }
            Token::String(value) => {
                let expr = Expr::String(value.clone());
                self.advance();
                expr
            }
            Token::True => {
                self.advance();
                Expr::Boolean(true)
            }
            Token::False => {
                self.advance();
                Expr::Boolean(false)
            }
            Token::Minus => {
                self.advance();
                let expr = self.parse_primary();
                Expr::UnaryOp(Token::Minus, Box::new(expr))
            }
            Token::Not => {
                self.advance();
                let expr = self.parse_primary();
                Expr::UnaryOp(Token::Not, Box::new(expr))
            }
            Token::LeftParen => {
                self.advance();
                let expr = self.parse_expr();
                self.eat(Token::RightParen);
                expr
            }
            Token::LeftBracket => {
                self.advance();
                self.parse_vector()
            }
            _ => panic!(
                "Unexpected token in primary expression: {:?}",
                self.current_token
            ),
        }
    }

    fn parse_vector(&mut self) -> Expr {
        let mut elements = Vec::new();
        while self.current_token != Token::RightBracket {
            elements.push(self.parse_expr());
            if self.current_token == Token::Comma {
                self.advance();
            }
        }
        self.eat(Token::RightBracket);
        Expr::Vector(elements)
    }

    fn parse_vector_index(&mut self, vector_name: String) -> Expr {
        self.advance();
        let index = self.parse_expr();
        self.eat(Token::RightBracket);
        Expr::VectorIndex(Box::new(Expr::Identifier(vector_name)), Box::new(index))
    }

    fn parse_binary_op(&mut self, precedence: u8) -> Expr {
        let mut left = self.parse_primary();
        while let Some(op_prec) = self.get_precedence(&self.current_token) {
            if precedence >= op_prec {
                break;
            }
            let op = self.current_token.clone();
            self.advance();
            let right = self.parse_binary_op(op_prec);
            left = Expr::BinaryOp(Box::new(left), op, Box::new(right));
        }
        left
    }

    fn get_precedence(&self, token: &Token) -> Option<u8> {
        match token {
            Token::Or => Some(1),
            Token::And => Some(2),
            Token::Equal | Token::NotEqual => Some(3),
            Token::LessThan | Token::GreaterThan => Some(4),
            Token::Plus | Token::Minus => Some(5),
            Token::Multiply | Token::Divide => Some(6),
            _ => None,
        }
    }

    fn parse_expr(&mut self) -> Expr {
        self.parse_binary_op(0)
    }

    fn parse_function_call(&mut self, function_name: String) -> Expr {
        self.eat(Token::LeftParen);
        let mut arguments = Vec::new();
        while self.current_token != Token::RightParen {
            arguments.push(self.parse_expr());
            if self.current_token == Token::Comma {
                self.advance();
            }
        }
        self.eat(Token::RightParen);
        Expr::FunctionCall(function_name, arguments)
    }

    fn parse_function_declaration(&mut self) -> Stmt {
        self.eat(Token::Poof);
        let name = match &self.current_token {
            Token::Identifier(name) => name.clone(),
            _ => panic!("Expected function name, found {:?}", self.current_token),
        };
        self.advance();
        self.eat(Token::LeftParen);

        let mut params = Vec::new();
        while self.current_token != Token::RightParen {
            if let Token::Identifier(param) = &self.current_token {
                params.push(param.clone());
                self.advance();
                if self.current_token == Token::Comma {
                    self.advance();
                }
            } else {
                panic!("Expected parameter name, found {:?}", self.current_token);
            }
        }
        self.eat(Token::RightParen);
        self.eat(Token::LeftCurly);

        let mut body = Vec::new();
        while self.current_token != Token::RightCurly {
            body.push(self.parse_statement());
        }
        self.eat(Token::RightCurly);

        Stmt::FunctionDeclaration(name, params, body)
    }

    fn parse_assignment(&mut self) -> Stmt {
        self.eat(Token::Poo);
        let mut is_mutable = false;
        if self.current_token == Token::Mut {
            self.eat(Token::Mut);
            is_mutable = true;
        }
        let name = match &self.current_token {
            Token::Identifier(name) => name.clone(),
            _ => panic!("Expected variable name, found {:?}", self.current_token),
        };
        self.advance();
        self.eat(Token::Assignment);
        let value = self.parse_expr();
        self.eat(Token::SemiColon);
        Stmt::Assignment(name, value, is_mutable)
    }

    fn parse_reassignment(&mut self) -> Stmt {
        let name = match &self.current_token {
            Token::Identifier(name) => name.clone(),
            _ => panic!("Expected variable name, found {:?}", self.current_token),
        };
        self.advance();
        self.eat(Token::Assignment);
        let value = self.parse_expr();
        self.eat(Token::SemiColon);
        Stmt::Reassignment(name, value)
    }

    fn parse_if(&mut self) -> Stmt {
        self.eat(Token::If);
        self.eat(Token::LeftParen);
        let condition = self.parse_expr();
        self.eat(Token::RightParen);
        self.eat(Token::LeftCurly);

        let mut then_branch = Vec::new();
        while self.current_token != Token::RightCurly {
            then_branch.push(self.parse_statement());
        }
        self.eat(Token::RightCurly);

        let else_branch = if self.current_token == Token::Else {
            self.advance();
            self.eat(Token::LeftCurly);
            let mut else_body = Vec::new();
            while self.current_token != Token::RightCurly {
                else_body.push(self.parse_statement());
            }
            self.eat(Token::RightCurly);
            Some(else_body)
        } else {
            None
        };

        Stmt::If(condition, then_branch, else_branch.unwrap())
    }

    fn parse_while(&mut self) -> Stmt {
        self.eat(Token::While);
        self.eat(Token::LeftParen);
        let condition = self.parse_expr();
        self.eat(Token::RightParen);
        self.eat(Token::LeftCurly);

        let mut body = Vec::new();
        while self.current_token != Token::RightCurly {
            body.push(self.parse_statement());
        }
        self.eat(Token::RightCurly);

        Stmt::While(condition, body)
    }

    fn parse_for_in_range(&mut self) -> Stmt {
        self.eat(Token::For);
        let iterator = match &self.current_token {
            Token::Identifier(name) => name.clone(),
            _ => panic!(
                "Expected iterator variable name, found {:?}",
                self.current_token
            ),
        };
        self.advance();
        self.eat(Token::In);
        let range_expr = self.parse_expr();

        let mut step_size = Expr::Int(1);
        if self.current_token == Token::Step {
            self.eat(Token::Step);
            step_size = self.parse_expr();
        }
        self.eat(Token::LeftCurly);

        let mut body = Vec::new();
        while self.current_token != Token::RightCurly {
            body.push(self.parse_statement());
        }
        self.eat(Token::RightCurly);

        Stmt::ForRange(iterator, range_expr, body, step_size)
    }

    fn parse_return(&mut self) -> Stmt {
        self.eat(Token::Return);
        let value = self.parse_expr();
        self.eat(Token::SemiColon);
        Stmt::Return(Some(value))
    }

    fn parse_statement(&mut self) -> Stmt {
        match &self.current_token {
            Token::Poof => self.parse_function_declaration(),
            Token::If => self.parse_if(),
            Token::While => self.parse_while(),
            Token::For => self.parse_for_in_range(),
            Token::Return => self.parse_return(),
            Token::Poo => self.parse_assignment(),
            Token::Identifier(_) => {
                if self.peek_token() == Token::Assignment {
                    self.parse_reassignment()
                } else {
                    let expr = self.parse_expr();
                    self.eat(Token::SemiColon);
                    Stmt::Expression(expr)
                }
            }
            _ => panic!("Unexpected token: {:?}", self.current_token),
        }
    }

    pub fn parse(&mut self) -> Vec<Stmt> {
        let mut statements = Vec::new();
        while self.current_token != Token::EOF {
            statements.push(self.parse_statement());
        }
        statements
    }
}
