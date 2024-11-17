// src/parser.rs
use crate::lexer::{Lexer, Token};
use crate::ast::{Expr, Stmt};

pub struct Parser {
    lexer: Lexer,
    current_token: Token,
}

impl Parser {
    pub fn new(lexer: Lexer) -> Self {
        let mut parser = Parser {
            lexer,
            current_token: Token::EOF,
        };
        parser.advance();  // Load the first token
        parser
    }

    // Advance the lexer and update the current token
    fn advance(&mut self) {
        self.current_token = self.lexer.next_token();
    }

    // Check the current token and advance if it matches the expected one
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

    // Parse primary expressions (numbers, identifiers, or parenthesized expressions)
    fn parse_primary(&mut self) -> Expr {
        match &self.current_token {
            Token::Number(value) => {
                let expr = Expr::Number(*value);
                self.advance();
                expr
            }
            Token::Identifier(name) => {
                let expr = Expr::Identifier(name.clone());
                self.advance();
                expr
            }
            Token::LeftParen => {
                self.advance();  // Consume '('
                let expr = self.parse_expr();
                self.eat(Token::RightParen);  // Consume ')'
                expr
            }
            Token::Minus => {
                self.advance();
                let expr = self.parse_primary();
                Expr::UnaryOp(Token::Minus, Box::new(expr))
            }
            _ => panic!("Unexpected token in primary expression: {:?}", self.current_token),
        }
    }

    // Parse binary operations with precedence
    fn parse_binary_op(&mut self, precedence: u8) -> Expr {
        let mut left = self.parse_primary();
        println!("Initial Left Expr: {:?}", left);
    
        while let Some(op_prec) = self.get_precedence(&self.current_token) {
            println!("Current Precedence (Left, Current): {:?}, Token: {:?}", (precedence, op_prec), self.current_token);
    
            // Break if current token's precedence is less than or equal to the current precedence
            if precedence >= op_prec {
                break;
            }
    
            // Capture the operator and advance
            let op = self.current_token.clone();
            self.advance();
    
            // Recursively parse the right-hand side expression with higher precedence
            let right = self.parse_binary_op(op_prec);
    
            // Construct a binary operation node with left and right expressions
            left = Expr::BinaryOp(Box::new(left), op, Box::new(right));
        }
        left
    }
    

    // Get operator precedence
    fn get_precedence(&self, token: &Token) -> Option<u8> {
        match token {
            Token::Plus | Token::Minus => Some(1),
            Token::Multiply | Token::Divide => Some(2),
            _ => None,
        }
    }

    // Parse general expressions
    fn parse_expr(&mut self) -> Expr {
        self.parse_binary_op(0)
    }

    // Parse assignment statements
    fn parse_assignment(&mut self) -> Stmt {
        self.eat(Token::Poo);
        if let Token::Identifier(var_name) = &self.current_token {
            let identifier = var_name.clone();
            self.advance();
            self.eat(Token::Assignment);
            let expr = self.parse_expr();
            self.eat(Token::SemiColon);
            Stmt::Assignment(identifier, expr)
        } else {
            panic!("Expected an identifier after 'poo'");
        }
    }

    // Parse if statements
    fn parse_if(&mut self) -> Stmt {
        self.eat(Token::If);
        let condition = self.parse_expr();
        self.eat(Token::OpenScope);

        let mut if_body = Vec::new();
        while self.current_token != Token::EOF && self.current_token != Token::Else {
            if_body.push(self.parse_statement());
        }

        let mut else_body = Vec::new();
        if self.current_token == Token::Else {
            self.eat(Token::Else);
            self.eat(Token::OpenScope);
            while self.current_token != Token::EOF {
                else_body.push(self.parse_statement());
            }
        }

        Stmt::If(condition, if_body, else_body)
    }

    // Parse while loops
    fn parse_while(&mut self) -> Stmt {
        self.eat(Token::While);
        let condition = self.parse_expr();
        self.eat(Token::OpenScope);

        let mut body = Vec::new();
        while self.current_token != Token::EOF {
            body.push(self.parse_statement());
        }

        Stmt::While(condition, body)
    }

    // Parse a return statement
    fn parse_return(&mut self) -> Stmt {
        self.eat(Token::Return);
        let expr = self.parse_expr();
        self.eat(Token::SemiColon);
        Stmt::Return(expr)
    }

    // Parse any statement
    fn parse_statement(&mut self) -> Stmt {
        match &self.current_token {
            Token::If => self.parse_if(),
            Token::While => self.parse_while(),
            Token::Return => self.parse_return(),
            Token::Poo => self.parse_assignment(),
            _ => {
                let expr = self.parse_expr();
                self.eat(Token::SemiColon);
                Stmt::Expression(expr)
            }
        }
    }

    // Parse a list of statements
    pub fn parse(&mut self) -> Vec<Stmt> {
        let mut statements = Vec::new();
        while self.current_token != Token::EOF {
            statements.push(self.parse_statement());
        }
        statements
    }
}
