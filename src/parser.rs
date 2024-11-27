use std::collections::HashMap;

// src/parser.rs
use crate::lexer::{Lexer, Token};
use crate::ast::{Type, Expr, Stmt};

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

    fn peek_token(&mut self) -> Token {
        self.lexer.peek_next_token()
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

    // fn error(&self, message: &str) {
    //     eprintln!("Parse error: {} at {:?}", message, self.current_token);
    // }    

    // Parse primary expressions (numbers, identifiers, or parenthesized expressions)
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
                if self.current_token == Token::LeftParen {
                    self.parse_function_call(identifier) // Function call
                } else {
                    Expr::Identifier(identifier) // Variable or other identifier
                }
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
            Token::Not => {
                self.advance();
                let expr = self.parse_primary();
                Expr::UnaryOp(Token::Not, Box::new(expr))
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
            Token::Or => Some(1),
            Token::And => Some(2),
            Token::Equal | Token::NotEqual => Some(3),
            Token::LessThan | Token::GreaterThan => Some(4),
            Token::Plus | Token::Minus => Some(5),
            Token::Multiply | Token::Divide => Some(6),
            _ => None,
        }
    }

    // Parse general expressions
    fn parse_expr(&mut self) -> Expr {
        self.parse_binary_op(0)
    }

    // Parse assignment statements
    fn parse_assignment(&mut self) -> Stmt {
        let mut is_mutable: bool = false;
        self.eat(Token::Poo);
        if let Token::Mut = &self.current_token {
            is_mutable = true;
            self.advance();
        }
        if let Token::Identifier(var_name) = &self.current_token {
            let identifier = var_name.clone();
            self.advance();
            self.eat(Token::Assignment);
            let expr = self.parse_expr();
            self.eat(Token::SemiColon);
            Stmt::Assignment(identifier, expr, is_mutable)
        } else {
            panic!("Expected an identifier after 'poo'");
        }
    }

    // Parse reassignment statements
    fn parse_reassignment(&mut self) -> Stmt {
        if let Token::Identifier(var_name) = &self.current_token {
            let identifier = var_name.clone();
            self.advance();
            self.eat(Token::Assignment);
            let expr = self.parse_expr();
            self.eat(Token::SemiColon);
            Stmt::Reassignment(identifier, expr)
        } else {
            panic!("Expected an identifier for reassignment");
        }
    }

    fn parse_function_declaration(&mut self) -> Stmt {
        self.eat(Token::Poof); // Consume 'fun'
    
        // Parse function name
        let function_name = if let Token::Identifier(name) = &self.current_token {
            name.clone()
        } else {
            panic!("Expected function name after 'fun'");
        };
        self.advance();
    
        // Parse parameters
        self.eat(Token::LeftParen); // Consume '('
        let mut parameters = Vec::new();
        while self.current_token != Token::RightParen {
            if let Token::Identifier(param) = &self.current_token {
                parameters.push(param.clone());
                self.advance();
                if self.current_token == Token::Comma {
                    self.advance(); // Consume ','
                }
            } else {
                panic!("Expected parameter name in function declaration");
            }
        }
        self.eat(Token::RightParen); // Consume ')'
    
        self.eat(Token::RightArrow); // Consume '>>'

        let return_type = match &self.current_token {
            Token::TVoid => Type::Void,
            Token::TBool => Type::Bool,
            Token::TInt => Type::Int,
            Token::TFloat => Type::Float,
            Token::TString => Type::String,
            _ => panic!("Expected return type after '->'"),
        };
        
        self.advance();
        self.eat(Token::LeftCurly); // Consume '{'
    
        // Parse the function body
        let mut body = Vec::new();
        while self.current_token != Token::RightCurly {
            body.push(self.parse_statement());
        }
        let is_returning = &body.iter().any(|s| match s {
            Stmt::Return(_) => true,
            _ => false
        });
        if !is_returning {
            body.push(Stmt::Return(None))
        }
        
        self.eat(Token::RightCurly); // Consume '}'

        fn validate_function_return_type(body: &[Stmt], expected_type: &Type, symbol_table: &mut HashMap<String, Type>) {
            for stmt in body {
                match stmt {
                    Stmt::Assignment(var_name, expr, _) => {
                        // Infer the type of the expression being assigned
                        let expr_type = infer_expr_type(expr, symbol_table);
                        symbol_table.insert(var_name.clone(), expr_type);
                    }
                    Stmt::Reassignment(var_name, expr) => {
                        // Ensure reassignment matches the declared type
                        if let Some(var_type) = symbol_table.get(var_name) {
                            let expr_type = infer_expr_type(expr, symbol_table);
                            if var_type != &expr_type {
                                panic!(
                                    "Type mismatch in reassignment: expected {:?}, found {:?} for variable {}",
                                    var_type, expr_type, var_name
                                );
                            }
                        } else {
                            panic!("Variable {} used before declaration", var_name);
                        }
                    }
                    Stmt::Return(Some(expr)) => {
                        let return_type = infer_expr_type(expr, symbol_table);
                        if &return_type != expected_type {
                            panic!(
                                "Return type mismatch: expected {:?}, but found {:?}",
                                expected_type, return_type
                            );
                        }
                    }
                    Stmt::Return(None) => {
                        if *expected_type != Type::Void {
                            panic!(
                                "Expected return type {:?}, but function returned nothing",
                                expected_type
                            );
                        }
                    }
                    _ => {}
                }
            }
        }
        
        
        // Helper to infer the type of an expression
        fn infer_expr_type(expr: &Expr, symbol_table: &HashMap<String, Type>) -> Type {
            match expr {
                Expr::Int(_) => Type::Int,
                Expr::Float(_) => Type::Float,
                Expr::String(_) => Type::String,
                Expr::Boolean(_) => Type::Bool,
                Expr::Identifier(name) => {
                    symbol_table.get(name).cloned().expect(&format!(
                        "Undefined variable or identifier: {}",
                        name
                    ))
                }
                Expr::FunctionCall(_, _) => todo!("Handle function call return type"),
                _ => panic!("Unexpected expression type: {:?}", expr),
            }
        }
        
    
        let mut symbol_table = HashMap::new();
        validate_function_return_type(&body, &return_type, &mut symbol_table);
        Stmt::FunctionDeclaration(function_name, parameters, body, return_type)
    }

    fn parse_function_call(&mut self, function_name: String) -> Expr {
        self.eat(Token::LeftParen); // Consume '('
    
        let mut arguments = Vec::new();
        while self.current_token != Token::RightParen {
            arguments.push(self.parse_expr());
            if self.current_token == Token::Comma {
                self.advance(); // Consume ','
            }
        }
        self.eat(Token::RightParen); // Consume ')'
    
        Expr::FunctionCall(function_name, arguments)
    }
    

    // Parse if statements
    fn parse_if(&mut self) -> Stmt {
        self.eat(Token::If);
        let condition = self.parse_expr();
        self.eat(Token::LeftCurly);

        let mut if_body = Vec::new();
        while self.current_token != Token::EOF && self.current_token != Token::RightCurly {
            if_body.push(self.parse_statement());
        }

        self.eat(Token::RightCurly);

        let mut else_body = Vec::new();
        if self.current_token == Token::Else {
            self.eat(Token::Else);
            self.eat(Token::LeftCurly);
            while self.current_token != Token::EOF && self.current_token != Token::RightCurly {
                else_body.push(self.parse_statement());
            }
            self.eat(Token::RightCurly);
        }

        Stmt::If(condition, if_body, else_body)
    }

    // Parse while loops
    fn parse_while(&mut self) -> Stmt {
        self.eat(Token::While);
        let condition = self.parse_expr();
        self.eat(Token::LeftCurly);

        let mut body = Vec::new();
        while self.current_token != Token::EOF && self.current_token != Token::RightCurly {
            body.push(self.parse_statement());
        }
        self.eat(Token::RightCurly);

        Stmt::While(condition, body)
    }

    fn parse_for_in_range(&mut self) -> Stmt {
        self.eat(Token::For); // Consume 'for'
        
        // Ensure we have an identifier for the loop variable
        let iter_identifier = if let Token::Identifier(var_name) = &self.current_token {
            var_name.clone()
        } else {
            panic!("Expected an identifier as the loop variable in 'for' statement");
        };
        self.advance(); // Move past the loop variable
        
        self.eat(Token::In); // Consume 'in'
        
        // Parse the range expressions
        let from = self.parse_expr(); // Parse start of the range
        self.eat(Token::DoubleDot); // Consume '..'
        let to = self.parse_expr(); // Parse end of the range
        
        self.eat(Token::LeftCurly); // Consume '{'
        
        // Parse the body of the loop
        let mut body = Vec::new();
        while self.current_token != Token::EOF && self.current_token != Token::RightCurly {
            body.push(self.parse_statement());
        }
        self.eat(Token::RightCurly); // Consume '}'
        
        Stmt::For(iter_identifier, from, to, body)
    }
    
    fn parse_use(&mut self) -> Stmt {
        self.eat(Token::Use); // Consume 'use'
        let mut module_path = String::new();
    
        // Parse module paths (e.g., std::cout)
        while let Token::Identifier(part) = &self.current_token {
            module_path.push_str(part);
            self.advance();
    
            if self.current_token == Token::DoubleColon {
                module_path.push_str("::");
                self.advance();
            } else {
                break;
            }
        }
    
        self.eat(Token::SemiColon); // Consume ';'
        Stmt::Use(module_path)
    }
    

    // Parse a return statement
    fn parse_return(&mut self) -> Stmt {
        self.eat(Token::Return);
    
        let expr = if self.current_token != Token::SemiColon {
            Some(self.parse_expr())
        } else {
            None
        };
    
        self.eat(Token::SemiColon);
        Stmt::Return(expr)
    }    
    

    // Parse any statement
    fn parse_statement(&mut self) -> Stmt {
        match &self.current_token {
            Token::Use => self.parse_use(),
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
