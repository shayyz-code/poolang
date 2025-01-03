use std::collections::HashMap;

// src/parser.rs
use crate::ast::{Expr, Property, Stmt, Type};
use crate::lexer::{Lexer, Token};
use crate::type_inference::*;
use crate::visitor::ScopedSymbolTable;

pub struct Parser {
    lexer: Lexer,
    current_token: Token,
    defined_types: HashMap<String, Type>,
    current_struct: Option<String>,
}

impl Parser {
    pub fn new(lexer: Lexer) -> Self {
        let mut parser = Parser {
            lexer,
            current_token: Token::EOF,
            defined_types: HashMap::new(),
            current_struct: None,
        };
        parser.advance(); // Load the first token
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
    pub fn infer_token_to_type(&mut self) -> Type {
        match &self.current_token {
            Token::TBool => Type::Bool,
            Token::TInt => Type::Int,
            Token::TFloat => Type::Float,
            Token::TVoid => Type::Void,
            Token::TChar => Type::Char,
            Token::TString => Type::String,
            Token::TVec => {
                self.advance();
                self.eat(Token::Colon);
                let vec_type = self.infer_token_to_type();
                if !matches!(vec_type, Type::Vector(_)) {
                    self.advance();
                }
                Type::Vector(Box::new(vec_type))
            }
            Token::TSelf => {
                if let Some(struct_name) = &self.current_struct {
                    if let Some(t) = self.defined_types.get(struct_name) {
                        t.clone()
                    } else {
                        panic!("Undefined type from struct: '{}'", struct_name);
                    }
                } else {
                    panic!("Current struct not found");
                }
            }
            Token::TMap => Type::Map(HashMap::new()),
            Token::Identifier(struct_name) => {
                if let Some(t) = self.defined_types.get(struct_name) {
                    t.clone()
                } else {
                    panic!("Undefined type from struct: '{}'", struct_name);
                }
            }
            _ => panic!("Expected a datatype instead of {:?}", self.current_token),
        }
    }

    // Parse primary expressions (numbers, identifiers, or parenthesized expressions)
    fn parse_primary(&mut self) -> Expr {
        let mut expr = match &self.current_token {
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
                } else if self.current_token == Token::LeftBracket {
                    self.parse_vector_index(identifier)
                } else if self.current_token == Token::DoubleColon {
                    self.advance();
                    if let Token::LeftCurly = &self.current_token {
                        self.parse_struct_compound(identifier)
                    } else {
                        panic!("Not implemented yet");
                    }
                } else {
                    Expr::Identifier(identifier) // Variable or other identifier
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
            Token::Not => {
                self.advance();
                let expr = self.parse_primary();
                Expr::UnaryOp(Token::Not, Box::new(expr))
            }
            Token::LeftParen => {
                self.advance(); // Consume '('
                let expr = self.parse_expr();
                self.eat(Token::RightParen); // Consume ')'
                expr
            }
            Token::LeftBracket => self.parse_vector(),
            Token::LeftCurly => self.parse_map(),
            Token::Minus => {
                self.advance();
                let expr = self.parse_primary();
                Expr::UnaryOp(Token::Minus, Box::new(expr))
            }

            _ => panic!(
                "Unexpected token in primary expression: {:?}",
                self.current_token
            ),
        };

        while self.current_token == Token::Dot {
            self.advance(); // Consume '.'
            if let Token::Identifier(method_name) = &self.current_token {
                let method = method_name.clone();
                self.advance(); // Consume method name
                let mut args = Vec::new();
                if self.current_token == Token::LeftParen {
                    self.advance(); // Consume '('
                    while self.current_token != Token::RightParen {
                        args.push(self.parse_expr());
                        if self.current_token == Token::Comma {
                            self.advance();
                        }
                    }
                    self.eat(Token::RightParen); // Consume ')'
                    expr = Expr::MethodCall(Box::new(expr), method, args);
                } else {
                    expr = Expr::MapKey(Box::new(expr), method);
                }
            } else {
                panic!("Expected method name after '.'");
            }
        }

        expr
    }

    fn parse_struct_compound(&mut self, struct_name: String) -> Expr {
        self.eat(Token::LeftCurly);
        let mut props_exprs = HashMap::new();
        while &self.current_token != &Token::RightCurly {
            if let Token::Identifier(key) = self.current_token.clone() {
                self.advance();
                self.eat(Token::Colon);
                let val_expr = self.parse_expr();
                props_exprs.insert(key, val_expr);
                if let Token::Comma = &self.current_token {
                    self.eat(Token::Comma);
                }
            }
        }
        self.eat(Token::RightCurly);
        Expr::StructCompound(struct_name, props_exprs)
    }

    fn parse_vector(&mut self) -> Expr {
        self.advance(); // Consume '['
        let mut elements = Vec::new();
        let mut extensor: Option<Box<Expr>> = None;
        while self.current_token != Token::RightBracket {
            elements.push(self.parse_expr());
            if self.current_token == Token::RightArrow {
                self.advance();
                extensor = Some(Box::new(self.parse_expr()));
                break;
            }
            if self.current_token == Token::Comma {
                self.advance();
            }
        }
        self.eat(Token::RightBracket);
        Expr::Vector(elements, extensor)
    }

    fn parse_map(&mut self) -> Expr {
        self.advance(); // Consume '{'
        let mut map = HashMap::new();
        while self.current_token != Token::RightCurly {
            let mut key = String::new();

            if let Token::Identifier(name) = &self.current_token {
                key = name.clone();
                self.advance();
            }

            self.eat(Token::Colon);
            let value = self.parse_expr();
            if self.current_token == Token::Comma {
                self.advance(); // Consume ','
            }
            map.insert(key, value);
        }
        self.eat(Token::RightCurly);
        Expr::Map(map)
    }

    fn parse_vector_index(&mut self, vector_name: String) -> Expr {
        let mut vec_of_indices = Vec::new();
        while let Token::LeftBracket = self.current_token {
            self.advance();
            let index = self.parse_primary();
            self.eat(Token::RightBracket);
            vec_of_indices.push(index);
        }

        Expr::VectorIndex(Box::new(Expr::Identifier(vector_name)), vec_of_indices)
    }

    // Parse binary operations with precedence
    fn parse_binary_op(&mut self, precedence: u8) -> Expr {
        let mut left = self.parse_primary();

        while let Some(op_prec) = self.get_precedence(&self.current_token) {
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
        let precedence_table = [
            (Token::Otherwise, 1),
            (Token::Or, 1),
            (Token::And, 2),
            (Token::Equal, 3),
            (Token::NotEqual, 3),
            (Token::LessThan, 4),
            (Token::GreaterThan, 4),
            (Token::Plus, 5),
            (Token::Minus, 5),
            (Token::Multiply, 6),
            (Token::Divide, 6),
        ];
        precedence_table
            .iter()
            .find_map(|(t, p)| if t == token { Some(*p) } else { None })
    }

    // Parse general expressions
    fn parse_expr(&mut self) -> Expr {
        self.parse_binary_op(0)
    }

    // Parse assignment statements
    fn parse_assignment(&mut self) -> Stmt {
        let mut is_mutable: bool = false;
        let mut explicit_var_type: Option<Type> = None;
        if let Token::Mut = &self.current_token {
            is_mutable = true;
        } else if let Token::Poo = &self.current_token {
            is_mutable = false;
        }
        self.advance(); // consume poo | mut
        if let Token::Identifier(var_name) = &self.current_token {
            let identifier = var_name.clone();
            self.advance();
            if self.current_token != Token::ShortAssignment {
                explicit_var_type = Some(self.infer_token_to_type());
                if !matches!(explicit_var_type, Some(Type::Vector(_))) {
                    self.advance(); // Move past the type if not vector because vector type has already advanced
                }
                self.eat(Token::Assignment);
            } else {
                self.eat(Token::ShortAssignment);
            }
            let expr = self.parse_expr();
            self.eat(Token::SemiColon);
            Stmt::Assignment(identifier, expr, is_mutable, explicit_var_type)
        } else {
            panic!("Expected an identifier after 'poo' or 'mut'");
        }
    }

    // Parse reassignment statements
    fn parse_reassignment(&mut self) -> Stmt {
        if let Token::Identifier(var_name) = &self.current_token {
            let identifier = var_name.clone();
            let mut vec_item_iden: Option<Expr> = None;
            self.advance();
            if self.current_token == Token::LeftBracket {
                vec_item_iden = Some(self.parse_vector_index(identifier.clone()));
            }
            self.eat(Token::Assignment);
            let expr = self.parse_expr();
            self.eat(Token::SemiColon);
            if let Some(v) = vec_item_iden {
                Stmt::Reassignment(identifier, Some(v), expr)
            } else {
                Stmt::Reassignment(identifier, None, expr)
            }
        } else {
            panic!("Expected an identifier for reassignment");
        }
    }

    fn parse_function_declaration(&mut self) -> Stmt {
        self.eat(Token::Poof); // Consume 'poof'

        // Parse function name
        let function_name = if let Token::Identifier(name) = &self.current_token {
            name.clone()
        } else {
            panic!("Expected function name after 'poof'");
        };
        self.advance();

        // if self.current_token == Token::LessThan {
        //     self.advance(); // Consume '<'
        //     let mut generics = Vec::new();
        //     while self.current_token != Token::GreaterThan {
        //         if let Token::Identifier(name) = &self.current_token {
        //             generics.push(name.clone());
        //         }
        //         self.advance();
        //         if self.current_token == Token::Comma {
        //             self.advance();
        //         }
        //     }
        //     self.eat(Token::GreaterThan); // Consume '>'
        // }

        // Parse parameters
        self.eat(Token::LeftParen); // Consume '('
        let mut parameters = Vec::new();
        while self.current_token != Token::RightParen {
            // Parse parameter name
            let param_name = if let Token::Identifier(name) = &self.current_token {
                name.clone()
            } else {
                panic!("Expected parameter name in function declaration");
            };
            self.advance(); // Move past the parameter name

            // Parse type annotation
            let param_type = self.infer_token_to_type();
            if !matches!(param_type, Type::Vector(_)) {
                self.advance(); // Move past the type if not vector because vector type has already advanced
            }

            parameters.push((param_name, param_type)); // Store the parameter and its type

            if self.current_token == Token::Comma {
                self.advance(); // Consume ',' if more parameters
            }
        }
        self.eat(Token::RightParen); // Consume ')'

        let return_type: Type;
        if self.current_token != Token::RightArrow {
            return_type = Type::Void;
        } else {
            self.eat(Token::RightArrow);
            // Consume '>>'

            return_type = self.infer_token_to_type();
            if !matches!(return_type, Type::Vector(_)) {
                self.advance(); // Move past the type if not vector because vector type has already advanced
            }
        }

        self.eat(Token::LeftCurly); // Consume '{'

        // Parse the function body
        let mut body = Vec::new();
        while self.current_token != Token::RightCurly {
            body.push(self.parse_statement());
        }
        let is_returning = &body.iter().any(|s| match s {
            Stmt::Return(_) => true,
            _ => false,
        });
        if !is_returning {
            body.push(Stmt::Return(None))
        }

        self.eat(Token::RightCurly); // Consume '}'

        fn validate_function_return_type(
            body: &[Stmt],
            expected_type: &Type,
            symbol_table: &mut ScopedSymbolTable,
        ) {
            symbol_table.enter_scope(); // New scope for the function body

            for stmt in body {
                match stmt {
                    Stmt::Assignment(var_name, expr, _, _) => {
                        let expr_type = infer_expr_type(expr, symbol_table);
                        symbol_table.insert(var_name.clone(), expr_type);
                    }
                    Stmt::Reassignment(var_name, _, expr) => {
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

            symbol_table.exit_scope(); // Exit the function body scope
        }

        let mut symbol_table = ScopedSymbolTable::new();

        // Add parameters to the symbol table
        for (param_name, param_type) in &parameters {
            symbol_table.insert(param_name.clone(), param_type.clone());
        }
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
        self.advance(); // cannot use self.eat (if && selif)
        let condition = self.parse_expr();
        self.eat(Token::LeftCurly);

        let mut if_body = Vec::new();
        while self.current_token != Token::EOF && self.current_token != Token::RightCurly {
            if_body.push(self.parse_statement());
        }
        self.eat(Token::RightCurly);

        let mut else_body = Vec::new();
        if self.current_token == Token::Else || self.current_token == Token::Elif {
            if self.current_token == Token::Elif {
                // Parse `else if` as a nested `If` statement
                else_body.push(self.parse_if());
            } else {
                self.eat(Token::Else);
                self.eat(Token::LeftCurly);
                while self.current_token != Token::EOF && self.current_token != Token::RightCurly {
                    else_body.push(self.parse_statement());
                }
                self.eat(Token::RightCurly);
            }
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
        // Parse the iterable (can be a range or vector)
        let iterable = self.parse_expr();

        // Check if it's a range (by looking for a DoubleDot) or a vector
        let (from, to, is_range) = if self.current_token == Token::DoubleDot {
            self.eat(Token::DoubleDot);
            let to = self.parse_expr();
            (Some(iterable), Some(to), true) // It's a range
        } else {
            (Some(iterable), None, false) // It's a vector or other iterable
        };

        let step = if self.current_token == Token::Step {
            self.advance();
            let step_size = self.parse_expr();
            step_size
        } else {
            Expr::Int(1)
        };

        self.eat(Token::LeftCurly); // Consume '{'

        // Parse the body of the loop
        let mut body = Vec::new();
        while self.current_token != Token::EOF && self.current_token != Token::RightCurly {
            body.push(self.parse_statement());
        }
        self.eat(Token::RightCurly); // Consume '}'

        if is_range {
            Stmt::ForRange(iter_identifier, from.unwrap(), to.unwrap(), step, body)
        } else {
            Stmt::ForVector(iter_identifier, from.unwrap(), body)
        }
    }

    fn parse_use(&mut self) -> Stmt {
        self.eat(Token::Use); // Consume 'use'
        let mut modules = Vec::new();
        // Parse module paths (e.g., std::cout)
        while self.current_token != Token::SemiColon {
            if let Token::Identifier(part) = &self.current_token {
                let mut module_path = String::new();
                module_path.push_str(part);
                self.advance();

                if self.current_token == Token::DoubleColon {
                    module_path.push_str("::");
                    self.advance();
                    if self.current_token == Token::LeftParen {
                        self.advance();
                        while let Token::Identifier(sub_part) = &self.current_token {
                            modules.push(module_path.to_owned() + sub_part);
                            self.advance();
                            if self.current_token == Token::Comma {
                                self.advance();
                            } else {
                                self.eat(Token::RightParen);
                                break;
                            }
                        }
                    } else if let Token::Identifier(_) = &self.current_token {
                        let mut final_module_part = String::new();
                        while let Token::Identifier(sub_part) = &self.current_token {
                            final_module_part.push_str(&sub_part);
                            self.advance();
                            if let Token::DoubleColon = &self.current_token {
                                final_module_part.push_str("::");
                                self.eat(Token::DoubleColon);
                            }
                        }
                        modules.push(module_path.to_owned() + final_module_part.as_str());
                    }
                }
            } else {
                break;
            }
        }

        self.eat(Token::SemiColon); // Consume ';'
        println!("{:?}", modules);
        Stmt::Use(modules)
    }

    fn parse_struct(&mut self) -> Stmt {
        self.eat(Token::Struct);

        let struct_name = if let Token::Identifier(struct_name) = &self.current_token {
            struct_name.to_owned()
        } else {
            panic!("Expected Identifier after struct keyword");
        };
        // update current struct
        self.current_struct = Some(struct_name.clone());

        self.advance();

        let mut inherit_names = Vec::new();
        if let Token::Inherits = &self.current_token {
            self.eat(Token::Inherits);
            while let Token::Identifier(inherit_name) = &self.current_token {
                inherit_names.push(inherit_name.clone());
                self.advance();
                if let Token::Comma = &self.current_token {
                    self.advance();
                }
            }
        }

        self.eat(Token::LeftCurly);

        let mut struct_properties = Vec::new();
        let mut struct_types = HashMap::new();

        while matches!(&self.current_token, Token::Pub | Token::Identifier(_)) {
            let property_access = if let Token::Pub = &self.current_token {
                self.advance();
                "public".to_owned()
            } else {
                "private".to_owned()
            };

            let property_name = if let Token::Identifier(property_name) = &self.current_token {
                property_name.to_owned()
            } else {
                panic!("Expected an Identifier for property name");
            };
            self.advance();

            let property_type = &self.infer_token_to_type();
            self.advance();
            struct_types.insert(property_name.clone(), property_type.clone()); // update defined types
            struct_properties.push(Property::new(
                property_name,
                property_type.clone(),
                property_access,
            ));
        }

        // update defined types
        self.defined_types
            .insert(struct_name.clone(), Type::Map(struct_types));

        let mut implements = HashMap::new();
        while let Token::Implements = &self.current_token {
            self.eat(Token::Implements);
            let impl_target = if let Token::Identifier(target_name) = &self.current_token {
                let name = target_name.clone();
                self.advance();
                name
            } else {
                "Self".to_string()
            };
            self.eat(Token::LeftCurly);
            let mut impl_methods = Vec::new();
            while &self.current_token != &Token::RightCurly {
                let impl_method = self.parse_function_declaration();
                impl_methods.push(impl_method);
            }
            implements.insert(impl_target, impl_methods);
            self.eat(Token::RightCurly);
        }

        self.eat(Token::RightCurly);

        // println!(
        //     "{:?}",
        //     Stmt::Struct(
        //         struct_name.clone(),
        //         inherit_names.clone(),
        //         struct_properties.clone(),
        //         implements.clone()
        //     )
        // );

        // update current struct
        self.current_struct = None;
        Stmt::Struct(struct_name, inherit_names, struct_properties, implements)
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
            Token::Struct => self.parse_struct(),
            Token::Poof => self.parse_function_declaration(),
            Token::If => self.parse_if(),
            Token::While => self.parse_while(),
            Token::For => self.parse_for_in_range(),
            Token::Return => self.parse_return(),
            Token::Poo | Token::Mut => self.parse_assignment(),
            Token::Identifier(_) => {
                if self.peek_token() == Token::Assignment || self.peek_token() == Token::LeftBracket
                // vector index reassingment
                {
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
