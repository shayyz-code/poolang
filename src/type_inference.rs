use crate::ast::{Expr, Stmt, Type};
use crate::lexer::Token;
use crate::visitor::ScopedSymbolTable;

pub fn infer_expr_type(expr: &Expr, symbol_table: &ScopedSymbolTable) -> Type {
    match expr {
        Expr::Null => Type::Void,
        Expr::Int(_) => Type::Int,
        Expr::Float(_) => Type::Float,
        Expr::Char(_) => Type::Char,
        Expr::String(_) => Type::String,
        Expr::Boolean(_) => Type::Bool,
        Expr::Vector(v, _) => {
            if let Some(a) = v.get(0) {
                infer_expr_type(a, symbol_table)
            } else {
                Type::Vector(Box::new(Type::Void))
            }
        }
        Expr::Map(_) => Type::Map,
        Expr::MapKey(_, _) => Type::String,
        Expr::Identifier(name) => symbol_table
            .get(name)
            .cloned()
            .expect(&format!("Undefined variable: {}", name)),
        Expr::UnaryOp(_, _) => Type::Bool,
        Expr::BinaryOp(lhs, op, rhs) => {
            let lhs_type = infer_expr_type(lhs, symbol_table);
            let rhs_type = infer_expr_type(rhs, symbol_table);

            // Example: Add logic for binary operators
            match (lhs_type, rhs_type, op) {
                (Type::Int, Type::Int, Token::Plus) => Type::Int,
                (Type::Int, Type::Int, Token::Minus) => Type::Int,
                (Type::Int, Type::Int, Token::Multiply) => Type::Int,
                (Type::Int, Type::Int, Token::Divide) => Type::Int,
                (Type::Float, Type::Float, Token::Plus) => Type::Float,
                (Type::Float, Type::Float, Token::Minus) => Type::Float,
                (Type::Float, Type::Float, Token::Multiply) => Type::Float,
                (Type::Float, Type::Float, Token::Divide) => Type::Float,
                (_, _, Token::Equal) => Type::Bool,
                (_, _, Token::NotEqual) => Type::Bool,
                (_, _, Token::And) => Type::Bool,
                (_, _, Token::Or) => Type::Bool,
                _ => panic!("Unsupported binary operation: {:?} {:?} {:?}", lhs, op, rhs),
            }
        }
        Expr::MethodCall(caller, method_name, args) => {
            // Infer the type of the object
            let caller_type = infer_expr_type(caller, symbol_table);

            // Ensure the object has fields and methods (i.e., it's an Object type)

            if let Some(methods) = caller_type.clone().get_methods() {
                // Check if the method exists in the object's fields and methods
                if let Some(method_type) = methods.get(method_name) {
                    if let Type::Function(param_types, return_type) = method_type {
                        // Validate argument types
                        if param_types.len() != args.len() {
                            panic!(
                                "Method '{}' expects {} arguments, but {} were provided",
                                method_name,
                                param_types.len(),
                                args.len()
                            );
                        }

                        for (param, arg) in param_types.iter().zip(args.iter()) {
                            let arg_type = infer_expr_type(arg, symbol_table);
                            if param != &arg_type {
                                panic!(
                                    "Type mismatch in argument for method '{}': expected {:?}, got {:?}",
                                    method_name, param, arg_type
                                );
                            }
                        }

                        // Return the inferred return type of the method
                        *return_type.clone()
                    } else {
                        panic!(
                            "'{}' of method type {:?} is not a callable method",
                            method_name, method_type
                        );
                    }
                } else {
                    panic!(
                        "Undefined method '{}' for object of type {:?}",
                        method_name, caller_type
                    );
                }
            } else {
                panic!(
                    "'{:?}' is not an object type that supports method calls",
                    (caller, caller_type)
                );
            }
        }

        Expr::FunctionCall(name, args) => {
            let func_type = symbol_table
                .get(name)
                .cloned()
                .expect(&format!("Undefined function: {}", name));

            match func_type {
                Type::Function(params, return_type) => {
                    if params.len() != args.len() {
                        panic!(
                            "Function {} expects {} arguments, but {} were provided",
                            name,
                            params.len(),
                            args.len()
                        );
                    }

                    for (param, arg) in params.iter().zip(args.iter()) {
                        let arg_type = infer_expr_type(arg, symbol_table);
                        if param != &arg_type {
                            panic!(
                                    "Type mismatch in argument for function {}: expected {:?}, got {:?}",
                                    name, param, arg_type
                                );
                        }
                    }

                    *return_type
                }
                _ => panic!("{} is not callable", name),
            }
        }
        _ => panic!("Unexpected expression: {:?}", expr),
    }
}

pub fn infer_stmt_types(stmt: &Stmt, symbol_table: &mut ScopedSymbolTable) {
    match stmt {
        Stmt::Assignment(name, expr, _, _) => {
            let expr_type = infer_expr_type(expr, symbol_table);
            symbol_table.insert(name.clone(), expr_type);
        }
        Stmt::Reassignment(name, None, expr) => {
            let expr_type = infer_expr_type(expr, symbol_table);
            let var_type = symbol_table
                .get(name)
                .expect(&format!("Variable '{}' used before declaration", name));

            if var_type != &expr_type {
                panic!(
                    "Type mismatch in reassignment of '{}': expected {:?}, got {:?}",
                    name, var_type, expr_type
                );
            }
        }
        Stmt::If(cond, body, else_body) => {
            let cond_type = infer_expr_type(cond, symbol_table);
            if cond_type != Type::Bool {
                panic!("Condition in if statement must be a boolean");
            }

            symbol_table.enter_scope();
            for stmt in body {
                infer_stmt_types(stmt, symbol_table);
            }
            symbol_table.exit_scope();

            symbol_table.enter_scope();
            for stmt in else_body {
                infer_stmt_types(stmt, symbol_table);
            }
            symbol_table.exit_scope();
        }
        Stmt::FunctionDeclaration(name, params, body, return_type) => {
            let param_types = params.iter().map(|(_, ty)| ty.clone()).collect::<Vec<_>>();
            symbol_table.insert(
                name.clone(),
                Type::Function(param_types, Box::new(return_type.clone())),
            );

            symbol_table.enter_scope();
            for (param_name, param_type) in params {
                symbol_table.insert(param_name.clone(), param_type.clone());
            }

            for stmt in body {
                infer_stmt_types(stmt, symbol_table);
            }

            symbol_table.exit_scope();
        }
        _ => {}
    }
}
