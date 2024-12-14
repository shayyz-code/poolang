use crate::ast::Type;
use std::collections::HashMap;

#[derive(Debug, PartialEq, Clone)]
pub struct ScopedSymbolTable {
    scopes: Vec<HashMap<String, Type>>,
}

impl ScopedSymbolTable {
    pub fn new() -> Self {
        Self {
            scopes: vec![HashMap::new()], // Start with a global scope
        }
    }

    pub fn enter_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    pub fn exit_scope(&mut self) {
        self.scopes.pop();
    }

    pub fn insert(&mut self, key: String, value: Type) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(key, value);
        }
    }

    pub fn get(&self, key: &str) -> Option<&Type> {
        for scope in self.scopes.iter().rev() {
            if let Some(value) = scope.get(key) {
                return Some(value);
            }
        }
        None
    }
}

// pub struct SemanticAnalyzer {
//     symbol_table: ScopedSymbolTable,
// }

// impl SemanticAnalyzer {
//     pub fn new() -> Self {
//         Self {
//             symbol_table: ScopedSymbolTable::new(),
//         }
//     }

//     pub fn analyze(&mut self, stmts: &[Stmt]) {
//         for stmt in stmts {
//             self.analyze_stmt(stmt);
//         }
//     }

//     fn analyze_stmt(&mut self, stmt: &Stmt) {
//         match stmt {
//             Stmt::Assignment(name, expr, _, _) => {
//                 let expr_type = self.analyze_expr(expr);

//                 self.symbol_table.insert(name.clone(), expr_type);
//             }
//             Stmt::Reassignment(name, None, expr) => {
//                 let expr_type = self.analyze_expr(expr);
//                 if let Some(var_type) = self.symbol_table.get(name) {
//                     if var_type != &expr_type {
//                         panic!(
//                             "Type mismatch for variable '{}': expected {:?}, found {:?}",
//                             name, var_type, expr_type
//                         );
//                     }
//                 } else {
//                     panic!("Variable '{}' used before declaration", name);
//                 }
//             }
//             Stmt::If(cond, body, else_body) => {
//                 let cond_type = self.analyze_expr(cond);
//                 if cond_type != Type::Bool {
//                     panic!("Condition in if statement must be a boolean");
//                 }
//                 self.symbol_table.enter_scope();
//                 self.analyze(body);
//                 self.symbol_table.exit_scope();
//                 self.symbol_table.enter_scope();
//                 self.analyze(else_body);
//                 self.symbol_table.exit_scope();
//             }
//             // Other statement types...
//             _ => {}
//         }
//     }

//     fn analyze_expr(&mut self, expr: &Expr) -> Type {
//         match expr {
//             Expr::Int(_) => Type::Int,
//             Expr::Float(_) => Type::Float,
//             Expr::Boolean(_) => Type::Bool,
//             Expr::Char(_) => Type::Char,
//             Expr::Identifier(name) => self
//                 .symbol_table
//                 .get(name)
//                 .cloned()
//                 .expect(&format!("Undefined variable '{}'", name)),
//             Expr::BinaryOp(lhs, op, rhs) => {
//                 let left_type = self.analyze_expr(lhs);
//                 let right_type = self.analyze_expr(rhs);
//                 match (left_type, right_type, op) {
//                     (Type::Int, Type::Int, Token::Plus) => Type::Int,
//                     // Add more cases for supported operations...
//                     _ => panic!("Unsupported operation: {:?} {:?} {:?}", lhs, op, rhs),
//                 }
//             }
//             // Other expression types...
//             _ => panic!("Unknown expression type"),
//         }
//     }
// }
