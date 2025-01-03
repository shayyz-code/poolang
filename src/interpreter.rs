use crate::ast::{Expr, Stmt, Type};
use crate::lexer::Token;
use crate::type_inference::infer_stmt_types;
use crate::visitor::ScopedSymbolTable;
use core::panic;
use std::collections::HashMap;

use std::fmt;

pub struct DebuggableIterator {
    // inner: Box<dyn Iterator<Item = Value>>,
}

// impl DebuggableIterator {
//     pub fn new(inner: Box<dyn Iterator<Item = Value>>) -> Self {
//         DebuggableIterator { inner }
//     }
// }

impl fmt::Debug for DebuggableIterator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "DebuggableIterator")
    }
}

// Extend Value to support Int, Float, Boolean, and String
#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Null,
    Int(i64),
    Float(f64),
    Boolean(bool),
    Char(char),
    String(String),
    Vector(Vec<Value>),
    Map(HashMap<String, Value>),
    Function(Vec<(String, Type)>, Vec<Stmt>, Type), // params, body, return_type
    BuiltinFunction(fn(Vec<Value>) -> Value),
    Struct(
        HashMap<String, Type>,
        Vec<String>,
        HashMap<String, Vec<Stmt>>,
        HashMap<String, Type>,
    ), // properties, implementations, implementations type
}
impl Value {
    // Helper method to convert Int and Float to Float if necessary
    // fn as_int(&self) -> i64 {
    //     match self {
    //         Value::Int(v) => *v,
    //         Value::Float(v) => *v as i64,
    //         _ => panic!("Cannot convert to Float"),
    //     }
    // }

    // fn as_float(&self) -> f64 {
    //     match self {
    //         Value::Int(v) => *v as f64,
    //         Value::Float(v) => *v,
    //         _ => panic!("Cannot convert to Float"),
    //     }
    // }

    // Helper method to check if the value is zero (for boolean checks)
    fn is_zero(&self) -> bool {
        match self {
            Value::Int(v) => *v == 0,
            Value::Float(v) => *v == 0.0,
            Value::Boolean(v) => !*v,
            Value::String(v) => v.is_empty(),
            Value::Vector(v) => v.is_empty(),
            Value::Map(v) => v.is_empty(),
            _ => panic!("Unexpected Value in zero checking at boolean checks"),
        }
    }

    fn get_type(&self) -> Type {
        match self {
            Value::Null => Type::Void,
            Value::Boolean(_) => Type::Bool,
            Value::Int(_) => Type::Int,
            Value::Float(_) => Type::Float,
            Value::Char(_) => Type::Char,
            Value::String(_) => Type::String,
            Value::Vector(v) => Type::Vector(Box::new(v[0].get_type())),
            Value::Map(_) => Type::Map(HashMap::new()),
            Value::Function(params, _, return_type) => Type::Function(
                params.iter().map(|p| p.1.clone()).collect(),
                Box::new(return_type.clone()),
            ),
            Value::Struct(prop_types, _, _, impl_types) => {
                Type::Struct(prop_types.clone(), impl_types.clone())
            }
            Value::BuiltinFunction(_) => Type::BuiltinFunction,
        }
    }

    fn is_of_type(&self, expected_type: &Type) -> bool {
        match (self, expected_type) {
            (Value::Null, Type::Void) => true,
            (Value::Int(_), Type::Int) => true,
            (Value::Float(_), Type::Float) => true,
            (Value::Boolean(_), Type::Bool) => true,
            (Value::String(_), Type::String) => true,
            (Value::Vector(_), Type::Vector(_)) => true,
            (Value::Map(_), Type::Map(_)) => true,
            // (Value::Function { return_type, .. }, Type::Function(expected_return_type)) => {
            //     return_type == expected_return_type.as_ref()
            // }
            _ => false,
        }
    }

    // Implement arithmetic operations with type promotion
    fn add(self, other: Value) -> Value {
        match (self, other) {
            (Value::Int(a), Value::Int(b)) => Value::Int(a + b),
            (Value::Float(a), Value::Float(b)) => Value::Float(a + b),
            (Value::String(a), Value::String(b)) => Value::String(a + &b),
            (a, b) => panic!("Adding mismatched types {:?} and {:?}", &a, &b),
        }
    }

    fn sub(self, other: Value) -> Value {
        match (self, other) {
            (Value::Int(a), Value::Int(b)) => Value::Int(a - b),
            (Value::Float(a), Value::Float(b)) => Value::Float(a - b),
            (a, b) => panic!("Subtracting mismatched types {:?} and {:?}", &a, &b),
        }
    }

    fn mul(self, other: Value) -> Value {
        match (self, other) {
            (Value::Int(a), Value::Int(b)) => Value::Int(a * b),
            (Value::Float(a), Value::Float(b)) => Value::Float(a * b),
            (a, b) => panic!("Multiplying mismatched types {:?} and {:?}", &a, &b),
        }
    }

    fn div(self, other: Value) -> Value {
        match (self, other) {
            (Value::Int(a), Value::Int(b)) => {
                if b == 0 {
                    panic!("Division by zero");
                }
                Value::Int(a / b)
            }
            (Value::Float(a), Value::Float(b)) => {
                if b == 0.0 {
                    panic!("Division by zero");
                }
                Value::Float(a / b)
            }
            (a, b) => panic!("Division not supported between {:?} and {:?}", a, b),
        }
    }

    // Comparison operations
    fn lt(self, other: Value) -> Value {
        match (self, other) {
            (Value::Int(a), Value::Int(b)) => Value::Boolean(a < b),
            (Value::Float(a), Value::Float(b)) => Value::Boolean(a < b),
            (a, b) => panic!(
                "Comparison operator < on mismatched types {:?} and {:?}",
                &a, &b
            ),
        }
    }

    fn gt(self, other: Value) -> Value {
        match (self, other) {
            (Value::Int(a), Value::Int(b)) => Value::Boolean(a > b),
            (Value::Float(a), Value::Float(b)) => Value::Boolean(a > b),
            (a, b) => panic!(
                "Comparison operator > on mismatched types {:?} and {:?}",
                &a, &b
            ),
        }
    }

    fn eq(self, other: Value) -> Value {
        match (self, other) {
            (Value::Int(a), Value::Int(b)) => Value::Boolean(a == b),
            (Value::Float(a), Value::Float(b)) => Value::Boolean(a == b),
            (Value::Boolean(a), Value::Boolean(b)) => Value::Boolean(a == b),
            (Value::Char(a), Value::Char(b)) => Value::Boolean(a == b),
            (Value::String(a), Value::String(b)) => Value::Boolean(a == b),

            (a, b) => panic!(
                "Comparison operator == on mismatched types {:?} and {:?}",
                &a, &b
            ),
        }
    }

    fn ne(self, other: Value) -> Value {
        match (self, other) {
            (Value::Int(a), Value::Int(b)) => Value::Boolean(a != b),
            (Value::Float(a), Value::Float(b)) => Value::Boolean(a != b),
            (Value::Boolean(a), Value::Boolean(b)) => Value::Boolean(a != b),
            (Value::String(a), Value::String(b)) => Value::Boolean(a != b),
            (a, b) => panic!(
                "Comparison operator != on mismatched types {:?} and {:?}",
                &a, &b
            ),
        }
    }

    // Vectors
    // fn get_index(self, index: Value) -> Value {
    //     if let Value::Vector(v) = self {
    //         if let Value::Int(i) = index {
    //             return v.get(i as usize).cloned().unwrap_or_else(|| {
    //                 panic!("Index {} out of bounds", i);
    //             });
    //         }
    //         panic!("Index must be an integer");
    //     }
    //     panic!("Indexing is only supported on vectors");
    // }
}

// Implement `to_string` for Value to handle printing
impl ToString for Value {
    fn to_string(&self) -> String {
        match self {
            Value::Null => "null".to_string(),
            Value::Int(v) => v.to_string(),
            Value::Float(v) => v.to_string(),
            Value::Boolean(v) => v.to_string(),
            Value::Char(v) => v.to_string(),
            Value::String(v) => v.clone(),
            Value::Vector(v) => {
                let mut vec = Vec::new();
                for i in v.iter() {
                    vec.push(i.to_string())
                }
                format!("{:?}", vec)
            }
            Value::Map(m) => {
                format!("{:?}", m)
            }
            Value::Function(params, body, return_type) => {
                format!("{:?}", (params, body, return_type))
            }
            Value::Struct(_, _, _, _) => panic!("Cannot be displayed"),
            Value::BuiltinFunction(_) => "BuiltinFunction".to_string(),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Variable {
    value: Value,
    is_mutable: bool,
    var_type: Type,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Scope {
    variables: HashMap<String, Variable>,
}

impl Scope {
    pub fn new() -> Self {
        Scope {
            variables: HashMap::new(),
        }
    }

    pub fn define_function(&mut self, name: String, function: Value) {
        self.variables.insert(
            name,
            Variable {
                value: function,
                is_mutable: false, // Functions are typically immutable
                var_type: Type::Function(vec![], Box::new(Type::Void)),
            },
        );
    }

    pub fn define_struct(&mut self, name: String, struct_val: Value) {
        self.variables.insert(
            name,
            Variable {
                value: struct_val.clone(),
                is_mutable: false,
                var_type: struct_val.get_type(),
            },
        );
    }

    // Get a variable from the current scope
    pub fn get_variable(&self, name: &str) -> Option<Variable> {
        self.variables.get(name).cloned()
    }

    // Set a variable in the current scope
    pub fn set_variable(&mut self, name: String, value: Variable) {
        self.variables.insert(name, value);
    }

    // Reassign a variable in the curent scope
    fn reassign_variable(&mut self, name: &str, new_value: Value) {
        if let Some(variable) = self.variables.get_mut(name) {
            if !variable.is_mutable {
                panic!("Variable {} is immutable", name);
            }
            variable.value = new_value;
        } else {
            panic!("Variable {} not found", name);
        }
    }
}

pub struct Interpreter {
    symbol_table: ScopedSymbolTable,
    scopes: Vec<Scope>,
    return_value: Option<Value>,
    modules: HashMap<String, HashMap<String, Value>>,
}

impl Interpreter {
    pub fn new() -> Self {
        // Start with a global scope
        Interpreter {
            symbol_table: ScopedSymbolTable::new(),
            scopes: vec![Scope::new()],
            return_value: None,
            modules: HashMap::new(),
        }
    }

    // Helper to get the current scope
    fn current_scope(&mut self) -> &mut Scope {
        self.scopes.last_mut().expect("No current scope available")
    }

    fn global_scope(&mut self) -> &mut Scope {
        self.scopes.first_mut().expect("No global scope available")
    }

    // Helper to find a variable from the current scope upwards
    fn find_variable(&self, name: &str) -> Option<Variable> {
        for scope in self.scopes.iter().rev() {
            if let Some(value) = scope.get_variable(name) {
                return Some(value);
            }
        }
        None
    }
    fn load_module(&mut self, module_name: &str) {
        match module_name {
            "std" => {
                let mut std = HashMap::new();
                std.insert(
                    "pout".to_string(),
                    Value::BuiltinFunction(|args| {
                        for arg in args {
                            print!("{}", arg.to_string());
                        }
                        Value::Null // Returning a dummy value
                    }),
                );
                std.insert(
                    "poutln".to_string(),
                    Value::BuiltinFunction(|args| {
                        if args.len() > 0 {
                            for arg in args {
                                print!("{}", arg.to_string());
                            }
                        }
                        println!();
                        Value::Null
                    }),
                );
                std.insert(
                    "cls".to_string(),
                    Value::BuiltinFunction(|_| {
                        print!("\x1B[2J\x1B[1;1H");
                        Value::Null // Returning a dummy value
                    }),
                );

                self.modules.insert("std".to_string(), std);
            }
            "math" => {
                let mut math = HashMap::new();
                math.insert("PI".to_string(), Value::Float(std::f64::consts::PI));
                self.modules.insert("math".to_string(), math);
            }
            "time" => {
                let mut time = HashMap::new();
                time.insert("sleep".to_string(), Value::BuiltinFunction(|args| {
                    if args.len() < 2 {
                        if let Value::Int(v) = args[0] {
                            std::thread::sleep(std::time::Duration::from_millis(v as u64));
                        } else {
                            panic!("Sleep method takes argument as Int in million seconds");
                        }
                    } else {
                        panic!("Sleep method needs only 1 argument is expected instead got {} arguments.", args.len())
                    }
                    Value::Null
                }));
                self.modules.insert("time".to_string(), time);
            }
            _ => panic!("Module '{}' not found", module_name),
        }
    }

    fn eval_method_call(&mut self, target: Value, method_name: &str, args: Vec<Value>) -> Value {
        match target {
            Value::Int(i) => match method_name {
                "_f" => Value::Float(i as f64),
                "clamp" => {
                    if args.len() != 2 {
                        panic!("Method 'clamp_' takes exactly 2 arguments")
                    }
                    match (&args[0], &args[1]) {
                        (&Value::Int(i1), &Value::Int(i2)) => Value::Int(i.clamp(i1, i2)),
                        (_, _) => panic!("Method 'clamp_' arguments must be type Int"),
                    }
                }
                _ => panic!("Unknown method '{}' for type Int.", method_name),
            },
            Value::Float(f) => match method_name {
                "_i" => Value::Int(f as i64),
                "sin" => Value::Float(f.sin()),
                "cos" => Value::Float(f.cos()),
                _ => panic!(
                    "Unknown method '{}' for '{:?}' of type Float",
                    method_name, target
                ),
            },
            Value::String(s) => match method_name {
                "chars" => Value::Vector(s.chars().map(|c| Value::Char(c)).collect()),
                "len" => {
                    if !args.is_empty() {
                        panic!("Method 'len' does not take arguments");
                    }
                    Value::Int(s.len() as i64)
                }
                "nth_char_otherwise_" => {
                    if args.len() != 2 {
                        panic!("Method 'nth_char_' takes exactly 2 arguments")
                    }
                    match (&args[0], &args[1]) {
                        (Value::Int(i1), Value::Char(c)) => {
                            Value::Char(s.chars().nth(*i1 as usize).unwrap_or(*c))
                        }
                        (_, _) => panic!("Method 'nth_char_' takes argument of type Int"),
                    }
                }
                _ => panic!("Unknown method '{}' for type String", method_name),
            },
            Value::Vector(v) => match method_name {
                "len" => {
                    if !args.is_empty() {
                        panic!("Method 'len' does not take arguments");
                    }
                    Value::Int(v.len() as i64)
                }
                "push" => {
                    if args.len() != 1 {
                        panic!("Method 'push' requires exactly 1 argument");
                    }
                    let mut new_vec = v.clone();
                    new_vec.push(args[0].clone());
                    Value::Vector(new_vec)
                }
                "pop" => {
                    let mut new_vec = v.clone();
                    new_vec.pop();
                    Value::Vector(new_vec)
                }
                "nth" => {
                    if args.len() != 1 {
                        panic!("Method 'nth' requires exactly 1 arguments");
                    }
                    match args[0] {
                        Value::Int(i) => v.iter().nth(i as usize).unwrap_or(&Value::Null).clone(),
                        _ => panic!("Method 'nth' needs type Int"),
                    }
                }

                _ => panic!("Unknown method '{}' for type Vector", method_name),
            },
            Value::Map(m) => {
                let mut function_return_value = Value::Null; // Default dummy value
                let get_prototypes = m.get(&"__prototypes__".to_string());
                if get_prototypes.is_some() {
                    let prototypes = get_prototypes.unwrap();
                    if let Value::Map(prototype_vals) = prototypes {
                        let get_method_from_prototype_vals =
                            prototype_vals.get(&method_name.to_string());
                        if let Some(Value::Function(params, body, return_type)) =
                            get_method_from_prototype_vals
                        {
                            if params.len() != args.len() {
                                panic!(
                                    "Method {} expects {} arguments, but {} were provided",
                                    method_name,
                                    params.len(),
                                    args.len()
                                );
                            }
                            // Push a new scope and set the current function context
                            self.scopes.push(Scope::new());

                            self.current_scope().set_variable(
                                "__current_function__".to_string(),
                                Variable {
                                    value: get_method_from_prototype_vals.unwrap().clone(),
                                    is_mutable: false,
                                    var_type: Type::Function(
                                        params.iter().map(|j| j.1.clone()).collect(),
                                        Box::new(return_type.clone()),
                                    ),
                                },
                            );

                            let mut m_types = HashMap::new();
                            // Set function parameters && m_types for self
                            for (param, arg_value) in params.iter().zip(args) {
                                m_types.insert(param.0.clone(), param.1.clone());
                                self.current_scope().set_variable(
                                    param.0.clone(),
                                    Variable {
                                        value: arg_value,
                                        is_mutable: true,
                                        var_type: param.1.clone(),
                                    },
                                );
                            }

                            // Set self
                            self.current_scope().set_variable(
                                "self".to_string(),
                                Variable {
                                    value: Value::Map(m.clone()),
                                    is_mutable: true,
                                    var_type: Type::Map(m_types),
                                },
                            );

                            for stmt in body {
                                self.exec_stmt(&stmt);
                                if let Some(return_val) = self.return_value.take() {
                                    function_return_value = return_val;
                                    break;
                                }
                            }

                            // Pop the scope after execution
                            self.scopes.pop();

                            // Validate return type

                            if !function_return_value.is_of_type(&return_type) {
                                panic!( "Function {} returned a value of mismatched type. Expected {:?}, got {:?}",
                                            method_name, return_type, function_return_value
                                        );
                            }
                        } else {
                            panic!("Method '{}' not supported for type {:?}", method_name, m);
                        }
                    }
                }
                function_return_value
            }
            _ => panic!(
                "Method '{}' not supported for type {:?}",
                method_name, target
            ),
        }
    }

    // Evaluate an expression and return its value
    fn eval_expr(&mut self, expr: &Expr) -> Value {
        match expr {
            Expr::Null => Value::Null,
            Expr::Int(value) => Value::Int(*value),
            Expr::Float(value) => Value::Float(*value),
            Expr::Boolean(value) => Value::Boolean(*value),
            Expr::Char(value) => Value::Char(*value),
            Expr::String(value) => Value::String(value.clone()),
            Expr::Identifier(name) => {
                let var = self
                    .find_variable(name)
                    .expect(&format!("Undefined variable: {}", name));
                var.value
            }
            Expr::Vector(elements, extensor) => {
                let evaluated_elements: Vec<Value> =
                    elements.iter().map(|e| self.eval_expr(e)).collect();
                if let Some(extensor_expr) = extensor {
                    let evaluated_extensor = self.eval_expr(&extensor_expr);
                    match evaluated_extensor {
                        Value::Int(i) => {
                            let vector = vec![evaluated_elements[0].clone(); i as usize];
                            Value::Vector(vector)
                        }
                        _ => panic!("Vector extensor needs to be of type Int"),
                    }
                } else {
                    Value::Vector(evaluated_elements)
                }
            }
            Expr::Map(map) => {
                let mut evaluated_map: HashMap<String, Value> = HashMap::new();

                for item in map {
                    evaluated_map.insert(item.0.clone(), self.eval_expr(item.1));
                }

                Value::Map(evaluated_map)
            }
            Expr::MapKey(map_expr, key) => {
                let map_val = self.eval_expr(map_expr);
                if let Value::Map(map) = map_val {
                    if map.is_empty() {
                        panic!("Indexing with key {} in an empty Map {:?}", key, map)
                    } else if !map.contains_key(key) {
                        panic!("Value for key {} does not exist in the map", key)
                    } else {
                        map.get(key).unwrap().clone()
                    }
                } else {
                    panic!("Indexing with key in a non-map type")
                }
            }
            Expr::VectorIndex(vector_expr, vec_of_indices_expr) => {
                let mut vector_value = self.eval_expr(vector_expr);
                let mut vec_of_indices = Vec::new();
                for expr in vec_of_indices_expr.iter() {
                    vec_of_indices.push(self.eval_expr(expr));
                }
                let mut return_value = Value::Null;
                for index in vec_of_indices {
                    match (vector_value, index.clone()) {
                        (Value::Vector(vec), Value::Int(idx)) => {
                            if idx < 0 || idx as usize >= vec.len() {
                                panic!(
                                    "Index out of bounds: index {} in len of {}",
                                    idx,
                                    vec.len()
                                );
                            }
                            vector_value = vec[idx as usize].clone();
                            return_value = vector_value.clone()
                        }
                        (Value::Vector(_), _) => panic!("Index must be an integer"),
                        (_, _) => panic!("Cannot index a non-vector value"),
                    }
                }
                return_value
            }
            Expr::MethodCall(base, method_name, args) => {
                // Evaluate the target of the method
                let target_value = self.eval_expr(base);

                // Evaluate arguments (if any)
                let evaluated_args: Vec<Value> =
                    args.iter().map(|arg| self.eval_expr(arg)).collect();

                // Dispatch to the appropriate method implementation
                self.eval_method_call(target_value, method_name, evaluated_args)
            }
            Expr::BinaryOp(left, op, right) => {
                let left_val = self.eval_expr(left);
                let right_val = self.eval_expr(right);

                match op {
                    Token::Plus => left_val.add(right_val),
                    Token::Minus => left_val.sub(right_val),
                    Token::Multiply => left_val.mul(right_val),
                    Token::Divide => left_val.div(right_val),
                    Token::LessThan => left_val.lt(right_val),
                    Token::GreaterThan => left_val.gt(right_val),
                    Token::Equal => left_val.eq(right_val),
                    Token::NotEqual => left_val.ne(right_val),
                    Token::And => Value::Boolean(!left_val.is_zero() && !right_val.is_zero()),
                    Token::Or => Value::Boolean(!left_val.is_zero() || !right_val.is_zero()),
                    Token::Otherwise => {
                        if left_val == Value::Null {
                            right_val
                        } else {
                            left_val
                        }
                    }
                    _ => panic!("Unexpected operator: {:?}", op),
                }
            }
            Expr::UnaryOp(op, right) => {
                let right_val = self.eval_expr(right);
                match op {
                    Token::Not => Value::Boolean(right_val.is_zero()),
                    Token::Minus => match right_val {
                        Value::Int(v) => Value::Int(-v),
                        Value::Float(v) => Value::Float(-v),
                        _ => panic!("Cannot negate this type"),
                    },
                    _ => panic!("Unexpected unary operator: {:?}", op),
                }
            }
            Expr::StructCompound(struct_name, prop_exprs) => {
                let struct_var = self.find_variable(struct_name);
                let mut props = HashMap::new();
                for prop_expr in prop_exprs {
                    let val = self.eval_expr(prop_expr.1);
                    props.insert(prop_expr.0.clone(), val);
                }
                if struct_var.is_some() {
                    let struct_val = struct_var.unwrap().value;
                    if let Value::Struct(prop_types, _, impl_stmts, _) = struct_val {
                        for prop_type in prop_types.iter() {
                            match prop_type {
                                (p_name, p_type) => {
                                    let a = props.get(p_name);
                                    if a.is_none() {
                                        panic!(
                                            "Mismatched key '{}' on struct compound - '{}'",
                                            p_name, struct_name
                                        )
                                    }
                                    let b = a.unwrap().get_type();
                                    if b != p_type.clone() {
                                        panic!(
                                            "Mismatched data type, expected '{:?}' but got '{:?}' during initializing '{}'",
                                            p_type, b, struct_name
                                        )
                                    }
                                }
                            }
                        }

                        let mut struct_map = props;
                        let mut prototypes = HashMap::new();

                        for (target, stmts) in impl_stmts {
                            for method_stmt in stmts.iter() {
                                if let Stmt::FunctionDeclaration(name, params, body, return_type) =
                                    method_stmt
                                {
                                    let function = Value::Function(
                                        params.clone(),
                                        body.clone(),
                                        return_type.clone(),
                                    );
                                    prototypes.insert(name.clone(), function);
                                }
                            }
                        }

                        struct_map.insert("__prototypes__".to_string(), Value::Map(prototypes));

                        Value::Map(struct_map)
                    } else {
                        panic!(
                            "Unexpected Compound literal on initializing non-struct type - {}",
                            struct_name
                        );
                    }
                } else {
                    panic!("Undefined struct {}", struct_name);
                }
            }
            Expr::FunctionCall(name, args) => {
                let evaluated_args: Vec<Value> =
                    args.iter().map(|arg| self.eval_expr(arg)).collect();

                if let Some(var) = self.find_variable(name) {
                    match &var.value {
                        Value::Function(params, body, return_type) => {
                            if params.len() != evaluated_args.len() {
                                panic!(
                                    "Function {} expects {} arguments, but {} were provided",
                                    name,
                                    params.len(),
                                    evaluated_args.len()
                                );
                            }
                            // Push a new scope and set the current function context
                            self.scopes.push(Scope::new());
                            self.current_scope().set_variable(
                                "__current_function__".to_string(),
                                Variable {
                                    value: var.value.clone(),
                                    is_mutable: false,
                                    var_type: Type::Function(
                                        params.iter().map(|j| j.1.clone()).collect(),
                                        Box::new(return_type.clone()),
                                    ),
                                },
                            );

                            // Set function parameters
                            for (param, arg_value) in params.iter().zip(evaluated_args) {
                                self.current_scope().set_variable(
                                    param.0.clone(),
                                    Variable {
                                        value: arg_value,
                                        is_mutable: true,
                                        var_type: param.1.clone(),
                                    },
                                );
                            }

                            let mut function_return_value = Value::Null; // Default dummy value
                            for stmt in body {
                                self.exec_stmt(&stmt);
                                if let Some(return_val) = self.return_value.take() {
                                    function_return_value = return_val;
                                    break;
                                }
                            }

                            // Pop the scope after execution
                            self.scopes.pop();

                            // Validate return type

                            if !function_return_value.is_of_type(&return_type) {
                                panic!( "Function {} returned a value of mismatched type. Expected {:?}, got {:?}",
                                            name, return_type, function_return_value
                                        );
                            }
                            function_return_value
                        }
                        Value::BuiltinFunction(func) => func(evaluated_args),
                        _ => panic!("Variable {} is not callable", name),
                    }
                } else {
                    panic!("Undefined function: {}", name);
                }
            }
        }
    }
    // Execute a statement
    fn exec_stmt(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::Use(modules) => {
                // print!("{:?}", modules);
                for module in modules {
                    let parts: Vec<&str> = module.split("::").collect();
                    let module_name = parts[0];
                    let symbol_name = parts[1];

                    // Load the module if not already loaded
                    if !self.modules.contains_key(module_name) {
                        if module_name == "std" || module_name == "math" || module_name == "time" {
                            self.load_module(module_name); // Example standard library module
                        } else {
                            panic!("Unknown module '{}'", module_name);
                        }
                    }
                    // Bind the symbol to the current scope
                    if let Some(module) = self.modules.get(module_name) {
                        if let Some(value) = module.get(symbol_name) {
                            let value_clone = value.clone(); // Clone the value here
                            self.current_scope().set_variable(
                                symbol_name.to_string(),
                                Variable {
                                    value: value_clone,
                                    is_mutable: false,
                                    var_type: Type::Module,
                                },
                            );
                        } else {
                            panic!(
                                "Symbol '{}' not found in module '{}'",
                                symbol_name, module_name
                            );
                        }
                    } else {
                        panic!("Module '{}' not found", module_name);
                    }
                }
            }

            Stmt::Struct(name, inherit_names, properties, impl_stmts_map) => {
                let mut props = HashMap::new();
                let mut impl_stmts = impl_stmts_map.clone();
                for inherit_name in inherit_names.iter() {
                    let inherit_struct = self.find_variable(&inherit_name);
                    if inherit_struct.is_none() {
                        panic!("Undefined Struct for inheritance");
                    } else {
                        let inherit_struct = inherit_struct.unwrap();
                        match inherit_struct.value {
                            Value::Struct(inhe_props, _, inhe_impl_stmts_map, _) => {
                                props.extend(inhe_props);
                                for stmt in inhe_impl_stmts_map
                                    .get(&"Self".to_string())
                                    .unwrap()
                                    .clone()
                                {
                                    impl_stmts
                                        .entry(inherit_name.clone())
                                        .or_insert_with(Vec::new)
                                        .push(stmt);
                                }
                            }
                            _ => panic!("Inheritance from a Struct is only allowed"),
                        }
                    }
                }
                for property in properties.iter() {
                    props.insert(property.name.clone(), property.prop_type.clone());
                }
                let struct_val = Value::Struct(
                    props,
                    inherit_names.clone(),
                    impl_stmts.clone(),
                    HashMap::new(),
                );
                // println!("////////{:?}", impl_stmts.clone());
                self.current_scope().define_struct(name.clone(), struct_val);
            }

            Stmt::Expression(expr) => {
                self.eval_expr(expr);
            }
            Stmt::Assignment(name, expr, is_mutable, var_type) => {
                let value = self.eval_expr(expr);
                let is_mutable = *is_mutable;
                let var_type = match var_type {
                    Some(vt) => vt.clone(),
                    None => value.get_type(),
                };
                self.current_scope().set_variable(
                    name.clone(),
                    Variable {
                        value,
                        is_mutable,
                        var_type,
                    },
                );
            }

            Stmt::Reassignment(name, vec_idx, expr) => {
                let value = self.eval_expr(expr); // Evaluate the new value
                let current_var = self.find_variable(name);

                if current_var.is_none() {
                    panic!("Variable '{}' has not been declared.", name);
                }

                let current_var = current_var.unwrap();
                if !current_var.is_mutable {
                    panic!("Variable '{}' is not mutable.", name);
                }

                if let Some(Expr::VectorIndex(vector_expr, indices_expr)) = vec_idx {
                    // Ensure the variable name matches
                    if let Expr::Identifier(vector_name) = &**vector_expr {
                        if name != vector_name {
                            panic!("Variable name mismatch in reassignment.");
                        }
                    } else {
                        panic!("Expected a vector identifier in vector index.");
                    }

                    // Evaluate indices into integers
                    let indices: Vec<usize> = indices_expr
                        .iter()
                        .map(|idx_expr| match self.eval_expr(idx_expr) {
                            Value::Int(idx) if idx >= 0 => idx as usize,
                            _ => {
                                panic!("Index expressions must evaluate to non-negative integers.")
                            }
                        })
                        .collect();

                    // Ensure the variable holds a vector
                    if let Value::Vector(mut vec) = current_var.value.clone() {
                        let mut current_element = &mut vec; // Start traversal at the top-level vector
                        for &idx in &indices[0..indices.len() - 1] {
                            if let Some(Value::Vector(inner_vec)) = current_element.get_mut(idx) {
                                current_element = inner_vec;
                            } else {
                                panic!("Index '{}' points to a non-vector type.", idx);
                            }
                        }

                        // Perform reassignment at the final index
                        let final_index = *indices.last().unwrap();
                        if let Some(element) = current_element.get_mut(final_index) {
                            match (&element, &value) {
                                (Value::Boolean(_), Value::Boolean(_))
                                | (Value::Float(_), Value::Float(_))
                                | (Value::Int(_), Value::Int(_))
                                | (Value::Char(_), Value::Char(_))
                                | (Value::String(_), Value::String(_)) => {
                                    *element = value;
                                }
                                _ => panic!(
                                    "Type mismatch {:?} and {:?} during reassignment.",
                                    element.get_type(),
                                    value.get_type()
                                ),
                            }
                        } else {
                            panic!("Index '{}' out of bounds for vector.", final_index);
                        }

                        // Update the variable in the current scope
                        self.current_scope().set_variable(
                            name.clone(),
                            Variable {
                                value: Value::Vector(vec),
                                is_mutable: current_var.is_mutable,
                                var_type: current_var.var_type,
                            },
                        );
                    } else {
                        panic!("Variable '{}' is not a vector.", name);
                    }
                } else {
                    self.current_scope().set_variable(
                        name.clone(),
                        Variable {
                            value,
                            is_mutable: current_var.is_mutable,
                            var_type: current_var.var_type,
                        },
                    );
                }
            }

            Stmt::FunctionDeclaration(name, params, body, return_type) => {
                let function = Value::Function(params.clone(), body.clone(), return_type.clone());
                self.current_scope().define_function(name.clone(), function);
            }
            Stmt::If(condition, if_body, else_body) => {
                let condition_value = self.eval_expr(condition);

                let body_to_execute = if !condition_value.is_zero() {
                    if_body
                } else if let Some(Stmt::If(..)) = else_body.first() {
                    // If the first element of the `else_body` is another `If` statement, evaluate it
                    for stmt in else_body {
                        self.exec_stmt(stmt);
                        if self.return_value.is_some() {
                            break;
                        }
                    }
                    return;
                } else {
                    else_body
                };

                // self.scopes.push(Scope::new());
                for stmt in body_to_execute {
                    self.exec_stmt(stmt);
                    if self.return_value.is_some() {
                        break;
                    }
                }
                // self.scopes.pop();
            }

            Stmt::While(condition, body) => {
                while !self.eval_expr(condition).is_zero() {
                    // self.scopes.push(Scope::new());
                    for stmt in body {
                        self.exec_stmt(stmt);
                        if self.return_value.is_some() {
                            self.scopes.pop();
                            return;
                        }
                    }
                    // self.scopes.pop();
                }
            }
            Stmt::ForRange(iter, from, to, step_size, body) => {
                let start = match self.eval_expr(from) {
                    Value::Int(v) => v,
                    _ => panic!("For loop start must be type Int"),
                };

                let end = match self.eval_expr(to) {
                    Value::Int(v) => v,
                    _ => panic!("For loop end must be type Int"),
                };

                let step = match self.eval_expr(step_size) {
                    Value::Int(v) => v,
                    _ => panic!("For loop step must be type Int"),
                };

                for i in (start..end).step_by(step as usize) {
                    // Push a new scope for the loop iteration
                    // self.scopes.push(Scope::new());
                    // Assign the loop variable in the current scope
                    self.current_scope().set_variable(
                        iter.clone(),
                        Variable {
                            value: Value::Int(i),
                            is_mutable: false,
                            var_type: Type::Int, // Typically, loop variables are immutable
                        },
                    );

                    // Execute the body statements
                    for stmt in body {
                        self.exec_stmt(stmt);
                        if self.return_value.is_some() {
                            self.scopes.pop();
                            return; // Exit if there's a return statement
                        }
                    }

                    // Pop the scope after each iteration
                    // self.scopes.pop();
                }
            }
            Stmt::ForVector(iter, vector, body) => {
                let evaluated_vector = match self.eval_expr(vector) {
                    Value::Vector(v) => v,
                    _ => panic!("ERROR: For in loop must be iterable"),
                };

                for item in evaluated_vector {
                    // Push a new scope for the loop iteration
                    // self.scopes.push(Scope::new());

                    // Assign the loop variable in the current scope
                    self.current_scope().set_variable(
                        iter.clone(),
                        Variable {
                            value: item.clone(),
                            is_mutable: false,
                            var_type: item.get_type(), // Typically, loop variables are immutable
                        },
                    );

                    // Execute the body statements
                    for stmt in body {
                        self.exec_stmt(stmt);
                        if self.return_value.is_some() {
                            self.scopes.pop();
                            return; // Exit if there's a return statement
                        }
                    }

                    // Pop the scope after each iteration
                    // self.scopes.pop();
                }
            }

            Stmt::Return(expr) => {
                let return_value = self.eval_expr(expr.as_ref().unwrap_or(&Expr::Null));

                // Check if we're inside a function
                let current_function = self.find_variable("__current_function__");
                if let Some(Variable {
                    value: Value::Function(.., return_type),
                    ..
                }) = current_function
                {
                    if !return_value.is_of_type(&return_type) {
                        panic!(
                            "Return value type mismatch. Expected {:?}, got {:?}",
                            return_type, return_value
                        );
                    }
                } else {
                    panic!("Return statement used outside of a function");
                }

                self.return_value = Some(return_value);
            }
        }
    }

    // Interpret a list of statements
    pub fn interpret(&mut self, statements: &[Stmt]) -> Option<Value> {
        self.return_value = None;
        for stmt in statements {
            infer_stmt_types(stmt, &mut self.symbol_table);

            self.exec_stmt(stmt);
            if self.return_value.is_some() {
                break;
            }
        }
        // for scope in self.scopes.iter() {
        //     println!("{:?}\n", scope);
        // }
        self.return_value.clone()
    }
}
