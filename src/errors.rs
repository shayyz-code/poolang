pub struct TypeError {
    pub message: String,
}

impl TypeError {
    pub fn new(message: &str) -> Self { 
        Self {
            message: message.to_string(),
        }
    }
}

pub struct UndefinedVariableError {
    pub message: String,
}

impl UndefinedVariableError {
    pub fn new(variable_name: &str) -> Self {
        Self {
            message: format!("Undefined variable: {}", variable_name),
        }
    }
}
