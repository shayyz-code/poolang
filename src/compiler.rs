use crate::ast::{Expr, Stmt};
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::values::FunctionValue; // Assuming these are your AST definitions

pub struct Codegen<'ctx> {
    context: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,
}

impl<'ctx> Codegen<'ctx> {
    pub fn new(context: &'ctx Context, module_name: &str) -> Self {
        let module = context.create_module(module_name);
        let builder = context.create_builder();

        Codegen {
            context,
            module,
            builder,
        }
    }

    pub fn compile_stmt(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::Expression(expr) => {
                self.compile_expr(expr);
            }
            Stmt::FunctionDeclaration(name, params, body, _) => {
                self.compile_function(name, params, body);
            }
            _ => todo!("Handle other statement types"),
        }
    }

    fn compile_expr(&mut self, expr: &Expr) -> inkwell::values::BasicValueEnum<'ctx> {
        match expr {
            Expr::Int(value) => self
                .context
                .i32_type()
                .const_int(*value as u64, false)
                .into(),
            Expr::Float(value) => self.context.f64_type().const_float(*value).into(),
            _ => todo!("Handle other expression types"),
        }
    }

    fn compile_function(
        &mut self,
        name: &str,
        params: &[(String, crate::ast::Type)],
        body: &[Stmt],
    ) -> FunctionValue<'ctx> {
        let i32_type = self.context.i32_type();
        let fn_type = i32_type.fn_type(&[], false);
        let function = self.module.add_function(name, fn_type, None);

        let entry = self.context.append_basic_block(function, "entry");
        self.builder.position_at_end(entry);

        for stmt in body {
            self.compile_stmt(stmt);
        }

        self.builder
            .build_return(Some(&i32_type.const_int(0, false)));
        function
    }

    pub fn print_ir(&self) {
        self.module.print_to_stderr();
    }
}
