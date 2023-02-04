use std::rc::Rc;
use std::{borrow::Borrow, collections::HashMap};

use crate::ast::{BinaryOperator, LogicalOperator, Type, UnaryOperator};
use crate::{
    ast::{self, Expression},
    typechecker::datatype,
};

use self::builder::StructBuilder;
use self::emitter::{emit_function_call, emit_if};
use self::{
    emitter::{emit_binary, emit_unary},
    generator::FunctionGenerator,
};

mod builder;
mod emitter;
mod generator;
mod resolver;

pub struct Compiler {
    function: FunctionGenerator,
    function_header: Vec<(String, Type, Vec<(String, String)>, String)>,
    record_types: HashMap<String, Rc<datatype::Type>>,
}

// TODO: create a table of symbols so we don't need to make new strings every time

impl Compiler {
    pub fn new(record_types: HashMap<String, Rc<datatype::Type>>) -> Self {
        Compiler {
            function: FunctionGenerator::new(),
            function_header: Vec::new(),
            record_types,
        }
    }

    // Wrapper over the resolver module
    fn resolve_type(&self, var_name: &String, var_type: &Type) -> String {
        match var_type {
            Type::Function(_, _) => resolver::get_function_pointer(var_name.into(), var_type),
            _ => format!("{} {}", resolver::get_native_type(var_type), var_name),
        }
    }

    // Generates the header of forward declarations and the struct definitions.
    fn generate_record_header(&self) -> String {
        let mut buffer = String::new();

        // TODO: have forward declarations before defining records

        for (name, record_type) in self.record_types.borrow() {
            let record_fields = match record_type.borrow() {
                datatype::Type::Record(_, fields) => fields,

                _ => panic!(),
            };

            // Define the record as a C struct.
            let mut record_struct = StructBuilder::new(name.clone());

            for (field_name, _field_type) in record_fields {
                record_struct.field(field_name.clone(), "int".to_string());
            }

            buffer.push_str(&record_struct.build());
            buffer.push_str(&record_struct.build_constructor());
        }

        buffer
    }

    // Generates the header of forward declarations & function definitions.
    fn generate_function_header(&mut self) -> String {
        let mut buffer = String::new();

        // TODO: have forward declarations before defining functions
        // TODO: implement functions that return function pointers
        // TODO: generate struct for function callback

        for (func_name, func_type, func_args, func_body) in &self.function_header {
            let args = func_args
                .iter()
                .map(|(_, arg_type)| arg_type.clone())
                .collect::<Vec<String>>()
                .join(", ");

            let func_env = StructBuilder::new(format!("{}_env", func_name));

            let result = match func_type.borrow() {
                Type::Function(base_func_args, _) => {
                    let func_pointer = format!(
                        "(*{}({}))",
                        func_name,
                        base_func_args
                            .iter()
                            .map(|t| resolver::get_native_type(t))
                            .collect::<Vec<String>>()
                            .join(", ")
                    );

                    format!(
                        "{} {{\n{}\n}}",
                        self.resolve_type(&func_pointer, func_type),
                        func_body
                    )
                }
                _ => format!(
                    "{} ({}){{\n{}\n}}",
                    self.resolve_type(func_name, func_type),
                    args,
                    func_body
                ),
            };

            buffer.push_str(&func_env.build());
            buffer.push('\n');
            buffer.push_str(&result);
            buffer.push('\n');
        }

        buffer
    }

    pub fn compile(&mut self, expression: &Expression) -> String {
        let mut buffer = String::new();

        let root_expr = self.compile_expression(expression);

        buffer.push_str("#include<stdio.h>\n");
        buffer.push_str("#include<stdlib.h>\n");

        buffer.push_str("// Record header\n");
        buffer.push_str(&self.generate_record_header());
        buffer.push_str("\n// Function header\n");
        buffer.push_str(&self.generate_function_header());
        buffer.push_str("\n\n// Program\n");

        // Inject the source into the main function
        buffer.push_str("int main(int argc, char** argv){\n");

        // Inject any Kilogram-specific C code here.
        buffer.push_str(&root_expr);

        // Destructors
        buffer.push_str("\n}");

        buffer
    }

    fn compile_unary(&mut self, expression: &Expression, operation: &UnaryOperator) -> String {
        let unary_symbol = match operation {
            ast::UnaryOperator::Minus => "-".to_string(),
            ast::UnaryOperator::Bang => "!".to_string(),
        };

        emit_unary(unary_symbol, self.compile_expression(expression))
    }

    fn compile_binary(
        &mut self,
        left: &Expression,
        right: &Expression,
        operation: &BinaryOperator,
    ) -> String {
        let binary_symbol = match operation {
            ast::BinaryOperator::Add => "+".to_string(),
            ast::BinaryOperator::Sub => "-".to_string(),
            ast::BinaryOperator::Mult => "*".to_string(),
            ast::BinaryOperator::Div => "/".to_string(),
            ast::BinaryOperator::Equality => "==".to_string(),
            ast::BinaryOperator::NotEqual => "!=".to_string(),
            ast::BinaryOperator::Greater => ">".to_string(),
            ast::BinaryOperator::GreaterEq => ">=".to_string(),
            ast::BinaryOperator::Less => "<".to_string(),
            ast::BinaryOperator::LessEq => "<=".to_string(),
        };

        emit_binary(
            self.compile_expression(left),
            binary_symbol,
            self.compile_expression(right),
        )
    }

    fn compile_logical(
        &mut self,
        left: &Expression,
        right: &Expression,
        operation: &LogicalOperator,
    ) -> String {
        let logical_symbol = match operation {
            ast::LogicalOperator::And => "&&",
            ast::LogicalOperator::Or => "||",
        };

        emit_binary(
            self.compile_expression(left),
            logical_symbol.to_string(),
            self.compile_expression(right),
        )
    }

    fn compile_let(
        &mut self,
        name: &String,
        var_type: &Type,
        value: &Box<Expression>,
        body: &Box<Expression>,
    ) -> String {
        // Is the last node not a declaration? Return it.
        let is_leaf = match body.borrow() {
            Expression::Let(..) => false,
            _ => true,
        };

        format!(
            "{} = {};\n{}{}{}",
            self.resolve_type(name, var_type),
            self.compile_expression(value),
            if is_leaf { "return " } else { "" },
            self.compile_expression(body),
            if is_leaf { ";" } else { "" }
        )
    }

    /// Define the lambda under a unique name and returns a pointer
    /// to it.
    ///
    /// * `func_type`:
    /// * `arg_types`:
    /// * `value`:
    fn compile_function(
        &mut self,
        func_type: &Type,
        arg_types: &Vec<(String, Type)>,
        value: &Box<Expression>,
    ) -> String {
        // Generate fresh name.
        let fresh_name = self.function.generate();
        let is_leaf = match value.borrow() {
            Expression::Let(_, _, _, _, _) => false,
            _ => true,
        };

        let func_body = format!(
            "{}{}{}",
            if is_leaf { "return " } else { "" },
            self.compile_expression(value),
            if is_leaf { ";" } else { "" }
        );

        let arguments = arg_types
            .iter()
            .map(|(arg_name, arg_type)| {
                (
                    arg_name.clone(),
                    self.resolve_type(arg_name, arg_type.borrow()),
                )
            })
            .collect();

        self.function_header
            .push((fresh_name.clone(), func_type.clone(), arguments, func_body));

        // All user-declared functions are a pointer to a function in the function header.
        fresh_name
    }

    /// Generates a function call given a function name and arguments.
    ///
    /// * `name`:
    /// * `arguments`:
    fn compile_function_call(
        &mut self,
        name: &Expression,
        arguments: &Vec<Box<Expression>>,
    ) -> String {
        let argument_list: Vec<String> = arguments
            .iter()
            .map(|arg| self.compile_expression(arg))
            .collect();

        let function = self.compile_expression(name);

        emit_function_call(function, &argument_list, false)
    }

    fn compile_record_instance(
        &mut self,
        name: &String,
        fields: &Vec<(String, Box<Expression>)>,
    ) -> String {
        let mut buffer = String::new();

        buffer.push_str(format!("_create_{}(", name).as_str());

        buffer.push_str(
            fields
                .iter()
                .map(|(_, field_value)| self.compile_expression(field_value))
                .collect::<Vec<String>>()
                .join(", ")
                .as_str(),
        );

        buffer.push_str(")");

        buffer
    }

    /// Compiles an expression.
    ///
    /// * `expression`:
    pub fn compile_expression(&mut self, expression: &Expression) -> String {
        match expression {
            Expression::Integer(value) => format!("{}", value),
            Expression::Float(value) => format!("{}", value),
            Expression::Str(value) => format!("string_create(\"{}\")", value),
            Expression::Boolean(value) => format!("{}", value),
            Expression::Variable(name) => format!("{}", name),

            Expression::Group(expr) => format!("({})", self.compile_expression(expr)),

            Expression::Unary(expr, operation) => self.compile_unary(expr, operation),
            Expression::Binary(left, operation, right) => {
                self.compile_binary(left, right, operation)
            }
            Expression::Logical(left, operation, right) => {
                self.compile_logical(left, right, operation)
            }

            Expression::If(if_expr, then_expr, else_expr) => emit_if(
                self.compile_expression(if_expr),
                self.compile_expression(then_expr),
                self.compile_expression(else_expr),
            ),

            Expression::Let(name, var_type, value, body, _) => {
                self.compile_let(name, var_type, value, body)
            }

            Expression::Function(_, func_type, arg_types, value) => {
                self.compile_function(func_type, arg_types, value)
            }

            Expression::Get(name, expr) => format!("(Get, name: '{}', parent: {})", name, expr),

            Expression::FunctionCall(name, arguments) => {
                self.compile_function_call(name, arguments)
            }

            // Just ignore record declarations, already handled in the record header.
            Expression::RecordDeclaration(_, _, body) => self.compile_expression(body),

            Expression::RecordInstance(name, fields) => self.compile_record_instance(name, fields),
        }
    }
}
