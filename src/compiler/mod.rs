use std::rc::Rc;
use std::{borrow::Borrow, collections::HashMap};

use crate::{
    ast::{self, Expression},
    typechecker::datatype,
};

use self::emitter::emit_function_call;
use self::{
    emitter::{emit_binary, emit_struct, emit_unary},
    generator::FunctionGenerator,
};

mod emitter;
mod generator;
mod resolver;

pub struct Compiler {
    function: FunctionGenerator,
    function_header: Vec<(String, String, Vec<(String, String)>, String)>,
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

    // Generates the header of forward declarations and the struct definitions.
    fn generate_record_header(&self) -> String {
        let mut buffer = String::new();

        for (name, record_type) in self.record_types.borrow() {
            let record_fields = match record_type.borrow() {
                datatype::Type::Record(_, fields) => {
                    let test: Vec<(String, String)> = fields
                        .iter()
                        .map(|(field_name, _)| (field_name.clone(), "int".to_string()))
                        .collect();

                    emit_struct(name.clone(), test)
                }

                _ => panic!(),
            };

            buffer.push_str(&record_fields);
        }

        buffer
    }

    // Generates the header of forward declarations & function definitions.
    fn generate_function_header(&mut self) -> String {
        let mut buffer = String::new();

        let body: Vec<String> = self
            .function_header
            .iter()
            .map(|(func_name, func_type, func_args, func_body)| {
                format!(
                    "{} {} ({}){{ {} }}",
                    func_type,
                    func_name,
                    func_args
                        .iter()
                        .map(|(arg_name, arg_type)| format!("{} {}", arg_type, arg_name))
                        .collect::<Vec<String>>()
                        .join(", "),
                    func_body
                )
            })
            .collect();

        buffer.push_str(body.join("\n").as_str());

        buffer
    }

    pub fn compile(&mut self, expression: &Expression) -> String {
        let mut buffer = String::new();

        let root_expr = self.compile_expression(expression);

        buffer.push_str("// Record header\n");
        buffer.push_str(&self.generate_record_header());
        buffer.push_str("\n// Function header\n");
        buffer.push_str(&self.generate_function_header());
        buffer.push_str("\n\n// Program\n");
        buffer.push_str(&root_expr);

        buffer
    }

    pub fn compile_expression(&mut self, expression: &Expression) -> String {
        match expression {
            Expression::Integer(value) => format!("{}", value),
            Expression::Float(value) => format!("{}", value),
            Expression::Str(value) => format!("\"{}\"", value),
            Expression::Boolean(value) => format!("{}", value),
            Expression::Variable(name) => format!("{}", name),

            Expression::Group(expression) => format!("({})", self.compile_expression(expression)),

            Expression::Unary(expression, operation) => {
                let unary_symbol = match operation {
                    ast::UnaryOperator::Minus => "-".to_string(),
                    ast::UnaryOperator::Bang => "!".to_string(),
                };

                emit_unary(unary_symbol, self.compile_expression(expression))
            }
            Expression::Binary(left, operation, right) => {
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
            Expression::Logical(left, operation, right) => {
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

            Expression::If(if_expr, then_expr, else_expr) => {
                format!(
                    "{} ? {} : {}",
                    self.compile_expression(if_expr),
                    self.compile_expression(then_expr),
                    self.compile_expression(else_expr)
                )
            }

            Expression::Let(name, var_type, value, body) => {
                // Is the last node not a declaration? Terminate it.
                let is_leaf = match body.borrow() {
                    Expression::Let(_, _, _, _) => false,
                    _ => true,
                };

                let is_func = match value.borrow() {
                    Expression::Function(_, _, _, _) => true,
                    _ => false,
                };

                if is_func {
                    format!(
                        "{} = {};\n {}{}{}",
                        resolver::get_function_pointer(name.clone(), var_type),
                        self.compile_expression(value),
                        if is_leaf { "return " } else { "" },
                        self.compile_expression(body),
                        if is_leaf { ";" } else { "" }
                    )
                } else {
                    format!(
                        "{} {} = {};\n {}{}{}",
                        resolver::get_native_type(var_type),
                        name,
                        self.compile_expression(value),
                        if is_leaf { "return " } else { "" },
                        self.compile_expression(body),
                        if is_leaf { ";" } else { "" }
                    )
                }
            }

            Expression::Function(_, func_type, arg_types, value) => {
                // Generate fresh name.
                let fresh_name = self.function.generate();
                let func_body = self.compile_expression(value);
                let arguments = arg_types
                    .iter()
                    .map(|(arg_name, arg_type)| {
                        (
                            arg_name.clone(),
                            resolver::get_native_type(arg_type.borrow()),
                        )
                    })
                    .collect();

                self.function_header.push((
                    fresh_name.clone(),
                    resolver::get_native_type(func_type),
                    arguments,
                    func_body,
                ));

                // All user-declared functions are a pointer to a function in the function header.
                fresh_name
            }

            Expression::Get(name, expr) => format!("(Get, name: '{}', parent: {})", name, expr),

            Expression::FunctionCall(name, arguments) => {
                let argument_list: Vec<String> = arguments
                    .iter()
                    .map(|arg| self.compile_expression(arg))
                    .collect();

                let function = self.compile_expression(name);

                emit_function_call(function, &argument_list, false)
            }

            // Just ignore record declarations, already handled in the record header.
            Expression::RecordDeclaration(_, _, body) => self.compile_expression(body),

            Expression::RecordInstance(name, fields) => {
                let field_list: Vec<String> = fields
                    .iter()
                    .map(|(field_name, field_value)| format!("({}: {})", field_name, field_value))
                    .collect();

                format!(
                    "(RecordInstance, name: {}, fields: [{}])",
                    name,
                    field_list.join(", ")
                )
            }
        }
    }
}
