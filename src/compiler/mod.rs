use std::collections::HashMap;
use std::rc::Rc;

use crate::ast::operator::{BinaryOperator, LogicalOperator, UnaryOperator};
use crate::typed::data_type::DataType;
use crate::typed::typed_node::TypedNode;

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
    function_header: Vec<(String, Rc<DataType>, Vec<(String, String)>, String)>,
    record_types: HashMap<String, Rc<DataType>>,
}

// TODO: create a table of symbols so we don't need to make new strings every time

impl Compiler {
    pub fn new(record_types: HashMap<String, Rc<DataType>>) -> Self {
        Compiler {
            function: FunctionGenerator::new(),
            function_header: Vec::new(),
            record_types,
        }
    }

    // Wrapper over the resolver module
    fn resolve_type(&self, var_name: &String, var_type: Rc<DataType>) -> String {
        match *var_type {
            DataType::Function(_, _) => format!("KiloFunction* {}", var_name),
            _ => format!("{} {}", resolver::get_native_type(var_type), var_name),
        }
    }

    // Generates the header of forward declarations and the struct definitions.
    fn generate_record_header(&self) -> String {
        let mut buffer = String::new();

        // TODO: have forward declarations before defining records

        for (name, record_type) in &self.record_types {
            let record_fields = match &**record_type {
                DataType::Record(_, fields) => fields,

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

            let result = match &**func_type {
                DataType::Function(base_func_args, _) => {
                    let func_pointer = format!(
                        "(*{}({}))",
                        func_name,
                        base_func_args
                            .iter()
                            .map(|t| resolver::get_native_type(t.clone()))
                            .collect::<Vec<String>>()
                            .join(", ")
                    );

                    format!(
                        "{} {{\n{}\n}}",
                        self.resolve_type(&func_pointer, func_type.clone()),
                        func_body
                    )
                }
                _ => format!(
                    "{} ({}){{\n{}\n}}",
                    self.resolve_type(func_name, func_type.clone()),
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

    pub fn compile(&mut self, expression: &TypedNode) -> String {
        let mut buffer = String::new();

        let root_expr = self.compile_expression(expression);

        buffer.push_str("#include<stdio.h>\n");
        buffer.push_str("#include<stdlib.h>\n");

        buffer.push_str("\n// Record header\n");
        buffer.push_str(&self.generate_record_header());
        buffer.push_str("\n// Function header\n");
        buffer.push_str(&self.generate_function_header());
        buffer.push_str("\n// Program\n");

        // Inject the source into the main function
        buffer.push_str("int main(int argc, char** argv){\n");

        // Inject any Kilogram-specific C code here.
        buffer.push_str(&root_expr);

        // Destructors
        buffer.push_str("\n}");

        buffer
    }

    fn compile_unary(&mut self, expression: &TypedNode, operation: &UnaryOperator) -> String {
        let unary_symbol = match operation {
            UnaryOperator::Minus => "-".to_string(),
            UnaryOperator::Bang => "!".to_string(),
        };

        emit_unary(unary_symbol, self.compile_expression(expression))
    }

    fn compile_binary(
        &mut self,
        left: &TypedNode,
        right: &TypedNode,
        operation: &BinaryOperator,
    ) -> String {
        let binary_symbol = match operation {
            BinaryOperator::Add => "+".to_string(),
            BinaryOperator::Sub => "-".to_string(),
            BinaryOperator::Mult => "*".to_string(),
            BinaryOperator::Div => "/".to_string(),
            BinaryOperator::Equality => "==".to_string(),
            BinaryOperator::NotEqual => "!=".to_string(),
            BinaryOperator::Greater => ">".to_string(),
            BinaryOperator::GreaterEq => ">=".to_string(),
            BinaryOperator::Less => "<".to_string(),
            BinaryOperator::LessEq => "<=".to_string(),
        };

        emit_binary(
            self.compile_expression(left),
            binary_symbol,
            self.compile_expression(right),
        )
    }

    fn compile_logical(
        &mut self,
        left: &TypedNode,
        right: &TypedNode,
        operation: &LogicalOperator,
    ) -> String {
        let logical_symbol = match operation {
            LogicalOperator::And => "&&",
            LogicalOperator::Or => "||",
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
        var_type: &Rc<DataType>,
        value: &Box<TypedNode>,
        body: &Box<TypedNode>,
    ) -> String {
        // Is the last node not a declaration? Return it.
        let is_leaf = match **body {
            TypedNode::Let(..) => false,
            _ => true,
        };

        format!(
            "{} = {};\n{}{}{}",
            self.resolve_type(name, var_type.clone()),
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
        func_type: &Rc<DataType>,
        arg_types: &Vec<(String, Rc<DataType>)>,
        value: &Box<TypedNode>,
    ) -> String {
        // Generate fresh name.
        let fresh_name = self.function.generate();
        let is_leaf = match **value {
            TypedNode::Let(..) => false,
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
                    self.resolve_type(arg_name, arg_type.clone()),
                )
            })
            .collect();

        self.function_header
            .push((fresh_name.clone(), func_type.clone(), arguments, func_body));

        // All user-declared functions are a pointer to a function in the function header.
        format!("function_create({}, NULL)", fresh_name)
    }

    /// Generates a function call given a function name and arguments.
    ///
    /// * `name`:
    /// * `arguments`:
    fn compile_function_call(
        &mut self,
        name: &TypedNode,
        arguments: &Vec<Box<TypedNode>>,
    ) -> String {
        let argument_list: Vec<String> = arguments
            .iter()
            .map(|arg| self.compile_expression(arg))
            .collect();

        let _is_local = match name {
            TypedNode::Variable(_, _) => true,
            _ => panic!("Do not support calling non-variables yet"),
        };

        let function = self.compile_expression(name);

        emit_function_call(function, &argument_list, false)
    }

    fn compile_record_instance(
        &mut self,
        name: &String,
        fields: &Vec<(String, Box<TypedNode>)>,
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
    pub fn compile_expression(&mut self, expression: &TypedNode) -> String {
        match expression {
            TypedNode::Integer(_, value) => format!("{}", value),
            TypedNode::Float(_, value) => format!("{}", value),
            TypedNode::Str(_, value) => format!("string_create(\"{}\")", value),
            TypedNode::Boolean(_, value) => format!("{}", value),
            TypedNode::Variable(_, name) => format!("{}", name),

            TypedNode::Group(_, expr) => format!("({})", self.compile_expression(expr)),

            TypedNode::Unary(_, expr, operation) => self.compile_unary(expr, operation),
            TypedNode::Binary(_, left, operation, right) => {
                self.compile_binary(left, right, operation)
            }
            TypedNode::Logical(_, left, operation, right) => {
                self.compile_logical(left, right, operation)
            }

            TypedNode::If(_, if_expr, then_expr, else_expr) => emit_if(
                self.compile_expression(if_expr),
                self.compile_expression(then_expr),
                self.compile_expression(else_expr),
            ),

            TypedNode::Let(name, var_type, value, body, _) => {
                self.compile_let(name, var_type, value, body)
            }

            TypedNode::Function(_, func_type, arg_types, value) => {
                self.compile_function(func_type, arg_types, value)
            }

            TypedNode::Get(_, _, _) => "GET TODO".to_string(),

            TypedNode::FunctionCall(_, name, arguments) => {
                self.compile_function_call(name, arguments)
            }

            // Just ignore record declarations, already handled in the record header.
            TypedNode::RecordDeclaration(_, _, body) => self.compile_expression(body),

            TypedNode::RecordInstance(name, fields) => self.compile_record_instance(name, fields),
        }
    }
}
