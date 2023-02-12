use std::collections::HashMap;
use std::rc::Rc;

use crate::ast::operator::{BinaryOperator, LogicalOperator, UnaryOperator};
use crate::typed::data_type::DataType;
use crate::typed::typed_node::TypedNode;
use crate::compiler::free::find_free;

use self::builder::StructBuilder;
use self::emitter::emit_if;
use self::{
    emitter::{emit_binary, emit_unary},
    generator::FunctionGenerator,
};

mod builder;
mod emitter;
mod generator;
mod resolver;
mod free;

struct FunctionDefinition {
    name: String,
    data_type: Rc<DataType>,
    arguments: Vec<(String, String)>,
    body: String,
    captures: HashMap<String, Rc<DataType>>,
}

pub struct Compiler {
    function: FunctionGenerator,
    function_header: Vec<FunctionDefinition>,
    record_types: HashMap<String, Rc<DataType>>
}

// TODO: create a table of symbols so we don't need to make new strings every time

impl Compiler {
    pub fn new(record_types: HashMap<String, Rc<DataType>>) -> Self {
        Compiler {
            function: FunctionGenerator::new(),
            function_header: Vec::new(),
            record_types
        }
    }

    // Wrapper over the resolver module
    fn resolve_type(&self, var_name: &String, var_type: Rc<DataType>) -> String {
        match *var_type {
            DataType::Function(_, _) => format!("KiloFunction* {var_name}"),
            _ => format!("{} {var_name}", resolver::get_native_type(var_type)),
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

            for (field_name, field_type) in record_fields {
                record_struct.field(field_name.clone(), resolver::get_native_type(field_type.clone()));
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

        for def in &self.function_header {
            let mut args = def
                .arguments
                .iter()
                .map(|(_, arg_type)| arg_type.clone())
                .collect::<Vec<String>>();

            args.push(format!("{}_env* env", def.name));
            let arg_buffer = args.join(", ");
            
            let mut func_env = StructBuilder::new(format!("{}_env", def.name));

            for (field_name, field_type) in &def.captures {
                func_env.field(field_name.clone(), resolver::get_native_type(field_type.clone()));
            }

            // Brings variables from environment back into scope.
            let hydrate_env = def.captures
                .iter()
                .map(|(env_name, env_type)| format!("{} {env_name} = env->{env_name};", resolver::get_native_type(env_type.clone())))
                .collect::<Vec<String>>()
                .join("\n");

            let func_buffer = format!(
                "{} ({}){{\n{}\n{}\n}}",
                self.resolve_type(&def.name, def.data_type.clone()),
                arg_buffer,
                hydrate_env,
                def.body
            );

            buffer.push_str(&func_env.build());
            buffer.push_str(&func_env.build_constructor());
            buffer.push('\n');
            buffer.push_str(&func_buffer);
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
        value: &TypedNode,
        body: &TypedNode,
    ) -> String {
        // Is the last node not a declaration? Return its value.
        let is_leaf = !matches!(*body, TypedNode::Let(..));

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
        arg_types: &[(String, Rc<DataType>)],
        value: &TypedNode,
        free_vars: HashMap<String, Rc<DataType>>
    ) -> String {
        // Generate fresh name.
        let fresh_name = self.function.generate();
        let is_leaf = !matches!(*value, TypedNode::Let(..));

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

        let create_env = format!("_create_{fresh_name}_env({})", free_vars
            .keys()
            .cloned()
            .collect::<Vec<String>>()
            .join(",")
        );

        self.function_header.push(FunctionDefinition {
            name: fresh_name.clone(),
            data_type: func_type.clone(),
            arguments,
            body: func_body,
            captures: free_vars
        });

        // All user-declared functions are a pointer to a function in the function header.
        format!("function_create({fresh_name}, {create_env})")
    }

    /// Generates a function call given a function name and arguments.
    ///
    /// * `name`:
    /// * `arguments`:
    fn compile_function_call(&mut self, name: &TypedNode, arguments: &[TypedNode]) -> String {
        let argument_list: Vec<String> = arguments
            .iter()
            .map(|arg| self.compile_expression(arg))
            .collect();

        let base_type = match name {
            TypedNode::Variable(func_type, ..) => func_type,
            _ => panic!("Do not support calling non-variables yet"),
        };

        let func_type = resolver::get_function_pointer("".to_string(), base_type.clone());
        let function = self.compile_expression(name);

        format!(
            "(({func_type}) {function}->body)({}, {function}->env)",
            argument_list.join(", "),
        )
    }

    fn compile_record_instance(&mut self, name: &String, fields: &[(String, TypedNode)]) -> String {
        let mut buffer = String::new();

        buffer.push_str(format!("_create_{name}(").as_str());

        buffer.push_str(
            fields
                .iter()
                .map(|(_, field_value)| self.compile_expression(field_value))
                .collect::<Vec<String>>()
                .join(", ")
                .as_str(),
        );

        buffer.push(')');

        buffer
    }

    /// Compiles an expression.
    ///
    /// * `expression`:
    pub fn compile_expression(&mut self, expression: &TypedNode) -> String {
        match expression {
            TypedNode::Integer(_, value) => format!("{value}"),
            TypedNode::Float(_, value) => format!("{value}"),
            TypedNode::Str(_, value) => format!("string_create(\"{value}\")"),
            TypedNode::Boolean(_, value) => format!("{value}"),
            TypedNode::Variable(_, name) => name.clone(),

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
                let free_vars = find_free(expression); 
                self.compile_function(func_type, arg_types, value, free_vars)
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
