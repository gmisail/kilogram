use crate::ast::{Expression, self};

use self::emitter::{emit_unary, emit_binary};

mod emitter;

pub struct Compiler {
    
}

// TODO: create a table of symbols so we don't need to make new strings every time

impl Compiler {
    pub fn new() -> Self {
        Compiler {  }
    } 

    pub fn compile_expression(&self, expression: &Expression) -> String {
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
                    ast::UnaryOperator::Bang => "!".to_string()
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

                emit_binary(self.compile_expression(left), binary_symbol, self.compile_expression(right))
            },
            Expression::Logical(left, operation, right) => { 
                let logical_symbol = match operation {
                    ast::LogicalOperator::And => "&&",
                    ast::LogicalOperator::Or => "||"
                }; 

                emit_binary(self.compile_expression(left), logical_symbol.to_string(), self.compile_expression(right))
            },

            Expression::If(if_expr, then_expr, else_expr) => format!(
                "(If, condition: {}, then: {}, else: {})",
                if_expr, then_expr, else_expr
            ),

            Expression::Let(name, var_type, value, body) => format!(
                "(Let, name: '{}', type: {}, value: {}, body: {})",
                name, var_type, value, body
            ),

            Expression::Function(name, func_type, _, value) => format!(
                "(Function, name: '{}', type: {}, value: {})",
                name, func_type, value
            ),

            Expression::Get(name, expr) => format!("(Get, name: '{}', parent: {})", name, expr),

            Expression::FunctionCall(name, arguments) => {
                let argument_list: Vec<String> =
                    arguments.iter().map(|arg| arg.to_string()).collect();

                format!(
                    "(FunctionCall, name: {}, arguments: [{}])",
                    name,
                    argument_list.join(", ")
                )
            }

            Expression::RecordDeclaration(name, fields, body) => {
                let field_list: Vec<String> = fields
                    .iter()
                    .map(|(field_name, field_type)| format!("({}: {})", field_name, field_type))
                    .collect();

                format!(
                    "(RecordDeclaration, name: {}, fields: [{}], body: {})",
                    name,
                    field_list.join(", "),
                    body
                )
            }

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
