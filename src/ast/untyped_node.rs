use std::fmt::Display;

use super::{
    ast_type::AstType,
    operator::{BinaryOperator, LogicalOperator, UnaryOperator},
};

#[derive(Clone)]
pub enum UntypedNode {
    // Literals
    Integer(i32),
    Float(f32),
    Str(String),
    Boolean(bool),
    Variable(String),
    Group(Box<UntypedNode>),

    Get(String, Box<UntypedNode>),

    RecordDeclaration(String, Vec<(String, AstType)>, Box<UntypedNode>),
    RecordInstance(String, Vec<(String, UntypedNode)>),
    AnonymousRecord(Vec<(String, UntypedNode)>),

    FunctionCall(Box<UntypedNode>, Vec<UntypedNode>),

    // Operators
    Unary(Box<UntypedNode>, UnaryOperator),
    Binary(Box<UntypedNode>, BinaryOperator, Box<UntypedNode>),
    Logical(Box<UntypedNode>, LogicalOperator, Box<UntypedNode>),

    // Control Flow
    // if  \/  then    \/   else   \/
    If(Box<UntypedNode>, Box<UntypedNode>, Box<UntypedNode>),

    // Declarations
    Let(
        String,
        Option<AstType>,
        Box<UntypedNode>,
        Box<UntypedNode>,
        bool,
    ),
    Function(String, AstType, Vec<(String, AstType)>, Box<UntypedNode>),

    Extern(String, AstType, Box<UntypedNode>),
}

impl Display for UntypedNode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let value: String = match self {
            UntypedNode::Integer(value) => format!("(Integer, value: '{value}')"),
            UntypedNode::Float(value) => format!("(Float, value: '{value}')"),
            UntypedNode::Str(value) => format!("(String, value: '{value}')"),
            UntypedNode::Boolean(value) => format!("(Boolean, value: '{value}')"),
            UntypedNode::Variable(name) => format!("(Variable, name: '{name}')"),

            UntypedNode::Group(expr) => format!("(Group, value: {expr})"),

            UntypedNode::Unary(expr, operation) => {
                format!("(Unary, operation: {operation}, value: {expr})")
            }
            UntypedNode::Binary(left, operation, right) => {
                format!("(Binary, operation: {operation}, left: {left}, right: {right})")
            }
            UntypedNode::Logical(left, operation, right) => {
                format!("(Logical, operation: {operation}, left: {left}, right: {right})")
            }

            UntypedNode::If(if_expr, then_expr, else_expr) => {
                format!("(If, condition: {if_expr}, then: {then_expr}, else: {else_expr})")
            }

            UntypedNode::Let(name, _, value, body, is_recursive) => format!(
                "(Let, name: '{name}', value: {value}, body: {body}, is_recursive: {is_recursive})",
            ),

            UntypedNode::Function(name, ast_type, _, value) => {
                format!("(Function, name: '{name}', type: {ast_type}, value: {value})")
            }

            UntypedNode::Get(name, expr) => format!("(Get, name: '{name}', parent: {expr})"),

            UntypedNode::FunctionCall(name, arguments) => {
                let argument_list: Vec<String> =
                    arguments.iter().map(|arg| arg.to_string()).collect();

                format!(
                    "(FunctionCall, name: {name}, arguments: [{}])",
                    argument_list.join(", ")
                )
            }

            UntypedNode::RecordDeclaration(name, fields, body) => {
                let field_list: Vec<String> = fields
                    .iter()
                    .map(|(field_name, field_type)| format!("({field_name}: {field_type})"))
                    .collect();

                format!(
                    "(RecordDeclaration, name: {name}, fields: [{}], body: {body})",
                    field_list.join(", "),
                )
            }

            UntypedNode::RecordInstance(name, fields) => {
                let field_list: Vec<String> = fields
                    .iter()
                    .map(|(field_name, field_value)| format!("({field_name}: {field_value})"))
                    .collect();

                format!(
                    "(RecordInstance, name: {name}, fields: [{}])",
                    field_list.join(", ")
                )
            }

            UntypedNode::AnonymousRecord(fields) => {
                let field_list: Vec<String> = fields
                    .iter()
                    .map(|(field_name, field_value)| format!("({field_name}: {field_value})"))
                    .collect();

                format!("(AnonymousRecord, fields: [{}])", field_list.join(", "))
            }

            UntypedNode::Extern(name, extern_type, body) => {
                format!("(Extern, name: {name}, type: {extern_type}, body: {body})")
            }
        };

        write!(f, "{value}")
    }
}
