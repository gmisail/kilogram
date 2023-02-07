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
    RecordInstance(String, Vec<(String, Box<UntypedNode>)>),

    FunctionCall(Box<UntypedNode>, Vec<Box<UntypedNode>>),

    // Operators
    Unary(Box<UntypedNode>, UnaryOperator),
    Binary(Box<UntypedNode>, BinaryOperator, Box<UntypedNode>),
    Logical(Box<UntypedNode>, LogicalOperator, Box<UntypedNode>),

    // Control Flow
    // if  \/  then    \/   else   \/
    If(Box<UntypedNode>, Box<UntypedNode>, Box<UntypedNode>),

    // Declarations
    Let(String, AstType, Box<UntypedNode>, Box<UntypedNode>, bool),
    Function(String, AstType, Vec<(String, AstType)>, Box<UntypedNode>),
}

impl Display for UntypedNode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let value: String = match self {
            UntypedNode::Integer(value) => format!("(Integer, value: '{}')", value),
            UntypedNode::Float(value) => format!("(Float, value: '{}')", value),
            UntypedNode::Str(value) => format!("(String, value: '{}')", value),
            UntypedNode::Boolean(value) => format!("(Boolean, value: '{}')", value),
            UntypedNode::Variable(name) => format!("(Variable, name: '{}')", name),

            UntypedNode::Group(UntypedNode) => format!("(Group, value: {})", UntypedNode),

            UntypedNode::Unary(UntypedNode, operation) => {
                format!("(Unary, operation: {}, value: {})", operation, UntypedNode)
            }
            UntypedNode::Binary(left, operation, right) => format!(
                "(Binary, operation: {}, left: {}, right: {})",
                operation, left, right
            ),
            UntypedNode::Logical(left, operation, right) => format!(
                "(Logical, operation: {}, left: {}, right: {})",
                operation, left, right
            ),

            UntypedNode::If(if_expr, then_expr, else_expr) => format!(
                "(If, condition: {}, then: {}, else: {})",
                if_expr, then_expr, else_expr
            ),

            UntypedNode::Let(name, var_AstType, value, body, is_recursive) => format!(
                "(Let, name: '{}', type: {}, value: {}, body: {}, is_recursive: {})",
                name, var_AstType, value, body, is_recursive
            ),

            UntypedNode::Function(name, func_AstType, _, value) => format!(
                "(Function, name: '{}', type: {}, value: {})",
                name, func_AstType, value
            ),

            UntypedNode::Get(name, expr) => format!("(Get, name: '{}', parent: {})", name, expr),

            UntypedNode::FunctionCall(name, arguments) => {
                let argument_list: Vec<String> =
                    arguments.iter().map(|arg| arg.to_string()).collect();

                format!(
                    "(FunctionCall, name: {}, arguments: [{}])",
                    name,
                    argument_list.join(", ")
                )
            }

            UntypedNode::RecordDeclaration(name, fields, body) => {
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

            UntypedNode::RecordInstance(name, fields) => {
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
        };

        write!(f, "{}", value)
    }
}
