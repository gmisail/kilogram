use std::fmt::Display;

use super::ast_type::AstType;
use crate::ast::operator::{BinaryOperator, LogicalOperator, UnaryOperator};

#[derive(Clone, Debug)]
pub enum UntypedNode {
    // Literals
    Integer(i32),
    Float(f32),
    Str(String),
    Boolean(bool),
    Variable(String),
    Group(Box<UntypedNode>),
    List(Vec<UntypedNode>),

    Get(String, Box<UntypedNode>),

    RecordDeclaration(
        String,
        Vec<(String, AstType)>,
        Vec<String>,
        Box<UntypedNode>,
    ),
    RecordInstance(String, Vec<AstType>, Vec<(String, UntypedNode)>),
    AnonymousRecord(Vec<(String, UntypedNode)>),

    EnumDeclaration(
        String,
        Vec<(String, Vec<AstType>)>,
        Vec<String>,
        Box<UntypedNode>,
    ),

    // Operators
    Unary(Box<UntypedNode>, UnaryOperator),
    Binary(Box<UntypedNode>, BinaryOperator, Box<UntypedNode>),
    Logical(Box<UntypedNode>, LogicalOperator, Box<UntypedNode>),

    // Control Flow
    If(Box<UntypedNode>, Box<UntypedNode>, Box<UntypedNode>),
    CaseOf(Box<UntypedNode>, Vec<(UntypedNode, UntypedNode)>),

    // Declarations
    Let(
        String,
        Option<AstType>,
        Box<UntypedNode>,
        Box<UntypedNode>,
        bool,
    ),

    Function(AstType, Vec<(String, AstType)>, Box<UntypedNode>),
    FunctionDeclaration(
        String,
        Vec<String>,
        AstType,
        Vec<(String, AstType)>,
        Box<UntypedNode>,
        Box<UntypedNode>,
    ),
    FunctionCall(Box<UntypedNode>, Vec<UntypedNode>),
    FunctionInstance(Box<UntypedNode>, Vec<AstType>),

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
            UntypedNode::List(elements) => {
                format!(
                    "(List, elements: [{:?}]",
                    elements
                        .iter()
                        .map(|element| format!("{element}"))
                        .collect::<Vec<String>>()
                )
            }

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
                "(Let, name: '{name}', value: {value}, body: {body}, is_recursive: {is_recursive})"
            ),

            UntypedNode::Function(_, _, value) => {
                format!("(Function, value: {value})")
            }

            UntypedNode::FunctionDeclaration(name, _, _, _, func_body, body) => {
                format!(
                    "(FunctionDeclaration, name: {name}, function_body: {func_body}, body: {body})"
                )
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

            UntypedNode::RecordDeclaration(name, fields, _, body) => {
                let field_list: Vec<String> = fields
                    .iter()
                    .map(|(field_name, field_type)| {
                        format!("{field_name} {}", field_type.to_string())
                    })
                    .collect();

                format!(
                    "(RecordDeclaration, name: {name}, fields: [{}], body: {body})",
                    field_list.join(", "),
                )
            }

            UntypedNode::RecordInstance(name, _, fields) => {
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

            UntypedNode::Extern(name, _, body) => {
                format!("(Extern, name: {name}, body: {body})")
            }

            UntypedNode::EnumDeclaration(name, _, _, body) => {
                format!("(EnumDeclaration, name: {name}, body: {body})")
            }

            UntypedNode::FunctionInstance(base, sub_types) => {
                let generics: Vec<String> = sub_types
                    .iter()
                    .map(|sub_type| sub_type.to_string())
                    .collect();

                format!("{base}[{}]", generics.join(", "))
            }

            UntypedNode::CaseOf(_, _) => todo!(),
        };

        write!(f, "{value}")
    }
}
