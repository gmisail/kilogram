use super::data_type::DataType;
use crate::ast::operator::{BinaryOperator, LogicalOperator, UnaryOperator};

use std::collections::BTreeMap;
use std::rc::Rc;

///
/// A type tree is an AST that contains type information. It is generated by the typechecker during the checking phase of the compiler.
///
#[derive(Clone)]
pub enum TypedNode {
    // Literals
    Integer(Rc<DataType>, i32),
    Float(Rc<DataType>, f32),
    Str(Rc<DataType>, String),
    Boolean(Rc<DataType>, bool),
    Variable(Rc<DataType>, String),
    Group(Rc<DataType>, Box<TypedNode>),

    Get(Rc<DataType>, String, Box<TypedNode>),

    RecordDeclaration(String, Vec<(String, Rc<DataType>)>, Box<TypedNode>),
    RecordInstance(String, Vec<(String, TypedNode)>),

    Let(String, Rc<DataType>, Box<TypedNode>, Box<TypedNode>, bool),
    FunctionCall(Rc<DataType>, Box<TypedNode>, Vec<TypedNode>),

    Unary(Rc<DataType>, Box<TypedNode>, UnaryOperator),
    Binary(Rc<DataType>, Box<TypedNode>, BinaryOperator, Box<TypedNode>),
    Logical(
        Rc<DataType>,
        Box<TypedNode>,
        LogicalOperator,
        Box<TypedNode>,
    ),

    If(Rc<DataType>, Box<TypedNode>, Box<TypedNode>, Box<TypedNode>),

    Function(
        Rc<DataType>,
        Rc<DataType>,
        Vec<(String, Rc<DataType>)>,
        Box<TypedNode>,
    ),

    Extern(String, Rc<DataType>, Box<TypedNode>),

    EnumInstance(Rc<DataType>, String, Vec<TypedNode>),
}

impl TypedNode {
    pub fn get_type(&self) -> Rc<DataType> {
        match self {
            TypedNode::Integer(t, _)
            | TypedNode::Float(t, _)
            | TypedNode::Str(t, _)
            | TypedNode::Boolean(t, _)
            | TypedNode::Variable(t, _)
            | TypedNode::Group(t, _)
            | TypedNode::Unary(t, _, _)
            | TypedNode::Binary(t, _, _, _)
            | TypedNode::Logical(t, _, _, _)
            | TypedNode::If(t, _, _, _)
            | TypedNode::Extern(_, t, _)
            | TypedNode::FunctionCall(t, _, _)
            | TypedNode::EnumInstance(t, _, _) => t.clone(),

            TypedNode::Let(_, _, _, body, _)
            | TypedNode::Function(_, _, _, body)
            | TypedNode::RecordDeclaration(_, _, body) => body.get_type(),

            TypedNode::Get(_, _, _) => todo!(),

            TypedNode::RecordInstance(name, fields) => {
                let field_types = fields
                    .iter()
                    .map(|(field_name, field_type)| (field_name.clone(), field_type.get_type()))
                    .collect::<Vec<(String, Rc<DataType>)>>();

                let mut ordered_fields = BTreeMap::new();
                for (field_name, field_type) in field_types {
                    ordered_fields.insert(field_name, field_type);
                }

                Rc::new(DataType::Record(name.clone(), ordered_fields))
            }
        }
    }
}
