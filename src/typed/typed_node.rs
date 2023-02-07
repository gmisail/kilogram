use super::data_type::DataType;
use crate::ast::operator::{BinaryOperator, LogicalOperator, UnaryOperator};

use std::rc::Rc;

///
/// A type tree is an AST that contains type information. It is generated by the typechecker during the checking phase of the compiler.
///
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
    RecordInstance(String, Vec<(String, Box<TypedNode>)>),

    FunctionCall(Rc<DataType>, Box<TypedNode>, Vec<Box<TypedNode>>),

    Unary(Rc<DataType>, Box<TypedNode>, UnaryOperator),
    Binary(Rc<DataType>, Box<TypedNode>, BinaryOperator, Box<TypedNode>),
    Logical(
        Rc<DataType>,
        Box<TypedNode>,
        LogicalOperator,
        Box<TypedNode>,
    ),

    If(Rc<DataType>, Box<TypedNode>, Box<TypedNode>, Box<TypedNode>),

    Let(String, Rc<DataType>, Box<TypedNode>, Box<TypedNode>, bool),
    Function(
        Rc<DataType>,
        Rc<DataType>,
        Vec<(String, Rc<DataType>)>,
        Box<TypedNode>,
    ),
}
