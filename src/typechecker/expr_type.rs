use crate::ast;

pub enum Type {
    Integer,
    Float,
    Str,
    Boolean,

    Function(Vec<Box<Type>>, Box<Type>),
    Record(String, Vec<(String, Box<Type>)>),
}

pub fn from_literal(literal: ast::Expression) -> Result<Type, String> {
    match literal {
        ast::Expression::Integer(_) => Ok(Type::Integer),
        ast::Expression::Float(_) => Ok(Type::Float),
        ast::Expression::Str(_) => Ok(Type::Str),
        ast::Expression::Boolean(_) => Ok(Type::Boolean),
        _ => Err("TODO".to_string()),
    }
}
