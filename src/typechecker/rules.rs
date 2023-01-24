use std::rc::Rc;

use crate::ast::{BinaryOperator, LogicalOperator, UnaryOperator};

use super::expr_type::Type;

pub fn check_unary(operator: &UnaryOperator, value_type: Rc<Type>) -> bool {
    match operator {
        UnaryOperator::Bang => *value_type == Type::Boolean,
        UnaryOperator::Minus => match *value_type {
            Type::Integer | Type::Float => true,
            _ => false,
        },
    }
}

pub fn check_binary(operator: &BinaryOperator, left_type: Rc<Type>, right_type: Rc<Type>) -> bool {
    match operator {
        // TODO: revise this such that we can work with floats + integers
        &BinaryOperator::Add
        | &BinaryOperator::Sub
        | &BinaryOperator::Mult
        | &BinaryOperator::Div
        | &BinaryOperator::Greater
        | &BinaryOperator::GreaterEq
        | &BinaryOperator::Less
        | &BinaryOperator::LessEq => {
            *left_type == *right_type
                && match *left_type {
                    Type::Integer | Type::Float => true,
                    _ => false,
                }
        }
        &BinaryOperator::Equality | &BinaryOperator::NotEqual => *left_type == *right_type,
    }
}

pub fn check_logical(_: &LogicalOperator, left_type: Rc<Type>, right_type: Rc<Type>) -> bool {
    *left_type == Type::Boolean && *right_type == Type::Boolean
}
