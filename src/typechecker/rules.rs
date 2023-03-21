use std::rc::Rc;

use crate::ast::operator::{BinaryOperator, LogicalOperator, UnaryOperator};
use crate::ast::typed::data_type::DataType;

pub fn check_unary(operator: &UnaryOperator, value_type: Rc<DataType>) -> bool {
    match operator {
        UnaryOperator::Bang => *value_type == DataType::Boolean,
        UnaryOperator::Minus => matches!(*value_type, DataType::Integer | DataType::Float),
    }
}

pub fn check_binary(
    operator: &BinaryOperator,
    left_type: Rc<DataType>,
    right_type: Rc<DataType>,
) -> bool {
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
            *left_type == *right_type && matches!(*left_type, DataType::Integer | DataType::Float)
        }
        &BinaryOperator::Equality | &BinaryOperator::NotEqual => *left_type == *right_type,
    }
}

pub fn check_logical(
    _: &LogicalOperator,
    left_type: Rc<DataType>,
    right_type: Rc<DataType>,
) -> bool {
    *left_type == DataType::Boolean && *right_type == DataType::Boolean
}
