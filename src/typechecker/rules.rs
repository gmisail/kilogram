use std::rc::Rc;

use crate::ast::UnaryOperator;

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
