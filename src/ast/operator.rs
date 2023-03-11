use std::fmt::Display;

#[derive(Clone, Debug)]
pub enum UnaryOperator {
    Minus,
    Bang,
}

impl Display for UnaryOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                UnaryOperator::Minus => "-",
                UnaryOperator::Bang => "!",
            }
        )
    }
}

#[derive(Clone, Debug)]
pub enum BinaryOperator {
    Add,
    Sub,
    Mult,
    Div,
    Equality,
    NotEqual,
    Greater,
    GreaterEq,
    Less,
    LessEq,
}

impl Display for BinaryOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                BinaryOperator::Add => "+",
                BinaryOperator::Sub => "-",
                BinaryOperator::Mult => "*",
                BinaryOperator::Div => "/",
                BinaryOperator::Equality => "==",
                BinaryOperator::NotEqual => "!=",
                BinaryOperator::Greater => ">",
                BinaryOperator::GreaterEq => ">=",
                BinaryOperator::Less => "<",
                BinaryOperator::LessEq => "<=",
            }
        )
    }
}

#[derive(Clone, Debug)]
pub enum LogicalOperator {
    And,
    Or,
}

impl Display for LogicalOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                LogicalOperator::And => "and",
                LogicalOperator::Or => "or",
            }
        )
    }
}
