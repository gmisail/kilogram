use std::fmt::Display;

#[derive(Clone)]
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

#[derive(Clone)]
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

#[derive(Clone)]
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
                LogicalOperator::And => "&&",
                LogicalOperator::Or => "||",
            }
        )
    }
}

#[derive(Clone)]
pub enum Type {
    Base(String),               // string, int, ...
    Generic(String, Box<Type>), // array<string>, list<int>, ...
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Type::Base(name) => format!("(Type, name: {})", name),
                Type::Generic(name, sub_type) =>
                    format!("(GenericType, name: {}, sub_type: {})", name, sub_type),
            }
        )
    }
}

#[derive(Clone)]
pub enum Expression {
    // Literals
    Integer(i32),
    Float(f32),
    Str(String),
    Boolean(bool),
    Variable(String),

    Group(Box<Expression>),
    Get(String, Box<Expression>),

    FunctionCall(Box<Expression>, Vec<Box<Expression>>),

    // Operators
    Unary(Box<Expression>, UnaryOperator),
    Binary(Box<Expression>, BinaryOperator, Box<Expression>),
    Logical(Box<Expression>, LogicalOperator, Box<Expression>),

    // Control Flow
    // if  \/  then    \/   else   \/
    If(Box<Expression>, Box<Expression>, Box<Expression>),

    // Declarations
    Let(String, Type, Box<Expression>),
    Function(String, Type, Vec<(String, Type)>, Box<Expression>),
}

impl Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let value: String = match self {
            Expression::Integer(value) => format!("(Integer, value: '{}')", value),
            Expression::Float(value) => format!("(Float, value: '{}')", value),
            Expression::Str(value) => format!("(String, value: '{}')", value),
            Expression::Boolean(value) => format!("(Boolean, value: '{}')", value),
            Expression::Variable(name) => format!("(Variable, name: '{}')", name),

            Expression::Group(expression) => format!("(Group, value: {})", expression),

            Expression::Unary(expression, operation) => {
                format!("(Unary, operation: {}, value: {})", operation, expression)
            }
            Expression::Binary(left, operation, right) => format!(
                "(Binary, operation: {}, left: {}, right: {})",
                operation, left, right
            ),
            Expression::Logical(left, operation, right) => format!(
                "(Logical, operation: {}, left: {}, right: {})",
                operation, left, right
            ),

            Expression::If(if_expr, then_expr, else_expr) => format!(
                "(If, condition: {}, then: {}, else: {})",
                if_expr, then_expr, else_expr
            ),

            Expression::Let(name, var_type, value) => format!(
                "(Let, name: '{}', type: {}, value: {})",
                name, var_type, value
            ),
            Expression::Function(name, func_type, _, value) => format!(
                "(Function, name: '{}', type: {}, value: {})",
                name, func_type, value
            ),

            Expression::Get(name, expr) => format!("(Get, name: '{}', parent: {})", name, expr),

            Expression::FunctionCall(name, arguments) => {
                let argument_list: Vec<String> =
                    arguments.iter().map(|arg| arg.to_string()).collect();

                format!(
                    "(FunctionCall, name: {}, arguments: [{}])",
                    name,
                    argument_list.join(", ")
                )
            }
        };

        write!(f, "{}", value)
    }
}
