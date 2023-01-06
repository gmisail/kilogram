#[derive(Clone)]
pub enum UnaryOperator {
    Minus,
    Bang,
}

#[derive(Clone)]
pub enum BinaryOperator {
    Minus,
    Bang,
}

#[derive(Clone)]
pub enum LogicalOperator {
    And,
    Or,
}

#[derive(Clone)]
pub enum Type {
    Base(String),               // string, int, ...
    Generic(String, Box<Type>), // array<string>, list<int>, ...
}

#[derive(Clone)]
pub enum Expression {
    // Literals
    Integer(i32),
    Float(f32),
    Str(String),
    Boolean(bool),
    Variable(String),

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
