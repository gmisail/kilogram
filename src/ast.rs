#[derive(Clone)]
pub enum UnaryOperator {
    Minus, Bang
}

#[derive(Clone)]
pub enum BinaryOperator {
    Minus, Bang
}

#[derive(Clone)]
pub enum LogicalOperator {
    And, Or 
}

#[derive(Clone)]
pub enum Type {
    Base(String),           // string, int, ...
    Generic(String, Type)   // array<string>, list<int>, ...
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
    Unary(Expression, UnaryOperator),                   
    Binary(Expression, BinaryOperator, Expression),
    Logical(Expression, LogicalOperator, Expression),

    // Control Flow
    // if  \/  then    \/   else   \/
    If(Expression, Expression, Expression),

    // Declarations
    Let(String, Type, Expression),
    Function(String, Type, &[(String, Type)], Expression)
}
