use super::ast;
use super::scanner;
use super::token::{Token, TokenKind};

struct Parser {
    current: usize,
    tokens: Vec<Token>,
}

impl Parser {
    fn is_at_end(&self) -> bool {
        match self.tokens.get(self.current) {
            Some(t) => matches!(t.kind, TokenKind::Eof), 
            None => true,
        }
    }
}

pub fn parse(input: String) -> Result<ast::Expression, &'static str> {
    let tokens = scanner::scan(input)?;
    let context = Parser { current: 0, tokens };

    Ok(
        ast::Expression::Binary(
            Box::new(ast::Expression::Integer(1234)), 
            ast::BinaryOperator::Add, 
            Box::new(ast::Expression::Unary(Box::new(ast::Expression::Float(0.678)), ast::UnaryOperator::Minus))
        )
    )
}
