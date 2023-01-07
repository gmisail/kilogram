use super::ast;
use super::token;
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

    fn peek_token(&self) -> Option<&token::Token> {
        self.tokens.get(self.current) 
    }

    fn next_token(&mut self) -> Option<&token::Token> {
        self.current += 1;
        self.peek_token()
    }

    fn match_token(&self, kind: token::TokenKind) -> bool {
        let peeked = self.peek_token();
        
        match peeked {
            Some(t) => t.kind == kind,
            None => false
        }
    }
}

pub fn parse(input: String) -> Result<ast::Expression, &'static str> {
    let tokens = scanner::scan(input)?;
    let mut context = Parser { current: 0, tokens };

    while !context.is_at_end() {
        println!("token: {}", context.peek_token().unwrap().kind);
        
        context.next_token();
    }

    Ok(
        ast::Expression::Binary(
            Box::new(ast::Expression::Integer(1234)), 
            ast::BinaryOperator::Add, 
            Box::new(ast::Expression::Unary(Box::new(ast::Expression::Float(0.678)), ast::UnaryOperator::Minus))
        )
    )
}
