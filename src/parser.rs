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

    fn peek_token(&self) -> Option<&Token> {
        self.tokens.get(self.current) 
    }

    fn next_token(&mut self) -> Option<&Token> {
        self.current += 1;
        self.peek_token()
    }

    fn match_token(&self, kind: &TokenKind) -> bool {
        let peeked = self.peek_token();
        
        match peeked {
            Some(t) => t.kind == *kind,
            None => false
        }
    }

    fn expect_token(&mut self, kind: TokenKind) -> Result<&Token, String> {
        let peeked_token = match self.peek_token() {
            Some(t) => t,
            None => return Err(format!("Expected token with kind '{}', got nothing instead.", kind))
        };

        if self.match_token(&kind) {
            Ok(peeked_token)
        } else {
            Err(format!("Expected token with kind '{}', got '{}' instead.", kind, peeked_token.kind))
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
