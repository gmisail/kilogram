use crate::ast::Expression;

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

    fn advance_token(&mut self) -> Option<&Token> {
        let peeked_token = self.tokens.get(self.current);

        match peeked_token {
            Some(token) => {
                self.current += 1;
                Some(token)
            },
            None => None
        }
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

    fn parse_primary(&mut self) -> Result<Expression, String> {
        let token = match self.advance_token() {
            Some(t) => t,
            None => return Err(String::from("Failed to load token while parsing primary expression."))
        };

        match &token.kind {
            TokenKind::Boolean(value) => Ok(Expression::Boolean(*value)),
            TokenKind::Str(value) => Ok(Expression::Str(value.clone())),
            TokenKind::Integer(value) => Ok(Expression::Integer(*value)),
            TokenKind::Float(value) => Ok(Expression::Float(*value)),
            TokenKind::Identifier(name) => Ok(Expression::Variable(name.clone())),
            TokenKind::LeftParen => {
                let grouped_expr = Expression::Group(Box::new(self.parse_expression()?));
                self.expect_token(TokenKind::RightParen)?;
                Ok(grouped_expr)
            }
            _ => Err(format!("Failed to parse expression beginning with token '{}'", token.kind))
        }
    }

    fn parse_expression(&mut self) -> Result<Expression, String> {
        return self.parse_primary()
    }
}

pub fn parse(input: String) -> Result<ast::Expression, &'static str> {
    let tokens = scanner::scan(input)?;
    let mut context = Parser { current: 0, tokens };

    while !context.is_at_end() {
        println!("{}", match context.parse_primary() {
            Ok(node) => format!("{}", node),
            Err(e) => e
        });
    }

    Err("...")
}
