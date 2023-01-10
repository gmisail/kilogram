use crate::ast::Expression;

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

    fn peek_token(&self) -> Option<&Token> {
        self.tokens.get(self.current)
    }

    fn advance_token(&mut self) {
        self.current += 1;
    }

    fn match_token(&mut self, kind: &TokenKind) -> bool {
        let peeked = self.peek_token();

        match peeked {
            Some(t) => t.kind == *kind,
            None => false,
        }
    }

    fn expect_token(&mut self, kind: TokenKind) -> Result<Option<&Token>, String> {
        if self.match_token(&kind) {
            self.advance_token();

            Ok(self.peek_token())
        } else {
            let actual_kind = match self.peek_token() {
                Some(t) => &t.kind,
                None => &TokenKind::Eof,
            };

            Err(format!(
                "Expected token with kind '{}', got '{}' instead.",
                kind, actual_kind
            ))
        }
    }

    fn parse_boolean(&mut self, value: bool) -> Result<Expression, String> {
        self.advance_token();
        Ok(Expression::Boolean(value))
    }

    fn parse_string(&mut self, value: String) -> Result<Expression, String> {
        self.advance_token();
        Ok(Expression::Str(value))
    }

    fn parse_integer(&mut self, value: i32) -> Result<Expression, String> {
        self.advance_token();
        Ok(Expression::Integer(value))
    }

    fn parse_float(&mut self, value: f32) -> Result<Expression, String> {
        self.advance_token();
        Ok(Expression::Float(value))
    }

    fn parse_identifier(&mut self, name: String) -> Result<Expression, String> {
        self.advance_token();
        Ok(Expression::Variable(name))
    }

    fn parse_group(&mut self) -> Result<Expression, String> {
        self.advance_token();

        // ( inner_expr )
        let inner_expr = Expression::Group(Box::new(self.parse_expression()?));
        self.expect_token(TokenKind::RightParen)?;

        Ok(inner_expr)
    }

    fn parse_primary(&mut self) -> Result<Expression, String> {
        match self.peek_token() {
            Some(t) => match &t.kind {
                TokenKind::Boolean(value) => self.parse_boolean(*value),
                TokenKind::Str(value) => self.parse_string(value.clone()),
                TokenKind::Integer(value) => self.parse_integer(*value),
                TokenKind::Float(value) => self.parse_float(*value),
                TokenKind::Identifier(name) => self.parse_identifier(name.clone()),
                TokenKind::LeftParen => self.parse_group(),
                _ => Err(format!(
                    "Failed to parse expression beginning with token '{}'",
                    t.kind
                )),
            },

            None => Err(String::from(
                "Failed to load token while parsing primary expression.",
            )),
        }
    }

    fn parse_expression(&mut self) -> Result<Expression, String> {
        self.parse_primary()
    }
}

pub fn parse(input: String) -> Result<ast::Expression, &'static str> {
    let tokens = scanner::scan(input)?;
    let mut context = Parser { current: 0, tokens };

    while !context.is_at_end() {
        println!(
            "{}",
            match context.parse_primary() {
                Ok(node) => format!("{}", node),
                Err(e) => {
                    context.advance_token();
                    e
                }
            }
        );
    }

    Err("...")
}
