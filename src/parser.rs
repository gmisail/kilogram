use crate::ast::Expression;
use crate::ast::Type;

use super::ast;
use super::scanner;
use super::token::{Token, TokenKind};

use std::mem;

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

    fn next_token(&self) -> Option<&Token> {
        self.tokens.get(self.current + 1)
    }

    fn previous_token(&self) -> Option<&Token> {
        self.tokens.get(self.current - 1)
    }

    fn advance_token(&mut self) {
        self.current += 1;
    }

    fn match_token(&mut self, kind: &TokenKind) -> bool {
        let peeked = self.peek_token();

        match peeked {
            Some(t) => mem::discriminant(&t.kind) == mem::discriminant(kind),
            None => false,
        }
    }

    fn match_next_token(&mut self, kind: &TokenKind) -> bool {
        match self.next_token() {
            Some(t) => mem::discriminant(&t.kind) == mem::discriminant(kind),
            None => false,
        }
    }

    fn expect_token(&mut self, kind: TokenKind) -> Result<Option<&Token>, String> {
        if self.match_token(&kind) {
            self.advance_token();

            Ok(self.previous_token())
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
        // Consume the identifer token.
        self.advance_token();

        if self.match_token(&TokenKind::LeftBrace) {
            self.parse_record_instance(name)
        } else {
            Ok(Expression::Variable(name))
        }
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

    fn parse_record_instance(&mut self, record_type: String) -> Result<Expression, String> {
        let mut fields = vec![];

        // Consume the opening brace.
        self.advance_token();

        // If we immediately get a '}', don't bother parsing any fields.
        if !self.match_token(&TokenKind::RightBrace) {
            loop {
                let identifier = self.expect_token(TokenKind::Identifier("".to_string()))?;
                let name = match identifier {
                    Some(t) => match &t.kind {
                        TokenKind::Identifier(literal) => literal.clone(),
                        _ => return Err(format!("Expected identifier, got {} instead.", t.kind)),
                    },
                    None => {
                        return Err("Reached end of input while parsing expression.".to_string())
                    }
                };

                self.expect_token(TokenKind::Colon)?;

                let value = self.parse_expression()?;
                fields.push((name, Box::new(value)));

                if self.match_token(&TokenKind::Comma) {
                    // Consume a comma after a key:value pair, implies there are multiple.
                    self.advance_token();
                } else {
                    // Not a comma? Then we must be done.
                    self.expect_token(TokenKind::RightBrace)?;

                    break;
                }
            }
        } else {
            // Consume the closing '}'.
            self.advance_token();
        }

        Ok(ast::Expression::RecordInstance(record_type, fields))
    }

    fn finish_function_call(&mut self, expr: Expression) -> Result<Expression, String> {
        let mut arguments = vec![];

        // If the next token is ')', then there are *no* arguments in
        // this function call.
        if !self.match_token(&TokenKind::RightParen) {
            loop {
                arguments.push(Box::new(self.parse_expression()?));

                if !self.match_token(&TokenKind::Comma) {
                    break;
                }

                // Consume the delimiting comma.
                self.advance_token();
            }
        }

        self.expect_token(TokenKind::RightParen)?;

        Ok(ast::Expression::FunctionCall(Box::new(expr), arguments))
    }

    fn parse_function_call(&mut self) -> Result<Expression, String> {
        let mut expr = self.parse_primary()?;

        loop {
            if self.match_token(&TokenKind::LeftParen) {
                self.advance_token();

                expr = self.finish_function_call(expr)?;
            } else if self.match_token(&TokenKind::Period) {
                self.advance_token();
                // expect_token only compares token *kind*, thus
                // the value here doesn't matter-- we just use an
                // empty string.
                let identifier = self.expect_token(TokenKind::Identifier("".to_string()))?;
                let name = match identifier {
                    Some(t) => match &t.kind {
                        TokenKind::Identifier(literal) => literal,
                        _ => return Err("Expected identifier after '.'.".to_string()),
                    },
                    None => return Err("Expected identifier after '.', got nothing.".to_string()),
                };

                expr = Expression::Get(name.clone(), Box::new(expr));
            } else {
                break;
            }
        }

        Ok(expr)
    }

    fn parse_unary(&mut self) -> Result<Expression, String> {
        // Is there a leading '-' or '!'?
        if self.match_token(&TokenKind::Sub) || self.match_token(&TokenKind::Bang) {
            let operator_kind = match self.peek_token() {
                Some(t) => match &t.kind {
                    TokenKind::Sub => ast::UnaryOperator::Minus,
                    TokenKind::Bang => ast::UnaryOperator::Bang,
                    _ => {
                        return Err(
                            "Cannot parse unary expression with unknown operator.".to_string()
                        )
                    }
                },
                None => {
                    return Err(
                        "Cannot parse unary expression, unable to find operator.".to_string()
                    )
                }
            };

            self.advance_token();

            Ok(ast::Expression::Unary(
                Box::new(self.parse_primary()?),
                operator_kind,
            ))
        } else {
            self.parse_function_call()
        }
    }

    fn parse_factor(&mut self) -> Result<Expression, String> {
        let mut expr = self.parse_unary()?;

        while self.match_token(&TokenKind::Mult) || self.match_token(&TokenKind::Div) {
            let operator_kind = match self.peek_token() {
                Some(t) => match &t.kind {
                    TokenKind::Mult => ast::BinaryOperator::Mult,
                    TokenKind::Div => ast::BinaryOperator::Div,
                    _ => {
                        return Err(
                            "Cannot parse binary expression with unknown operator.".to_string()
                        )
                    }
                },
                None => {
                    return Err(
                        "Cannot parse binary expression, unable to find operator.".to_string()
                    )
                }
            };

            self.advance_token();

            let right_expr = self.parse_unary()?;
            expr = ast::Expression::Binary(Box::new(expr), operator_kind, Box::new(right_expr));
        }

        Ok(expr)
    }

    fn parse_term(&mut self) -> Result<Expression, String> {
        let mut expr = self.parse_factor()?;

        while self.match_token(&TokenKind::Add) || self.match_token(&TokenKind::Sub) {
            let operator_kind = match self.peek_token() {
                Some(t) => match &t.kind {
                    TokenKind::Add => ast::BinaryOperator::Add,
                    TokenKind::Sub => ast::BinaryOperator::Sub,
                    _ => {
                        return Err(
                            "Cannot parse binary expression with unknown operator.".to_string()
                        )
                    }
                },
                None => {
                    return Err(
                        "Cannot parse binary expression, unable to find operator.".to_string()
                    )
                }
            };

            self.advance_token();

            let right_expr = self.parse_factor()?;
            expr = ast::Expression::Binary(Box::new(expr), operator_kind, Box::new(right_expr));
        }

        Ok(expr)
    }

    fn parse_comparison(&mut self) -> Result<Expression, String> {
        let mut expr = self.parse_term()?;

        while self.match_token(&TokenKind::Greater)
            || self.match_token(&TokenKind::GreaterEq)
            || self.match_token(&TokenKind::Less)
            || self.match_token(&TokenKind::LessEq)
        {
            let operator = match self.peek_token() {
                Some(t) => match &t.kind {
                    TokenKind::Greater => ast::BinaryOperator::Greater,
                    TokenKind::GreaterEq => ast::BinaryOperator::GreaterEq,
                    TokenKind::Less => ast::BinaryOperator::Less,
                    TokenKind::LessEq => ast::BinaryOperator::LessEq,
                    _ => return Err("Invalid operator".to_string()),
                },
                None => return Err("Unable to get operator from token.".to_string()),
            };

            self.advance_token();

            let right_expr = self.parse_term()?;
            expr = ast::Expression::Binary(Box::new(expr), operator, Box::new(right_expr));
        }

        Ok(expr)
    }

    fn parse_equality(&mut self) -> Result<Expression, String> {
        let mut expr = self.parse_comparison()?;

        while self.match_token(&TokenKind::Equality) || self.match_token(&TokenKind::NotEqual) {
            let operator_kind = match self.peek_token() {
                Some(t) => match &t.kind {
                    TokenKind::Equality => ast::BinaryOperator::Equality,
                    TokenKind::NotEqual => ast::BinaryOperator::NotEqual,
                    _ => {
                        return Err(
                            "Cannot parse binary expression with unknown operator.".to_string()
                        )
                    }
                },
                None => {
                    return Err(
                        "Cannot parse binary expression, unable to find operator.".to_string()
                    )
                }
            };

            self.advance_token();

            let right_expr = self.parse_comparison()?;
            expr = ast::Expression::Binary(Box::new(expr), operator_kind, Box::new(right_expr));
        }

        Ok(expr)
    }

    fn parse_logical_and(&mut self) -> Result<Expression, String> {
        let mut expr = self.parse_equality()?;

        while self.match_token(&TokenKind::And) {
            self.advance_token();

            let right_expr = self.parse_equality()?;
            expr = ast::Expression::Logical(
                Box::new(expr),
                ast::LogicalOperator::And,
                Box::new(right_expr),
            );
        }

        Ok(expr)
    }

    fn parse_logical_or(&mut self) -> Result<Expression, String> {
        let mut expr = self.parse_logical_and()?;

        while self.match_token(&TokenKind::Or) {
            self.advance_token();

            let right_expr = self.parse_primary()?;
            expr = ast::Expression::Logical(
                Box::new(expr),
                ast::LogicalOperator::Or,
                Box::new(right_expr),
            );
        }

        Ok(expr)
    }

    fn parse_type(&mut self) -> Result<Type, String> {
        let base_type = match self.peek_token() {
            Some(t) => match &t.kind {
                TokenKind::Identifier(name) => name.clone(),
                _ => return Err(format!("Expected an type name, got {} instead.", t.kind)),
            },
            None => {
                return Err(
                    "Expected type name, instead we reached the end of the file.".to_string(),
                )
            }
        };

        self.advance_token();

        if self.match_token(&TokenKind::LeftParen) {
            self.advance_token();

            let sub_type = self.parse_type()?;

            self.expect_token(TokenKind::RightParen)?;

            Ok(ast::Type::Generic(base_type, Box::new(sub_type)))
        } else {
            Ok(ast::Type::Base(base_type))
        }
    }

    fn parse_let_expression(&mut self) -> Result<Expression, String> {
        if self.match_token(&TokenKind::Let) {
            self.advance_token();

            // let <name> : <type> = <value>

            let name_token = self.expect_token(TokenKind::Identifier("".to_string()))?;
            let variable_name = match name_token {
                Some(t) => match &t.kind {
                    TokenKind::Identifier(name) => name.clone(),
                    _ => return Err("Expected an identifier as variable name.".to_string()),
                },
                None => {
                    return Err(
                        "Expected variable name, instead we reached the end of the file."
                            .to_string(),
                    )
                }
            };

            self.expect_token(TokenKind::Colon)?;

            let variable_type = self.parse_type()?;

            self.expect_token(TokenKind::Equal)?;

            let variable_value = self.parse_expression()?;

            let variable_body = self.parse_expression()?;

            Ok(ast::Expression::Let(
                variable_name,
                variable_type,
                Box::new(variable_value),
                Box::new(variable_body),
            ))
        } else {
            self.parse_logical_or()
        }
    }

    fn parse_expression(&mut self) -> Result<Expression, String> {
        self.parse_let_expression()
    }
}

pub fn parse(input: String) -> Result<ast::Expression, &'static str> {
    let tokens = scanner::scan(input)?;
    let mut context = Parser { current: 0, tokens };

    while !context.is_at_end() {
        println!(
            "{}",
            match context.parse_expression() {
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
