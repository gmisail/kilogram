use crate::ast::ast_type::AstType;
use crate::ast::operator::*;
use crate::ast::untyped_node::UntypedNode;

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

    fn previous_token(&self) -> Option<&Token> {
        self.tokens.get(self.current - 1)
    }

    fn advance_token(&mut self) {
        self.current += 1;
    }

    fn match_token(&self, kind: &TokenKind) -> bool {
        let peeked = self.peek_token();

        match peeked {
            Some(t) => mem::discriminant(&t.kind) == mem::discriminant(kind),
            None => false,
        }
    }

    fn expect_token(&mut self, kind: &TokenKind) -> Result<Option<&Token>, String> {
        if self.match_token(kind) {
            self.advance_token();

            Ok(self.previous_token())
        } else {
            let actual_kind = match self.peek_token() {
                Some(t) => &t.kind,
                None => &TokenKind::Eof,
            };

            Err(format!(
                "Expected token with kind '{kind}', got '{actual_kind}' instead."
            ))
        }
    }

    fn parse_boolean(&mut self, value: bool) -> Result<UntypedNode, String> {
        self.advance_token();
        Ok(UntypedNode::Boolean(value))
    }

    fn parse_string(&mut self, value: String) -> Result<UntypedNode, String> {
        self.advance_token();
        Ok(UntypedNode::Str(value))
    }

    fn parse_integer(&mut self, value: i32) -> Result<UntypedNode, String> {
        self.advance_token();
        Ok(UntypedNode::Integer(value))
    }

    fn parse_float(&mut self, value: f32) -> Result<UntypedNode, String> {
        self.advance_token();
        Ok(UntypedNode::Float(value))
    }

    fn parse_identifier(&mut self, name: String) -> Result<UntypedNode, String> {
        // Consume the identifer token.
        self.advance_token();

        if self.match_token(&TokenKind::LeftBrace) {
            self.parse_record_instance(name)
        } else {
            Ok(UntypedNode::Variable(name))
        }
    }

    fn parse_group(&mut self) -> Result<UntypedNode, String> {
        self.advance_token();

        // ( inner_expr )
        let inner_expr = UntypedNode::Group(Box::new(self.parse_expression()?));
        self.expect_token(&TokenKind::RightParen)?;

        Ok(inner_expr)
    }

    fn parse_primary(&mut self) -> Result<UntypedNode, String> {
        match self.peek_token() {
            Some(t) => match &t.kind {
                TokenKind::Boolean(value) => self.parse_boolean(*value),
                TokenKind::Str(value) => self.parse_string(value.clone()),
                TokenKind::Integer(value) => self.parse_integer(*value),
                TokenKind::Float(value) => self.parse_float(*value),
                TokenKind::Identifier(name) => self.parse_identifier(name.clone()),
                TokenKind::LeftParen => self.parse_group(),
                _ => Err(format!(
                    "Failed to parse UntypedNode beginning with token '{}'",
                    t.kind
                )),
            },

            None => Err(String::from(
                "Failed to load token while parsing primary UntypedNode.",
            )),
        }
    }

    fn parse_record_instance(&mut self, record_type: String) -> Result<UntypedNode, String> {
        let mut fields = vec![];

        // Consume the opening brace.
        self.advance_token();

        // If we immediately get a '}', don't bother parsing any fields.
        if !self.match_token(&TokenKind::RightBrace) {
            loop {
                let identifier = self.expect_token(&TokenKind::Identifier("".to_string()))?;
                let name = match identifier {
                    Some(t) => match &t.kind {
                        TokenKind::Identifier(literal) => literal.clone(),
                        _ => return Err(format!("Expected identifier, got {} instead.", t.kind)),
                    },
                    None => {
                        return Err("Reached end of input while parsing UntypedNode.".to_string())
                    }
                };

                self.expect_token(&TokenKind::Colon)?;

                let value = self.parse_expression()?;
                fields.push((name, value));

                if self.match_token(&TokenKind::Comma) {
                    // Consume a comma after a key:value pair, implies there are multiple.
                    self.advance_token();
                } else {
                    // Not a comma? Then we must be done.
                    self.expect_token(&TokenKind::RightBrace)?;

                    break;
                }
            }
        } else {
            // Consume the closing '}'.
            self.advance_token();
        }

        Ok(UntypedNode::RecordInstance(record_type, fields))
    }

    fn finish_function_call(&mut self, expr: UntypedNode) -> Result<UntypedNode, String> {
        let mut arguments = vec![];

        // If the next token is ')', then there are *no* arguments in
        // this function call.
        if !self.match_token(&TokenKind::RightParen) {
            loop {
                arguments.push(self.parse_expression()?);

                if !self.match_token(&TokenKind::Comma) {
                    break;
                }

                // Consume the delimiting comma.
                self.advance_token();
            }
        }

        self.expect_token(&TokenKind::RightParen)?;

        Ok(UntypedNode::FunctionCall(Box::new(expr), arguments))
    }

    fn parse_function_call(&mut self) -> Result<UntypedNode, String> {
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
                let identifier = self.expect_token(&TokenKind::Identifier("".to_string()))?;
                let name = match identifier {
                    Some(t) => match &t.kind {
                        TokenKind::Identifier(literal) => literal,
                        _ => return Err("Expected identifier after '.'.".to_string()),
                    },
                    None => return Err("Expected identifier after '.', got nothing.".to_string()),
                };

                expr = UntypedNode::Get(name.clone(), Box::new(expr));
            } else {
                break;
            }
        }

        Ok(expr)
    }

    fn parse_unary(&mut self) -> Result<UntypedNode, String> {
        // Is there a leading '-' or '!'?
        if self.match_token(&TokenKind::Sub) || self.match_token(&TokenKind::Bang) {
            let operator_kind = match self.peek_token() {
                Some(t) => match &t.kind {
                    TokenKind::Sub => UnaryOperator::Minus,
                    TokenKind::Bang => UnaryOperator::Bang,
                    _ => {
                        return Err(
                            "Cannot parse unary UntypedNode with unknown operator.".to_string()
                        )
                    }
                },
                None => {
                    return Err(
                        "Cannot parse unary UntypedNode, unable to find operator.".to_string()
                    )
                }
            };

            self.advance_token();

            Ok(UntypedNode::Unary(
                Box::new(self.parse_primary()?),
                operator_kind,
            ))
        } else {
            self.parse_function_call()
        }
    }

    fn parse_factor(&mut self) -> Result<UntypedNode, String> {
        let mut expr = self.parse_unary()?;

        while self.match_token(&TokenKind::Mult) || self.match_token(&TokenKind::Div) {
            let operator_kind = match self.peek_token() {
                Some(t) => match &t.kind {
                    TokenKind::Mult => BinaryOperator::Mult,
                    TokenKind::Div => BinaryOperator::Div,
                    _ => {
                        return Err(
                            "Cannot parse binary UntypedNode with unknown operator.".to_string()
                        )
                    }
                },
                None => {
                    return Err(
                        "Cannot parse binary UntypedNode, unable to find operator.".to_string()
                    )
                }
            };

            self.advance_token();

            let right_expr = self.parse_unary()?;
            expr = UntypedNode::Binary(Box::new(expr), operator_kind, Box::new(right_expr));
        }

        Ok(expr)
    }

    fn parse_term(&mut self) -> Result<UntypedNode, String> {
        let mut expr = self.parse_factor()?;

        while self.match_token(&TokenKind::Add) || self.match_token(&TokenKind::Sub) {
            let operator_kind = match self.peek_token() {
                Some(t) => match &t.kind {
                    TokenKind::Add => BinaryOperator::Add,
                    TokenKind::Sub => BinaryOperator::Sub,
                    _ => {
                        return Err(
                            "Cannot parse binary UntypedNode with unknown operator.".to_string()
                        )
                    }
                },
                None => {
                    return Err(
                        "Cannot parse binary UntypedNode, unable to find operator.".to_string()
                    )
                }
            };

            self.advance_token();

            let right_expr = self.parse_factor()?;
            expr = UntypedNode::Binary(Box::new(expr), operator_kind, Box::new(right_expr));
        }

        Ok(expr)
    }

    fn parse_comparison(&mut self) -> Result<UntypedNode, String> {
        let mut expr = self.parse_term()?;

        while self.match_token(&TokenKind::Greater)
            || self.match_token(&TokenKind::GreaterEq)
            || self.match_token(&TokenKind::Less)
            || self.match_token(&TokenKind::LessEq)
        {
            let operator = match self.peek_token() {
                Some(t) => match &t.kind {
                    TokenKind::Greater => BinaryOperator::Greater,
                    TokenKind::GreaterEq => BinaryOperator::GreaterEq,
                    TokenKind::Less => BinaryOperator::Less,
                    TokenKind::LessEq => BinaryOperator::LessEq,
                    _ => return Err("Invalid operator".to_string()),
                },
                None => return Err("Unable to get operator from token.".to_string()),
            };

            self.advance_token();

            let right_expr = self.parse_term()?;
            expr = UntypedNode::Binary(Box::new(expr), operator, Box::new(right_expr));
        }

        Ok(expr)
    }

    fn parse_equality(&mut self) -> Result<UntypedNode, String> {
        let mut expr = self.parse_comparison()?;

        while self.match_token(&TokenKind::Equality) || self.match_token(&TokenKind::NotEqual) {
            let operator_kind = match self.peek_token() {
                Some(t) => match &t.kind {
                    TokenKind::Equality => BinaryOperator::Equality,
                    TokenKind::NotEqual => BinaryOperator::NotEqual,
                    _ => {
                        return Err(
                            "Cannot parse binary UntypedNode with unknown operator.".to_string()
                        )
                    }
                },
                None => {
                    return Err(
                        "Cannot parse binary UntypedNode, unable to find operator.".to_string()
                    )
                }
            };

            self.advance_token();

            let right_expr = self.parse_comparison()?;
            expr = UntypedNode::Binary(Box::new(expr), operator_kind, Box::new(right_expr));
        }

        Ok(expr)
    }

    fn parse_logical_and(&mut self) -> Result<UntypedNode, String> {
        let mut expr = self.parse_equality()?;

        while self.match_token(&TokenKind::And) {
            self.advance_token();

            let right_expr = self.parse_equality()?;
            expr = UntypedNode::Logical(Box::new(expr), LogicalOperator::And, Box::new(right_expr));
        }

        Ok(expr)
    }

    fn parse_logical_or(&mut self) -> Result<UntypedNode, String> {
        let mut expr = self.parse_logical_and()?;

        while self.match_token(&TokenKind::Or) {
            self.advance_token();

            let right_expr = self.parse_primary()?;
            expr = UntypedNode::Logical(Box::new(expr), LogicalOperator::Or, Box::new(right_expr));
        }

        Ok(expr)
    }

    fn parse_function_type(&mut self) -> Result<AstType, String> {
        self.advance_token();

        let mut arguments = vec![];

        if !self.match_token(&TokenKind::RightParen) {
            loop {
                arguments.push(Box::new(self.parse_type()?));

                if self.match_token(&TokenKind::Comma) {
                    // Consume a comma after a AstType, implies there are multiple.
                    self.advance_token();
                } else {
                    // Not a comma? Then we must be done.
                    self.expect_token(&TokenKind::RightParen)?;

                    break;
                }
            }
        } else {
            // Consume the closing ')'.
            self.advance_token();
        }

        self.expect_token(&TokenKind::ThinArrow)?;

        let return_type = self.parse_type()?;

        Ok(AstType::Function(arguments, Box::new(return_type)))
    }

    fn parse_type(&mut self) -> Result<AstType, String> {
        if self.match_token(&TokenKind::LeftParen) {
            return self.parse_function_type();
        }

        let base_type = match self.peek_token() {
            Some(t) => match &t.kind {
                TokenKind::Identifier(name) => name.clone(),
                _ => return Err(format!("Expected an AstType name, got {} instead.", t.kind)),
            },
            None => {
                return Err(
                    "Expected AstType name, instead we reached the end of the file.".to_string(),
                )
            }
        };

        self.advance_token();

        if self.match_token(&TokenKind::LeftParen) {
            self.advance_token();

            let sub_type = self.parse_type()?;

            self.expect_token(&TokenKind::RightParen)?;

            Ok(AstType::Generic(base_type, Box::new(sub_type)))
        } else {
            Ok(AstType::Base(base_type))
        }
    }

    fn parse_function_expression(&mut self) -> Result<UntypedNode, String> {
        if self.match_token(&TokenKind::Function) {
            // Consume 'function' keyword.
            self.advance_token();

            self.expect_token(&TokenKind::LeftParen)?;

            let mut arguments = vec![];

            // If we immediately get a ')', don't bother parsing any arguments.
            if !self.match_token(&TokenKind::RightParen) {
                loop {
                    let identifier = self.expect_token(&TokenKind::Identifier("".to_string()))?;
                    let name = match identifier {
                        Some(t) => match &t.kind {
                            TokenKind::Identifier(literal) => literal.clone(),
                            _ => {
                                return Err(format!("Expected identifier, got {} instead.", t.kind))
                            }
                        },
                        None => {
                            return Err(
                                "Reached end of input while parsing UntypedNode.".to_string()
                            )
                        }
                    };

                    self.expect_token(&TokenKind::Colon)?;

                    let argument_type = self.parse_type()?;
                    arguments.push((name, argument_type));

                    if self.match_token(&TokenKind::Comma) {
                        // Consume a comma after a argument:AstType pair, implies there are multiple.
                        self.advance_token();
                    } else {
                        // Not a comma? Then we must be done.
                        self.expect_token(&TokenKind::RightParen)?;

                        break;
                    }
                }
            } else {
                // Consume the closing '}'.
                self.advance_token();
            }

            self.expect_token(&TokenKind::Colon)?;

            let function_type = self.parse_type()?;
            let function_body = self.parse_expression()?;

            self.expect_token(&TokenKind::End)?;

            Ok(UntypedNode::Function(
                "anonymous".to_string(),
                function_type,
                arguments,
                Box::new(function_body),
            ))
        } else {
            self.parse_logical_or()
        }
    }

    fn parse_record_declaration(&mut self) -> Result<UntypedNode, String> {
        if self.match_token(&TokenKind::Record) {
            self.advance_token();

            let mut fields = vec![];

            let name_token = self.expect_token(&TokenKind::Identifier("".to_string()))?;
            let record_name = match name_token {
                Some(t) => match &t.kind {
                    TokenKind::Identifier(name) => name.clone(),
                    _ => return Err("Expected an identifier as record name.".to_string()),
                },
                None => {
                    return Err(
                        "Expected record name, instead we reached the end of the file.".to_string(),
                    )
                }
            };

            // If we immediately get a 'end' don't bother parsing any fields.
            if !self.match_token(&TokenKind::End) {
                loop {
                    let identifier = self.expect_token(&TokenKind::Identifier("".to_string()))?;
                    let field_name = match identifier {
                        Some(t) => match &t.kind {
                            TokenKind::Identifier(literal) => literal.clone(),
                            _ => {
                                return Err(format!("Expected identifier, got {} instead.", t.kind))
                            }
                        },
                        None => {
                            return Err(
                                "Reached end of input while parsing UntypedNode.".to_string()
                            )
                        }
                    };

                    self.expect_token(&TokenKind::Colon)?;

                    let field_type = self.parse_type()?;
                    fields.push((field_name, field_type));

                    if self.match_token(&TokenKind::Comma) {
                        // Consume a comma after a field:AstType pair, implies there are multiple.
                        self.advance_token();
                    } else {
                        // Not a comma? Then we must be done.
                        self.expect_token(&TokenKind::End)?;

                        break;
                    }
                }
            } else {
                // Consume the closing 'end'.
                self.advance_token();
            }

            let body = self.parse_expression()?;

            Ok(UntypedNode::RecordDeclaration(
                record_name,
                fields,
                Box::new(body),
            ))
        } else {
            self.parse_function_expression()
        }
    }

    fn parse_if_expression(&mut self) -> Result<UntypedNode, String> {
        if self.match_token(&TokenKind::If) {
            // Consume 'if' token.
            self.advance_token();

            let if_condition = self.parse_expression()?;

            self.expect_token(&TokenKind::Then)?;

            let then_expression = self.parse_expression()?;

            self.expect_token(&TokenKind::Else)?;

            let else_expression = self.parse_expression()?;

            Ok(UntypedNode::If(
                Box::new(if_condition),
                Box::new(then_expression),
                Box::new(else_expression),
            ))
        } else {
            self.parse_record_declaration()
        }
    }

    fn parse_extern_expression(&mut self) -> Result<UntypedNode, String> {
        if self.match_token(&TokenKind::Extern) {
            self.advance_token();

            let name_token = self.expect_token(&TokenKind::Identifier("".to_string()))?;
            let extern_name = match name_token {
                Some(t) => match &t.kind {
                    TokenKind::Identifier(name) => name.clone(),
                    _ => return Err("Expected an identifier as extern name.".to_string()),
                },
                None => {
                    return Err(
                        "Expected variable name, instead we reached the end of the file."
                            .to_string(),
                    )
                }
            };

            self.expect_token(&TokenKind::Colon)?;

            let extern_type = self.parse_type()?;
            let extern_body = self.parse_expression()?;

            Ok(UntypedNode::Extern(
                extern_name,
                extern_type,
                Box::new(extern_body),
            ))
        } else {
            self.parse_if_expression()
        }
    }

    fn parse_let_expression(&mut self) -> Result<UntypedNode, String> {
        if self.match_token(&TokenKind::Let) {
            self.advance_token();

            let mut is_recursive = false;

            // let <rec?> <name> : <AstType> = <value>

            if self.match_token(&TokenKind::Recursive) {
                is_recursive = true;
                self.advance_token();
            }

            let name_token = self.expect_token(&TokenKind::Identifier("".to_string()))?;
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

            self.expect_token(&TokenKind::Colon)?;

            let variable_type = self.parse_type()?;

            self.expect_token(&TokenKind::Equal)?;

            let variable_value = self.parse_expression()?;
            let variable_body = self.parse_expression()?;

            let is_func = matches!(variable_value, UntypedNode::Function(..));

            if is_recursive && !is_func {
                return Err(format!("Expected value of variable '{variable_name}' to be a function since it was marked as recursive."));
            }

            Ok(UntypedNode::Let(
                variable_name,
                variable_type,
                Box::new(variable_value),
                Box::new(variable_body),
                is_recursive,
            ))
        } else {
            self.parse_extern_expression()
        }
    }

    fn parse_expression(&mut self) -> Result<UntypedNode, String> {
        self.parse_let_expression()
    }
}

pub fn parse(input: String) -> Result<UntypedNode, &'static str> {
    let tokens = scanner::scan(input)?;
    let mut context = Parser { current: 0, tokens };

    let mut expressions = vec![];

    while !context.is_at_end() {
        match context.parse_expression() {
            Ok(node) => {
                expressions.push(node);
            }
            Err(e) => {
                context.advance_token();
                println!("{e}");
            }
        }
    }

    Ok(expressions.get(0).unwrap().clone())
}
