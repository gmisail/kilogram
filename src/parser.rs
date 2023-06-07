use crate::ast::operator::*;
use crate::ast::untyped::ast_type::AstType;
use crate::ast::untyped::untyped_node::UntypedNode;

use super::scanner;
use super::token::{Token, TokenKind};

use crate::ast::untyped::untyped_node::UntypedNode::FunctionInstance;
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

    fn parse_name(&mut self) -> Result<String, String> {
        let identifier = self.expect_token(&TokenKind::Identifier("".to_string()))?;
        match identifier {
            Some(t) => match &t.kind {
                TokenKind::Identifier(literal) => Ok(literal.clone()),
                _ => Err(format!("Expected identifier, got {} instead.", t.kind)),
            },
            None => Err("Reached end of input while parsing UntypedNode.".to_string()),
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
        // Consume the identifier token.
        self.advance_token();

        if self.match_token(&TokenKind::LeftBracket) || self.match_token(&TokenKind::LeftBrace) {
            self.parse_sub_type_instance(name)
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
                TokenKind::LeftBrace => self.parse_anonymous_record(),
                TokenKind::LeftBracket => self.parse_list(),
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

    fn parse_list(&mut self) -> Result<UntypedNode, String> {
        self.advance_token();

        let mut elements = Vec::new();

        if !self.match_token(&TokenKind::RightBracket) {
            loop {
                elements.push(self.parse_expression()?);

                if !self.match_token(&TokenKind::Comma) {
                    break;
                }

                // TODO: what happens if the list is empty?

                // Consume the delimiting comma.
                self.advance_token();
            }
        }

        self.expect_token(&TokenKind::RightBracket)?;

        Ok(UntypedNode::List(elements))
    }

    fn parse_anonymous_record(&mut self) -> Result<UntypedNode, String> {
        let mut fields = Vec::new();

        self.advance_token();

        // If we immediately get a '}', don't bother parsing any fields.
        if !self.match_token(&TokenKind::RightBrace) {
            loop {
                let name = self.parse_name()?;
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

        Ok(UntypedNode::AnonymousRecord(fields))
    }

    // Parses either a sub-typed function instance or sub-typed record instance.
    fn parse_sub_type_instance(&mut self, instance_name: String) -> Result<UntypedNode, String> {
        let mut type_params = Vec::new();
        if self.match_token(&TokenKind::LeftBracket) {
            self.advance_token();

            loop {
                type_params.push(self.parse_type()?);

                if self.match_token(&TokenKind::Comma) {
                    // Consume a comma after a type, implies there are multiple.
                    self.advance_token();
                } else {
                    // Not a comma? Then we must be done.
                    self.expect_token(&TokenKind::RightBracket)?;

                    break;
                }
            }
        }

        if !self.match_token(&TokenKind::LeftBrace) {
            Ok(FunctionInstance(
                Box::new(UntypedNode::Variable(instance_name)),
                type_params,
            ))
        } else {
            self.advance_token();

            let mut fields = Vec::new();

            // If we immediately get a '}', don't bother parsing any fields.
            if !self.match_token(&TokenKind::RightBrace) {
                loop {
                    let name = self.parse_name()?;

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

            Ok(UntypedNode::RecordInstance(
                instance_name,
                type_params,
                fields,
            ))
        }
    }

    fn finish_function_call(&mut self, expr: UntypedNode) -> Result<UntypedNode, String> {
        let mut sub_types = Vec::new();

        if self.match_token(&TokenKind::LeftBracket) {
            self.advance_token();

            loop {
                let sub_type = self.parse_type()?;

                sub_types.push(sub_type);

                if self.match_token(&TokenKind::RightBracket) {
                    self.advance_token();

                    break;
                } else {
                    self.expect_token(&TokenKind::Comma)?;
                }
            }
        }

        self.expect_token(&TokenKind::LeftParen)?;

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
                expr = self.finish_function_call(expr)?;
            } else if self.match_token(&TokenKind::Period) {
                self.advance_token();

                let name = self.parse_name()?;

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

    fn parse_record_type(&mut self) -> Result<AstType, String> {
        self.advance_token();

        let mut fields = Vec::new();

        // If we immediately get a '}', don't bother parsing any fields.
        if !self.match_token(&TokenKind::RightBrace) {
            loop {
                let name = self.parse_name()?;

                self.expect_token(&TokenKind::Colon)?;

                let field_type = self.parse_type()?;
                fields.push((name, field_type));

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

        Ok(AstType::Record(fields))
    }

    fn parse_type(&mut self) -> Result<AstType, String> {
        if self.match_token(&TokenKind::LeftParen) {
            return self.parse_function_type();
        } else if self.match_token(&TokenKind::LeftBracket) {
            return self.parse_record_type();
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

        if self.match_token(&TokenKind::LeftBracket) {
            self.advance_token();

            let mut sub_types = Vec::new();

            loop {
                let sub_type = self.parse_type()?;

                sub_types.push(sub_type);

                if self.match_token(&TokenKind::RightBracket) {
                    self.advance_token();

                    break;
                } else {
                    self.expect_token(&TokenKind::Comma)?;
                }
            }

            Ok(AstType::Generic(base_type, sub_types))
        } else {
            Ok(AstType::Base(base_type))
        }
    }

    fn parse_type_params(&mut self) -> Result<Vec<String>, String> {
        let mut type_params = Vec::new();

        if self.match_token(&TokenKind::LeftBracket) {
            self.advance_token();

            loop {
                let type_param = self.parse_name()?;
                type_params.push(type_param);

                if self.match_token(&TokenKind::Comma) {
                    // Consume a comma after a type, implies there are multiple.
                    self.advance_token();
                } else {
                    // Not a comma? Then we must be done.
                    self.expect_token(&TokenKind::RightBracket)?;

                    break;
                }
            }
        }

        Ok(type_params)
    }

    fn parse_function_expression(&mut self) -> Result<UntypedNode, String> {
        if self.match_token(&TokenKind::Function) {
            // Consume 'function' keyword.
            self.advance_token();

            let name = match self.parse_name() {
                Ok(name) => Some(name),
                Err(_) => None,
            };

            let type_params = self.parse_type_params()?;

            if !type_params.is_empty() && name.is_none() {
                return Err("Closures cannot have type parameters.".to_string());
            }

            self.expect_token(&TokenKind::LeftParen)?;

            let mut arguments = vec![];

            // If we immediately get a ')', don't bother parsing any arguments.
            if !self.match_token(&TokenKind::RightParen) {
                loop {
                    let name = self.parse_name()?;

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

            // Named functions are stored separately from closures.
            if let Some(func_name) = name {
                let body = self.parse_expression()?;

                Ok(UntypedNode::FunctionDeclaration(
                    func_name,
                    type_params,
                    function_type,
                    arguments,
                    Box::new(function_body),
                    Box::new(body),
                ))
            } else {
                Ok(UntypedNode::Function(
                    function_type,
                    arguments,
                    Box::new(function_body),
                ))
            }
        } else {
            self.parse_logical_or()
        }
    }

    fn parse_enum(&mut self) -> Result<UntypedNode, String> {
        if self.match_token(&TokenKind::Enum) {
            self.advance_token();

            let name_token = self.expect_token(&TokenKind::Identifier("".to_string()))?;
            let enum_name = match name_token {
                Some(t) => match &t.kind {
                    TokenKind::Identifier(name) => name.clone(),
                    _ => return Err("Expected an identifier as enum name.".to_string()),
                },
                None => {
                    return Err(
                        "Expected enum name, instead we reached the end of the file.".to_string(),
                    )
                }
            };

            let mut type_params = Vec::new();

            if self.match_token(&TokenKind::LeftParen) {
                self.advance_token();

                loop {
                    let identifier = self.expect_token(&TokenKind::Identifier("".to_string()))?;
                    let type_param = match identifier {
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

                    type_params.push(type_param);

                    if self.match_token(&TokenKind::RightParen) {
                        self.advance_token();

                        break;
                    } else {
                        self.expect_token(&TokenKind::Comma)?;
                    }
                }
            }

            let mut enum_types = Vec::new();

            if !self.match_token(&TokenKind::End) {
                loop {
                    let identifier = self.expect_token(&TokenKind::Identifier("".to_string()))?;
                    let option_name = match identifier {
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

                    let mut option_types = Vec::new();
                    if self.match_token(&TokenKind::LeftParen) {
                        self.advance_token();

                        loop {
                            option_types.push(self.parse_type()?);

                            if !self.match_token(&TokenKind::Comma)
                                && self.match_token(&TokenKind::RightParen)
                            {
                                self.advance_token();

                                break;
                            }

                            self.expect_token(&TokenKind::Comma)?;
                        }
                    }

                    enum_types.push((option_name, option_types));

                    if self.match_token(&TokenKind::Comma) {
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

            if enum_types.is_empty() {
                return Err(format!("Enum {enum_name} defined with no options."));
            }

            let body = self.parse_expression()?;

            Ok(UntypedNode::EnumDeclaration(
                enum_name,
                enum_types,
                type_params,
                Box::new(body),
            ))
        } else {
            self.parse_function_expression()
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

            let type_params = self.parse_type_params()?;

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
                type_params,
                Box::new(body),
            ))
        } else {
            self.parse_enum()
        }
    }

    fn parse_case_of_expression(&mut self) -> Result<UntypedNode, String> {
        if self.match_token(&TokenKind::Case) {
            /*
               case <expr> of
                   [<condition> -> <body>]+
               end
            */
            self.advance_token();

            let expr = self.parse_expression()?;
            self.expect_token(&TokenKind::Of)?;

            let mut arms = Vec::new();

            if !self.match_token(&TokenKind::End) {
                loop {
                    let condition = self.parse_expression()?;

                    self.expect_token(&TokenKind::ThinArrow)?;

                    let body = self.parse_expression()?;

                    arms.push((condition, body));

                    if !self.match_token(&TokenKind::Comma) && self.match_token(&TokenKind::End) {
                        self.advance_token();

                        break;
                    } else {
                        self.expect_token(&TokenKind::Comma)?;
                    }
                }

                Ok(UntypedNode::CaseOf(Box::new(expr), arms))
            } else {
                Err("No conditions in 'case' statement.".to_string())
            }
        } else {
            self.parse_record_declaration()
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
            self.parse_case_of_expression()
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

            // let <rec?> <name> (: <AstType>)? = <value>

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

            let variable_type = if self.match_token(&TokenKind::Colon) {
                self.advance_token();

                Some(self.parse_type()?)
            } else {
                None
            };

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
