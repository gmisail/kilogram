use std::collections::HashMap;

use super::token::*;

struct ScannerContext {
    start: usize,
    current: usize,
    line: usize,
    index: usize,
    input: Vec<u8>,
    tokens: Vec<Token>,
    keywords: HashMap<&'static str, TokenKind>,
}

trait Scanner {
    fn is_at_end(&mut self) -> bool;

    fn add_token(&mut self, token: Token);

    fn new_symbol_token(&self, kind: TokenKind) -> Token;

    fn match_char(&mut self, c: char) -> bool;
    fn current_char(&self) -> Option<char>;
    fn next_char(&mut self) -> Option<char>;
    fn peek_char(&mut self) -> Option<char>;

    fn scan_integer(&mut self) -> Result<Token, &'static str>;
    fn scan_number(&mut self) -> Result<Token, &'static str>;
    fn scan_string(&mut self) -> Result<Token, &'static str>;
    fn scan_identifier(&mut self) -> Result<Token, &'static str>;
    fn scan_conditional(
        &mut self,
        options: &[(char, TokenKind)],
        fallback: TokenKind,
    ) -> Result<Token, &'static str>;
    fn scan_whitespace(&mut self);

    fn scan_token(&mut self) -> Result<Token, &'static str>;
}

fn to_char(input: Option<&u8>) -> Option<char> {
    input.map(|c| char::from(*c))
}

fn is_digit(c: char) -> bool {
    c.is_ascii_digit()
}

impl Scanner for ScannerContext {
    fn is_at_end(&mut self) -> bool {
        self.current >= self.input.len()
    }

    fn current_char(&self) -> Option<char> {
        to_char(self.input.get(self.current))
    }

    fn next_char(&mut self) -> Option<char> {
        let c = self.current_char();
        self.current += 1;
        self.index += 1;

        if c.map_or(false, |c| c == '\n') {
            self.index = 0;
        }

        c
    }

    fn peek_char(&mut self) -> Option<char> {
        to_char(self.input.get(self.current))
    }

    fn match_char(&mut self, c: char) -> bool {
        if self.is_at_end() {
            return false;
        }

        if char::from(self.input[self.current]) != c {
            false
        } else {
            self.current += 1;
            self.index += 1;

            true
        }
    }

    fn add_token(&mut self, token: Token) {
        self.tokens.push(token);
    }

    fn new_symbol_token(&self, kind: TokenKind) -> Token {
        let length = 1;

        Token {
            kind,
            line: self.line,
            relative: self.index - length,
            absolute: self.current - length,
            length,
        }
    }

    fn scan_conditional(
        &mut self,
        options: &[(char, TokenKind)],
        fallback: TokenKind,
    ) -> Result<Token, &'static str> {
        for (c, kind) in options.iter() {
            if !self.is_at_end() && *c == self.peek_char().unwrap_or('\0') {
                self.next_char();

                return Ok(self.new_symbol_token(kind.clone()));
            }
        }

        // TODO: actually handle lengths for multi-character symbols
        Ok(self.new_symbol_token(fallback))
    }

    fn scan_integer(&mut self) -> Result<Token, &'static str> {
        while !self.is_at_end() && self.peek_char().map_or(false, is_digit) {
            self.next_char();
        }

        if self.start == self.current {
            return Err("Could not find digits while scanning integer.");
        }

        let literal = String::from_utf8(self.input[self.start..self.current].to_vec()).unwrap();
        let length = literal.len();

        Ok(Token {
            kind: TokenKind::Integer(literal.parse().unwrap()),
            line: self.line,
            relative: self.index - length + 1,
            absolute: self.current - length + 1,
            length,
        })
    }

    fn scan_number(&mut self) -> Result<Token, &'static str> {
        let integer_comp = self.scan_integer()?;

        if self.match_char('.') {
            // Start parsing from *after* the decimal token
            self.start = self.current;

            let fractional_comp = self.scan_integer();

            // Check if the integer after '.' is actually an integer.
            let fraction = match fractional_comp {
                Ok(tok) => match tok.kind {
                    TokenKind::Integer(num) => num,
                    _ => 0,
                },

                Err(_) => return Err("Expected digits after '.' in float."),
            };

            let integer = match integer_comp.kind {
                TokenKind::Integer(num) => num,
                _ => 0,
            };

            let full_str = format!("{integer}.{fraction}");
            let full_val = full_str.parse().unwrap();

            Ok(Token {
                kind: TokenKind::Float(full_val),
                line: self.line,
                relative: self.index,
                absolute: self.current,
                length: full_str.len(),
            })
        } else {
            Ok(integer_comp)
        }
    }

    fn scan_string(&mut self) -> Result<Token, &'static str> {
        while !self.is_at_end() && self.peek_char() != Some('"') {
            if self.peek_char() == Some('\n') {
                self.line += 1
            }

            self.next_char();
        }

        // Missing '"' at the end of input.
        if self.is_at_end() {
            return Err("Unclosed string literal.");
        }

        // Skip the trailing character
        self.next_char();

        let content =
            String::from_utf8(self.input[self.start + 1..self.current - 1].to_vec()).unwrap();
        let length = content.len();

        Ok(Token {
            kind: TokenKind::Str(content),
            line: self.line,
            relative: self.index - length + 1,
            absolute: self.current - length + 1,
            length,
        })
    }

    fn scan_identifier(&mut self) -> Result<Token, &'static str> {
        while !self.is_at_end()
            && self
                .peek_char()
                .map_or(false, |c| c.is_alphanumeric() || c == '_')
        {
            self.next_char();
        }

        let literal = String::from_utf8(self.input[self.start..self.current].to_vec()).unwrap();
        let length = literal.len();

        if literal.starts_with("_kg_") {
            Err("Keywords beginning with '_kg_' are reserved for the compiler.")
        } else {
            match self.keywords.get(literal.as_str()) {
                Some(kind) => Ok(Token {
                    kind: kind.clone(),
                    line: self.line,
                    relative: self.index - length,
                    absolute: self.current - length,
                    length,
                }),

                None => Ok(Token {
                    kind: TokenKind::Identifier(literal),
                    line: self.line,
                    relative: self.index - length,
                    absolute: self.current - length, // TODO: use actual absolute position
                    length,
                }),
            }
        }
    }

    fn scan_whitespace(&mut self) {
        while !self.is_at_end() && self.peek_char().map_or(false, char::is_whitespace) {
            if self.peek_char().map_or(false, |c| c == '\n') {
                self.line += 1;
            }

            self.next_char();
        }
    }

    fn scan_token(&mut self) -> Result<Token, &'static str> {
        self.scan_whitespace();
        self.start = self.current;

        if self.is_at_end() {
            Ok(Token {
                kind: TokenKind::Eof,
                line: self.line,
                relative: self.index,
                absolute: self.current,
                length: 0,
            })
        } else {
            match self.next_char() {
                Some(c) => match c {
                    // check for keywords, identifier, or number
                    ch if ch.is_numeric() => self.scan_number(),
                    ch if ch.is_alphabetic() || ch == '_' || ch == '\'' => self.scan_identifier(),
                    '"' => self.scan_string(),

                    // check single-character tokens
                    '+' => Ok(self.new_symbol_token(TokenKind::Add)),
                    '-' => self.scan_conditional(&[('>', TokenKind::ThinArrow)], TokenKind::Sub),
                    '*' => Ok(self.new_symbol_token(TokenKind::Mult)),
                    '/' => Ok(self.new_symbol_token(TokenKind::Div)),
                    '(' => Ok(self.new_symbol_token(TokenKind::LeftParen)),
                    ')' => Ok(self.new_symbol_token(TokenKind::RightParen)),
                    '[' => Ok(self.new_symbol_token(TokenKind::LeftBracket)),
                    ']' => Ok(self.new_symbol_token(TokenKind::RightBracket)),
                    '{' => Ok(self.new_symbol_token(TokenKind::LeftBrace)),
                    '}' => Ok(self.new_symbol_token(TokenKind::RightBrace)),
                    ',' => Ok(self.new_symbol_token(TokenKind::Comma)),
                    '.' => Ok(self.new_symbol_token(TokenKind::Period)),
                    ':' => Ok(self.new_symbol_token(TokenKind::Colon)),
                    '?' => Ok(self.new_symbol_token(TokenKind::Question)),

                    '!' => self.scan_conditional(&[('=', TokenKind::NotEqual)], TokenKind::Bang),
                    '=' => self.scan_conditional(&[('=', TokenKind::Equality)], TokenKind::Equal),
                    '>' => {
                        self.scan_conditional(&[('=', TokenKind::GreaterEq)], TokenKind::Greater)
                    }
                    '<' => self.scan_conditional(&[('=', TokenKind::LessEq)], TokenKind::Less),

                    _ => Err("Unknown character"),
                },
                None => Err("Cannot read character."),
            }
        }
    }
}

pub fn scan(input: String) -> Result<Vec<Token>, &'static str> {
    let mut keywords = HashMap::new();
    keywords.insert("let", TokenKind::Let);
    keywords.insert("if", TokenKind::If);
    keywords.insert("else", TokenKind::Else);
    keywords.insert("then", TokenKind::Then);
    keywords.insert("function", TokenKind::Function);
    keywords.insert("module", TokenKind::Module);
    keywords.insert("record", TokenKind::Record);
    keywords.insert("and", TokenKind::And);
    keywords.insert("or", TokenKind::Or);
    keywords.insert("end", TokenKind::End);
    keywords.insert("true", TokenKind::Boolean(true));
    keywords.insert("false", TokenKind::Boolean(false));
    keywords.insert("rec", TokenKind::Recursive);
    keywords.insert("extern", TokenKind::Extern);
    keywords.insert("enum", TokenKind::Enum);
    keywords.insert("case", TokenKind::Case);
    keywords.insert("of", TokenKind::Of);

    let mut context = ScannerContext {
        current: 0,
        start: 0,
        line: 1,
        index: 0,
        input: input.into_bytes(),
        tokens: Vec::new(),
        keywords,
    };

    loop {
        let token = context.scan_token()?;

        match token.kind {
            TokenKind::Eof => break,
            _ => {
                context.tokens.push(token);
                continue;
            }
        };
    }

    Ok(context.tokens)
}
