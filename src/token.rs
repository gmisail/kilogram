use std::fmt::Display;

#[derive(Clone, PartialEq)]
pub enum TokenKind {
    // Symbols (single-width)
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    LeftBracket,
    RightBracket,
    Quote,
    Comma,
    Period,
    Colon,
    Bang,
    Equal,
    Add,
    Sub,
    Mult,
    Div,
    Question,

    // Symbols (double-width)
    ThinArrow,
    ThickArrow,
    Equality,
    GreaterEq,
    Greater,
    LessEq,
    Less,
    NotEqual,

    // Literals
    Identifier(String),
    Integer(i32),
    Float(f32),
    Boolean(bool),
    Str(String),

    // Keywords
    Let,
    If,
    Else,
    Then,
    Function,
    Module,
    Record,
    And,
    Or,
    End,

    Eof,
}

pub struct Token {
    pub kind: TokenKind,
    pub line: usize,
    pub relative: usize,
    pub absolute: usize,
    pub length: usize,
}

impl Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                TokenKind::LeftParen => "'('",
                TokenKind::RightParen => "')'",
                TokenKind::LeftBrace => "'{'",
                TokenKind::RightBrace => "'}'",
                TokenKind::LeftBracket => "'['",
                TokenKind::RightBracket => "']'",
                TokenKind::Comma => "','",
                TokenKind::Period => "'.'",
                TokenKind::Sub => "'-'",
                TokenKind::Add => "'+'",
                TokenKind::Div => "'/'",
                TokenKind::Mult => "'*'",
                TokenKind::Bang => "'!'",
                TokenKind::Equal => "'='",
                TokenKind::Equality => "'=='",
                TokenKind::NotEqual => "!=",
                TokenKind::Greater => "'>'",
                TokenKind::GreaterEq => "'>='",
                TokenKind::Less => "'<'",
                TokenKind::LessEq => "'<='",
                TokenKind::Question => "?",
                TokenKind::Quote => "\"",
                TokenKind::Colon => ":",

                TokenKind::ThinArrow => "->",
                TokenKind::ThickArrow => "=>",

                TokenKind::Identifier(val) => val,
                TokenKind::Str(val) => val,
                TokenKind::Integer(_) => "(literal | type: integer)",
                TokenKind::Float(_) => "(literal | type: float)",
                TokenKind::Boolean(_) => "(literal | type: bool)",

                TokenKind::Let => "'let'",
                TokenKind::End => "'end'",
                TokenKind::Then => "'then'",
                TokenKind::Module => "'module'",
                TokenKind::Record => "'record'",
                TokenKind::And => "'and'",
                TokenKind::Else => "'else'",
                TokenKind::Function => "'function'",
                TokenKind::If => "'if'",
                TokenKind::Or => "'or'",
                TokenKind::Eof => "<EOF>",
            }
        )
    }
}
