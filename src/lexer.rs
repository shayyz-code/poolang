// use regex::Regex;

#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    Identifier(String),
    Assignment,
    ShortAssignment,
    SemiColon,
    LeftCurly,
    RightCurly,
    LeftParen,
    RightParen,
    LeftBracket,
    RightBracket,
    Colon,
    DoubleColon,
    Dot,
    DoubleDot,
    Comma,
    RightArrow,
    // DoubleSlash,

    // keywords
    Use,
    Pub,
    Struct,
    Inherits,
    Implements,
    Trait,
    If,
    Else,
    Elif,
    Return,
    While,
    For,
    In,
    Step,
    Poo,
    Poof,
    Mut,
    Otherwise,

    // values
    True,
    False,
    Int(i64),
    Float(f64),
    Char(char),
    String(String),

    // Types
    TVoid,
    TBool,
    TInt,
    TFloat,
    TChar,
    TString,
    TVec,
    TMap,
    TSelf,

    // maths
    Plus,
    Minus,
    Multiply,
    Divide,

    // comparison
    And,
    Or,
    LessThan,
    GreaterThan,
    Equal,
    Not,
    NotEqual,

    EOF,
}

pub struct Lexer {
    input: String,
    pos: usize,
}

impl Lexer {
    pub fn new(input: String) -> Self {
        Lexer { input, pos: 0 }
    }

    pub fn at(&mut self) -> char {
        if self.pos < self.input.len() {
            self.input[self.pos..].chars().next().unwrap()
        } else {
            '\0' // Null char to signify end
        }
    }

    pub fn advance(&mut self) {
        if self.pos < self.input.len() {
            self.pos += self.at().len_utf8();
        }
    }

    pub fn peek_next_token(&mut self) -> Token {
        self.peek_next_token_checked()
            .unwrap_or_else(|message| panic!("{message}"))
    }

    pub(crate) fn peek_next_token_checked(&mut self) -> Result<Token, String> {
        let current_pos = self.pos;

        let next_token = self.next_token_checked();
        self.pos = current_pos;

        next_token
    }

    pub fn peek_next_char(&mut self) -> char {
        let current_pos = self.pos;
        self.advance();

        let next_char = self.at();
        self.pos = current_pos;

        next_char
    }

    pub fn next_token(&mut self) -> Token {
        self.next_token_checked()
            .unwrap_or_else(|message| panic!("{message}"))
    }

    pub(crate) fn next_token_checked(&mut self) -> Result<Token, String> {
        if self.pos >= self.input.len() {
            return Ok(Token::EOF);
        }

        let current_char = self.at();

        let token = match current_char {
            '/' => {
                self.advance();
                if self.at() == '/' {
                    self.advance();

                    while self.at() != '\0' {
                        if self.at() == '/' && self.peek_next_char() == '/' {
                            self.advance();
                            self.advance();
                            return self.next_token_checked();
                        }
                        self.advance();
                    }

                    return Err("Unterminated comment".to_string());
                } else {
                    Token::Divide
                }
            }
            '0'..='9' => {
                let mut num_str: String = self.input[self.pos..]
                    .chars()
                    .take_while(|c| c.is_ascii_digit())
                    .collect();
                self.pos += num_str.len();

                if self.at() == '.' {
                    if self.peek_next_char() == '.' {
                        // It's a range
                        Token::Int(num_str.parse().unwrap())
                    } else {
                        self.advance();
                        // It's a float
                        let num_str2: String = self.input[self.pos..]
                            .chars()
                            .take_while(|c| c.is_ascii_digit())
                            .collect();
                        self.pos += num_str2.len();
                        num_str.push('.');
                        num_str.push_str(&num_str2);
                        Token::Float(num_str.parse::<f64>().unwrap())
                    }
                } else {
                    Token::Int(num_str.parse().unwrap())
                }
            }

            '\"' => {
                self.advance();
                let mut str_val = String::new();

                loop {
                    if self.at() == '\0' {
                        return Err("Unterminated string literal".to_string());
                    }

                    if self.at() == '\"' {
                        self.advance();
                        break;
                    }

                    if self.at() == '\\' {
                        self.advance();
                        let escaped = self.at();
                        if escaped == '\0' {
                            return Err("Unterminated string literal".to_string());
                        }
                        match escaped {
                            'n' => str_val.push('\n'),
                            't' => str_val.push('\t'),
                            '\\' => str_val.push('\\'),
                            '\"' => str_val.push('\"'),
                            _ => return Err(format!("Unknown escape sequence: \\{escaped}")),
                        }
                    } else {
                        str_val.push(self.at());
                    }
                    self.advance();
                }
                Token::String(str_val)
            }
            '\'' => {
                self.advance();
                let mut char_val: char = ' ';

                loop {
                    if self.at() == '\0' {
                        return Err("Unterminated character literal".to_string());
                    }

                    if self.at() == '\'' {
                        self.advance();
                        break;
                    }

                    if self.at() == '\\' {
                        self.advance();
                        let escaped = self.at();
                        if escaped == '\0' {
                            return Err("Unterminated character literal".to_string());
                        }
                        match escaped {
                            'n' => char_val = '\n',
                            't' => char_val = '\t',
                            '\\' => char_val = '\\',
                            '\"' => char_val = '\"',
                            '\'' => char_val = '\'',
                            _ => return Err(format!("Unknown escape sequence: \\{escaped}")),
                        }
                    } else {
                        char_val = self.at();
                    }
                    self.advance();
                }
                Token::Char(char_val)
            }

            ';' => {
                self.advance();
                Token::SemiColon
            }
            '+' => {
                self.advance();
                Token::Plus
            }
            '-' => {
                self.advance();
                Token::Minus
            }
            '*' => {
                self.advance();
                Token::Multiply
            }

            '{' => {
                self.advance();
                Token::LeftCurly
            }
            '}' => {
                self.advance();
                Token::RightCurly
            }
            '(' => {
                self.advance();
                Token::LeftParen
            }
            ')' => {
                self.advance();
                Token::RightParen
            }
            '[' => {
                self.advance();
                Token::LeftBracket
            }
            ']' => {
                self.advance();
                Token::RightBracket
            }
            ':' => {
                self.advance();
                if self.at() == ':' {
                    self.advance();
                    Token::DoubleColon
                } else {
                    Token::Colon
                }
            }
            '.' => {
                self.advance();
                if self.at() == '.' {
                    self.advance();
                    Token::DoubleDot
                } else {
                    Token::Dot
                }
            }
            ',' => {
                self.advance();
                Token::Comma
            }
            '<' => {
                self.advance();
                if self.at() == ':' {
                    self.advance();
                    Token::ShortAssignment
                } else {
                    Token::LessThan
                }
            }
            '>' => {
                self.advance();
                if self.at() == '>' {
                    self.advance();
                    Token::RightArrow
                } else {
                    Token::GreaterThan
                }
            }
            '=' => {
                self.advance();
                if self.at() == '=' {
                    self.advance();
                    Token::Equal
                } else {
                    Token::Assignment
                }
            }
            '!' => {
                self.advance();
                if self.at() == '=' {
                    self.advance();
                    Token::NotEqual
                } else {
                    Token::Not
                }
            }
            'a'..='z' | 'A'..='Z' | '_' => {
                let id_str: String = self.input[self.pos..]
                    .chars()
                    .take_while(|c| c.is_ascii_alphanumeric() || *c == '_')
                    .collect();
                self.pos += id_str.len();

                match id_str.as_str() {
                    "use" => Token::Use,
                    "poo" => Token::Poo,
                    "poof" => Token::Poof,
                    "mut" => Token::Mut,
                    "pub" => Token::Pub,
                    "struct" => Token::Struct,
                    "inherits" => Token::Inherits,
                    "impl" => Token::Implements,
                    "trait" => Token::Trait,
                    "if" => Token::If,
                    "else" => Token::Else,
                    "elif" => Token::Elif,
                    "and" => Token::And,
                    "or" => Token::Or,
                    "not" => Token::Not,
                    "while" => Token::While,
                    "for" => Token::For,
                    "in" => Token::In,
                    "step" => Token::Step,
                    "return" => Token::Return,
                    "otherwise" => Token::Otherwise,

                    // values
                    "true" => Token::True,
                    "false" => Token::False,

                    // types
                    "void" => Token::TVoid,
                    "bool" => Token::TBool,
                    "int" => Token::TInt,
                    "float" => Token::TFloat,
                    "char" => Token::TChar,
                    "str" => Token::TString,
                    "vec" => Token::TVec,
                    "map" => Token::TMap,
                    "Self" => Token::TSelf,

                    _ => Token::Identifier(id_str),
                }
            }

            _ if current_char.is_whitespace() => {
                self.advance();
                return self.next_token_checked(); // Skip whitespace and get the next token
            }
            _ => return Err(format!("Unexpected character: {current_char}")),
        };

        Ok(token)
    }
}
