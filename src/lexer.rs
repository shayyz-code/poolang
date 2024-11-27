// src/lexer.rs
// use regex::Regex;

#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    Identifier(String),
    Assignment,
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
    
    // keywords
    Use,
    If,
    Else,
    Elif,
    Return,
    While,
    For,
    In,
    Poo,
    Poof,
    Mut,
    
    // values
    
    True,
    False,
    Int(i64),
    Float(f64),
    String(String),

    // Types
    TVoid,
    TBool,
    TInt,
    TFloat,
    TString,

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
        self.pos += 1;
    }

    pub fn peek_next_token(&mut self) -> Token {
        let current_pos = self.pos;

        let next_token = self.next_token();
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
        if self.pos >= self.input.len() {
            return Token::EOF;
        }

        let current_char = self.at();
        
        match current_char {
            '0'..='9' => {
                let mut num_str1: String = self.input[self.pos..]
                    .chars()
                    .take_while(|c| c.is_digit(10))
                    .collect();
                self.pos += num_str1.len();
                if self.at() == '.' {
                    if self.peek_next_char() == '.' {
                        Token::Int(num_str1.parse().unwrap())
                    } else {
                        self.advance();
                        let num_str2: String = self.input[self.pos..].chars().take_while(|c| c.is_digit(10)).collect();
                        self.pos += num_str2.len();
                        num_str1.push('.');
                        num_str1.push_str(&num_str2);
                        Token::Float(num_str1.parse::<f64>().unwrap())
                    }
                } else {
                    Token::Int(num_str1.parse().unwrap())
                }
            }
            '\"' => {
                self.advance();
                let str:String = self.input[self.pos..].chars().take_while(|c| *c != '\"').collect();
                self.pos += str.len() + 1;
                Token::String(str)
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
            '/' => {
                self.advance();
                Token::Divide
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
                if self.at() == '<' {
                    self.advance();
                    Token::Assignment
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
                    self.next_token()
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
            'a'..='z' | 'A'..='Z' => {
                let id_str: String = self.input[self.pos..]
                    .chars()
                    .take_while(|c| c.is_alphanumeric())
                    .collect();
                self.pos += id_str.len();
                
                match id_str.as_str() {
                    "use" => Token::Use,
                    "poo" => Token::Poo,
                    "poof" => Token::Poof,
                    "mut" => Token::Mut,
                    "if" => Token::If,
                    "else" => Token::Else,
                    "elif" => Token::Elif,
                    "and" => Token::And,
                    "or" => Token::Or,
                    "not" => Token::Not,
                    "while" => Token::While,
                    "for" => Token::For,
                    "in" => Token::In,
                    "return" => Token::Return,

                    // values
                    "true" => Token::True,
                    "false" => Token::False,

                    // types
                    "void" => Token::TVoid,
                    "bool" => Token::TBool,
                    "int" => Token::TInt,
                    "float" => Token::TFloat,
                    "string" => Token::TString,
                    _ => Token::Identifier(id_str),
                }
            }
            
            _ if current_char.is_whitespace() => {
                self.advance();
                self.next_token() // Skip whitespace and get the next token
            }
            _ => panic!("Unexpected character: {}", current_char),
        }
    }
}
