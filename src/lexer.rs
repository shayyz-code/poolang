// src/lexer.rs
// use regex::Regex;

#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    Number(i64),
    Identifier(String),
    Assignment,
    OpenScope,
    SemiColon,

    // keywords
    If,
    Else,
    Elif,
    Return,
    While,
    Poo,
    Poof,
    Mut,


    // maths
    Plus,
    Minus,
    Multiply,
    Divide,
    
    // comparison
    LessThan,
    GreaterThan,
    Equal,
    Not,
    NotEqual,

    LeftParen,
    RightParen,
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

    pub fn next_token(&mut self) -> Token {
        if self.pos >= self.input.len() {
            return Token::EOF;
        }

        let current_char = self.at();
        
        match current_char {
            '0'..='9' => {
                let num_str: String = self.input[self.pos..]
                    .chars()
                    .take_while(|c| c.is_digit(10))
                    .collect();
                self.pos += num_str.len();
                Token::Number(num_str.parse().unwrap())
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
            '(' => {
                self.advance();
                Token::LeftParen
            }
            ')' => {
                self.advance();
                Token::RightParen
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
                    Token::OpenScope
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
                    "poo" => Token::Poo,
                    "poof" => Token::Poof,
                    "mut" => Token::Mut,
                    "if" => Token::If,
                    "else" => Token::Else,
                    "elif" => Token::Elif,
                    "while" => Token::While,
                    "return" => Token::Return,
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
