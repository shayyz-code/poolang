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
            '/' => {
                self.advance();
                if self.at() == '/' {
                    self.advance();
                    while self.at() != '/' && self.peek_next_char() != '/' {
                        self.advance();
                    }
                    self.advance(); // above while miss 1 char
                    self.advance(); // first /
                    self.advance(); // second /
                    self.next_token()
                } else {
                    Token::Divide
                }
            }
            '0'..='9' => {
                let mut num_str: String = self.input[self.pos..]
                    .chars()
                    .take_while(|c| c.is_digit(10))
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
                            .take_while(|c| c.is_digit(10))
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
                while self.at() != '\"' && self.at() != '\0' {
                    if self.at() == '\\' {
                        self.advance();
                        match self.at() {
                            'n' => str_val.push('\n'),
                            't' => str_val.push('\t'),
                            '\\' => str_val.push('\\'),
                            '\"' => str_val.push('\"'),
                            _ => panic!("Unknown escape sequence"),
                        }
                    } else {
                        str_val.push(self.at());
                    }
                    self.advance();
                }
                self.advance(); // Consume closing quote
                Token::String(str_val)
            }
            '\'' => {
                self.advance();
                let mut char_val: char = ' ';
                while self.at() != '\'' && self.at() != '\0' {
                    if self.at() == '\\' {
                        self.advance();
                        match self.at() {
                            'n' => char_val = '\n',
                            't' => char_val = '\t',
                            '\\' => char_val = '\\',
                            '\"' => char_val = '\"',
                            '\'' => char_val = '\'',
                            _ => panic!("Unknown escape sequence"),
                        }
                    } else {
                        char_val = self.at();
                    }
                    self.advance();
                }
                self.advance(); // Consume closing quote
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
                    .take_while(|c| c.is_alphanumeric() || *c == '_')
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
                self.next_token() // Skip whitespace and get the next token
            }
            _ => panic!("Unexpected character: {}", current_char),
        }
    }
}
