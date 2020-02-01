use std::error::Error;
use std::fmt;
use std::fs;

pub struct Config<'a> {
    file_name: &'a str,
}

impl Config<'_> {
    pub fn new(file_name: &str) -> Config {
        Config { file_name }
    }
}

enum TokenKind {
    Identifier,
    Number,
    EndOfFile,
}

struct Token<'a> {
    kind: TokenKind,
    value: Option<&'a [u8]>,
}

impl Token<'_> {
    pub fn new(kind: TokenKind, value: Option<&[u8]>) -> Token {
        Token { kind, value }
    }
}

#[derive(Debug)]
struct LexerError {
    msg: String,
}

impl Error for LexerError {}

impl fmt::Display for LexerError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.msg)
    }
}

struct Lexer<'a> {
    source: &'a [u8],
    current_pos: usize,
    current_char: Option<&'a u8>,
}

impl Lexer<'_> {
    pub fn new(source: &str) -> Lexer {
        let mut lexer = Lexer {
            source: source.as_bytes(),
            current_pos: 0,
            current_char: None,
        };
        lexer.read_char();
        lexer
    }

    pub fn lex(&mut self) -> Result<Token, Box<dyn Error>> {
        // Trim leading whitespace.
        loop {
            match self.current_char {
                Some(current_char) => {
                    if !current_char.is_ascii_whitespace() {
                        break;
                    }
                }
                None => break,
            };
            self.read_char();
        }
        match self.current_char {
            Some(current_char) => {
                if current_char.is_ascii_digit() {
                    self.lex_number()
                } else if current_char.is_ascii_alphabetic() {
                    self.lex_identifier()
                } else {
                    Err(Box::new(LexerError {
                        msg: String::from("unrecognised token"),
                    }))
                }
            }
            None => Ok(Token::new(TokenKind::EndOfFile, None)),
        }
    }

    fn lex_number(&mut self) -> Result<Token, Box<dyn Error>> {
        let start = self.current_pos;
        let mut end = start;
        loop {
            match self.current_char {
                Some(current_char) => {
                    if !current_char.is_ascii_digit() {
                        break;
                    }
                }
                None => break,
            }
            end += 1;
            self.read_char();
        }
        Ok(Token::new(
            TokenKind::Number,
            Some(&self.source[start..end]),
        ))
    }

    fn lex_identifier(&mut self) -> Result<Token, Box<dyn Error>> {
        let start = self.current_pos;
        let mut end = start;
        loop {
            match self.current_char {
                Some(current_char) => {
                    if current_char.is_ascii_alphanumeric() {
                        break;
                    }
                }
                None => break,
            }
            end += 1;
            self.read_char();
        }
        Ok(Token::new(
            TokenKind::Identifier,
            Some(&self.source[start..end]),
        ))
    }

    fn read_char(&mut self) {
        self.current_char = self.source.get(self.current_pos);
    }
}

pub fn run(config: Config) -> Result<(), Box<dyn Error>> {
    let source = fs::read_to_string(config.file_name)?;
    let lexer = Lexer::new(&source);
    Ok(())
}
