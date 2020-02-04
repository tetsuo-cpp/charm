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

#[derive(Debug, PartialEq, Clone)]
enum TokenKind {
    Identifier,
    Number,
    If,
    Else,
    EndOfFile,
}

#[derive(Debug, PartialEq, Clone)]
struct Token<'a> {
    kind: TokenKind,
    value: Option<&'a [u8]>,
}

impl Token<'_> {
    pub fn new(kind: TokenKind, value: Option<&[u8]>) -> Token {
        Token { kind, value }
    }

    pub fn is_eof(&self) -> bool {
        if let TokenKind::EndOfFile = &self.kind {
            true
        } else {
            false
        }
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

static KEYWORDS: [(&'static [u8], Token); 2] = [
    (
        b"if",
        Token {
            kind: TokenKind::If,
            value: None,
        },
    ),
    (
        b"else",
        Token {
            kind: TokenKind::Else,
            value: None,
        },
    ),
];

struct Lexer<'a> {
    source: &'a [u8],
    current_pos: usize,
    current_char: Option<&'a u8>,
}

impl<'a> Lexer<'a> {
    pub fn new(source: &str) -> Lexer {
        let mut lexer = Lexer {
            source: source.as_bytes(),
            current_pos: 0,
            current_char: None,
        };
        lexer.position_char();
        lexer
    }

    pub fn lex(&mut self) -> Result<Token<'a>, Box<dyn Error>> {
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
                } else if current_char.is_ascii() {
                    self.lex_symbol()
                } else {
                    Err(Box::new(LexerError {
                        msg: String::from("unrecognised token"),
                    }))
                }
            }
            None => Ok(Token::new(TokenKind::EndOfFile, None)),
        }
    }

    fn lex_number(&mut self) -> Result<Token<'a>, Box<dyn Error>> {
        let start = self.current_pos;
        loop {
            match self.current_char {
                Some(current_char) => {
                    if !current_char.is_ascii_digit() {
                        break;
                    }
                }
                None => break,
            }
            self.read_char();
        }
        let end = self.current_pos;
        Ok(Token::new(
            TokenKind::Number,
            Some(&self.source[start..end]),
        ))
    }

    fn lex_identifier(&mut self) -> Result<Token<'a>, Box<dyn Error>> {
        let start = self.current_pos;
        loop {
            match self.current_char {
                Some(current_char) => {
                    if !current_char.is_ascii_alphanumeric() {
                        break;
                    }
                }
                None => break,
            }
            self.read_char();
        }
        let end = self.current_pos;
        let value = &self.source[start..end];
        for (kw, kw_tok) in KEYWORDS.iter() {
            if *kw == value {
                return Ok(kw_tok.clone());
            }
        }
        Ok(Token::new(TokenKind::Identifier, Some(value)))
    }

    fn lex_symbol(&mut self) -> Result<Token<'a>, Box<dyn Error>> {
        Ok(Token::new(TokenKind::EndOfFile, None))
    }

    fn read_char(&mut self) {
        self.current_pos += 1;
        self.position_char();
    }

    fn position_char(&mut self) {
        self.current_char = self.source.get(self.current_pos);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn lex<'a>(source: &'a str) -> Result<Vec<Token>, Box<dyn Error>> {
        let mut tokens = Vec::new();
        let mut lexer = Lexer::new(source);
        loop {
            let token = lexer.lex()?;
            if token.is_eof() {
                break;
            }
            tokens.push(token);
        }
        Ok(tokens)
    }

    #[test]
    fn test_lex_identifier() -> Result<(), Box<dyn Error>> {
        assert_eq!(
            lex("foo")?,
            vec![Token::new(TokenKind::Identifier, Some(b"foo"))]
        );
        Ok(())
    }

    #[test]
    fn test_lex_number() -> Result<(), Box<dyn Error>> {
        assert_eq!(
            lex("123")?,
            vec![Token::new(TokenKind::Number, Some(b"123"))]
        );
        Ok(())
    }

    #[test]
    fn test_lex_kw() -> Result<(), Box<dyn Error>> {
        assert_eq!(
            lex("if else")?,
            vec![
                Token::new(TokenKind::If, None),
                Token::new(TokenKind::Else, None)
            ]
        );
        Ok(())
    }
}

pub fn run(config: Config) -> Result<(), Box<dyn Error>> {
    let source = fs::read_to_string(config.file_name)?;
    let mut lexer = Lexer::new(&source);
    let mut tokens = Vec::new();
    loop {
        let token = lexer.lex()?;
        if token.is_eof() {
            break;
        }
        tokens.push(token);
    }
    Ok(())
}
