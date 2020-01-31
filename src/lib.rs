use std::error::Error;
use std::fs;

pub struct Config<'a> {
    file_name: &'a str,
}

impl Config<'_> {
    pub fn new(file_name: &str) -> Config {
        Config { file_name }
    }
}

struct Lexer<'a> {
    source: &'a str,
    current_pos: usize,
}

impl Lexer<'_> {
    pub fn new(source: &str) -> Lexer {
        Lexer {
            source,
            current_pos: 0,
        }
    }
}

pub fn run(config: Config) -> Result<(), Box<dyn Error>> {
    let source = fs::read_to_string(config.file_name)?;
    let lexer = Lexer::new(&source);
    Ok(())
}
