use crate::scanner::Scanner;
use crate::tokens::{ Token, TokenType };

use std::fs;
use std::io::{BufRead, Write, stdin, stdout, Result, Error, ErrorKind};
use std::process;
use std::path::Path;

#[derive(Debug, Clone)]
pub struct Mini {
    has_error: bool,
}

impl Mini {
    pub fn new() -> Self {
        Mini { 
            has_error: false,
        }
    }

    pub fn run_file(&self, path: &str) -> Result<()> {
        if !path.ends_with(".mini") {
            return Err(Error::new(ErrorKind::InvalidInput, "Error: Only files with '.mini' extension are supported."));
        }
    
        let bytes  = fs::read(Path::new(path))?;
        let content = String::from_utf8(bytes).map_err(|_| {
            Error::new(ErrorKind::InvalidData, "Failed to parse bytes into a valid UTF-8 string")
        })?;
    
        Mini::run(content);
    
        if self.has_error {
            process::exit(65);
        }
    
        Ok(())
    }

    pub fn run_prompt(&mut self) -> Result<()> {
        let stdin = stdin();
        let mut reader = stdin.lock();
    
        loop {
            print!("> ");
            stdout().flush()?;
    
            let mut line = String::new();
            let bytes_read = reader.read_line(&mut line)?;
            if bytes_read == 0 {
                break;
            }
    
            let trimmed_line = line.trim();
            if !trimmed_line.is_empty() {
                Mini::run(trimmed_line.to_string());
            }
            self.has_error = false;
        }
        Ok(())
    }

    fn run(source: String) {
        let mut scanner = Scanner::new(source);
        let tokens = scanner.scan_tokens();

        for token in tokens {
            println!("{:?}", token);
        }
    }

    // pub fn error(line: usize, message: &str) {
    //     Mini::report(line, "", message);
    // }

    fn report(line: usize, position: &str, message: &str) {
        eprintln!("[Line {}] Error {}: {}", line, position, message);
    }

    pub fn error(token: Token, message: &str) {
        if token.token_type == TokenType::EOF {
            Mini::report(token.line, " at end", message);
        } else {
            let position = format!("at {}", token.lexeme);
            let position_ref: &str = &position;

            Mini::report(token.line, position_ref, message);
        }
    }
}
