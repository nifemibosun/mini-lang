use crate::scanner::Scanner;

use std::fs;
use std::process;
use std::io::{self, BufRead, Write};
use std::path::Path;

pub struct Lox {
    has_error: bool,
}

impl Mini {
    fn default() -> Self {
        Mini { has_error: false }
    }
    
    pub fn run_file(path: &str) -> io::Result<()> {
        let mini = Mini::default();
        let bytes = fs::read(Path::new(path))?;
        let content = String::from_utf8(bytes).expect("Failed to parse bytes");
        Mini::run(content);
        if mini.has_error {
            process::exit(65);
        }
        Ok(())
    }

    pub fn run_prompt() -> io::Result<()> {
        let stdin = io::stdin();
        let mut reader = stdin.lock();
        let mut mini = Mini::default();
        loop {
            print!("> ");
            io::stdout().flush()?;
            let mut line = String::new();
            let bytes_read = reader.read_line(&mut line)?;
            if bytes_read == 0 {
                break;
            }
            Mini::run(line.trim().to_string());
            lox.has_error = false;
        }

        Ok(())
    }

    fn run(source: String) {
        let mut scanner = Scanner::new(source);
        let tokens = scanner.scan_tokens();

        // For now, just print the tokens.
        for token in tokens {
            println!("{:?}", token);
        }
    }

    pub fn error(line: usize, message: &str) {
        Mini::report(line, "", message);
    }

    fn report(line: usize, position: &str, message: &str) {
        eprintln!("[Line {}] Error {}: {}", line, position, message);
    }
}
  