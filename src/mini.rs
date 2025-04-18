use std::io::{ stdin, stdout, BufRead, Write, Error, ErrorKind, Result };
use std::fs;
use std::path::Path;
use crate::scanner::Scanner;


pub struct Mini {
    pub has_error: bool,
}

#[allow(unused)]
impl Mini {
    pub fn new() -> Self {
        Self { has_error: false }
    }

    pub fn run_file(&self, path: &str) -> Result<()> {
        let bytes = fs::read(Path::new(path))?;
        let contents = String::from_utf8(bytes).map_err(|_| {
            Error::new(ErrorKind::InvalidData, "Failed to parse bytes into a valid UTF-8 string")
        })?;

        self.run(contents);

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

            if trimmed_line == "exit" {
                break;
            }

            if !trimmed_line.is_empty() {
                self.run(trimmed_line.to_string());
            }
        }
        Ok(())
    }

    pub fn run(&self, source: String) {
        let mut scanner = Scanner::new(source);
        let tokens = scanner.scan_tokens();

        for token in tokens {
            println!("{:?}", token);
        }
    }

    pub fn error(&mut self, line: usize, message: &str) {
        self.report(line, "", message);
    }

    fn report(&mut self, line: usize, pos: &str, message: &str) {
        eprintln!("[Line {}] Error {}: {}", line, pos, message);

        self.has_error = true;
    }
}
