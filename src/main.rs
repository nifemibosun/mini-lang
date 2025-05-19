mod scanner;
mod parser;

use std::process::exit;
use std::env;
use std::io::{self, Write, BufRead, Result};
use std::fs;

use scanner::scanner::Scanner;
use mini::MiniState;


fn main() {
    let args: Vec<String> = env::args().collect();
    let mut state = MiniState::new();
    
    if args.len() > 2 {
        eprintln!("Usage: mini run <filename.mini>");
        exit(64);
    } else if args.len() == 2 {
        if let Err(err) = run_file(&mut state, &args[1]) {
            eprintln!("Error: {}", err);
            exit(74);
        }
    } else {
        if let Err(e) = run_prompt(&mut state) {
            eprintln!("Error: {}", e);
            exit(74);
        }
    }
}

fn run_file(state: &mut MiniState, path: &str) -> Result<()> {
    let content = fs::read_to_string(path)?;
    run(state, &content);
    
    if state.had_error {
        exit(65);
    }
    
    Ok(())
}

fn run_prompt(state: &mut MiniState) -> Result<()> {
    let stdin = io::stdin();
    let mut reader = stdin.lock();
    let mut stdout = io::stdout();
    
    loop {
        print!("> ");
        stdout.flush()?;
        
        let mut line = String::new();
        let bytes_read = reader.read_line(&mut line)?;
        
        if bytes_read == 0 {
            break;
        }
        
        let line = line.trim_end();
        
        if line == "exit" {
            break;
        }
        
        run(state, line);
    }
    
    Ok(())
}

fn run(state: &mut MiniState, source: &str) {
    let mut scanner = Scanner::new(source, state);
    let (tokens, _) = scanner.scan_tokens();

    for token in tokens {
        println!("{:?}", token);
    }
}