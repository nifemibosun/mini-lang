mod scanner;
mod parser;

use std::process::exit;
use std::env;
use std::io::{self, Write, BufRead, Result};
use std::fs;

use scanner::scanner::Scanner;
use mini::MiniState;


fn help_msg() {
    println!("Usage: mini <flag> <filename.mini>\n");
    println!("Flags:");
    println!("  --help          Shows this help message");
    println!("  --version       Shows version information");
    println!("  -h              Shows this help message");
    println!("  -v              Shows version information");
    println!("  run             Run the specified file");
    println!("  comp            Compile the specified file");
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let mut state = MiniState::new();
    
    if args.len() > 3 {
        help_msg();
        exit(64);
    } else if  args.len() == 2 {
        match args[1].as_str() {
            "--help" | "-h" => {
                help_msg();
                exit(0);
            }
            "--version" | "-v" => {
                println!("mini v0.1.0");
                exit(0);
            }
            _ => {
                match args[1].as_str() {
                    "run" => println!("Usage: mini run <filename.mini>"),
                    "comp" => println!("Usage: mini comp <filename.mini>"),
                    _ => {
                        help_msg();
                    }
                }
                // exit(64);
            }
        }

    } else if args.len() == 3 {
        match args[1].as_str() {
            "run" => {
                if let Err(err) = run_file(&mut state, &args[2]) {
                    println!("Error: {}", err);
                    exit(74);
                }
            }
            "comp" => {
                if let Err(err) = run_file(&mut state, &args[2]) {
                    println!("Error: {}", err);
                    exit(74);
                }
            }
            _ => {
                match args[1].as_str() {
                    "--help" | "-h" => {
                        match args[2].as_str() {
                            "--version" | "-v" => println!("Usage: mini --version  |  mini -v"),
                            "run" => println!("Usage: mini run <filename.mini>"),
                            "comp" => println!("Usage: mini comp <filename.mini>"),
                            _ => {
                                help_msg();
                            }
                        }
                    }
                    _ => {
                        help_msg();
                    }
                }
            }
        }
    } else {
        if let Err(e) = run_prompt(&mut state) {
            println!("Error: {}", e);
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