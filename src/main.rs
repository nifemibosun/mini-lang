// extern crate mini;
mod scanner;
mod parser;

use std::process::exit;
use std::env;
use std::io::Result;
use std::fs;

use scanner::scanner::Scanner;
use mini::MiniState;


fn help_msg() {
    println!("Usage: mini <option> | <filename.mini>\n");
    println!("Options:");
    println!("  -h, --help         Shows this help message");
    println!("  -v, --version      Shows version information");
}

fn help_args(args: Vec<String>, state: &mut MiniState) {
    if args.len() > 2 {
        help_msg();
        exit(64);
    } else if  args.len() == 2 {
        match args[1].as_str() {
            "--help" | "-h" => {
                help_msg();
                exit(0);
            }
            "--version" | "-v" | "-V" => {
                println!("mini v0.1.0");
                exit(0);
            }
            _ => {
                if let Err(err) = run_file(state, &args[1]) {
                    eprintln!("Error: {}", err);
                    exit(74);
                }
            }
        }
    } else {
        help_msg();
        exit(64);
    }
}


fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() == 1 {
        help_msg();
        exit(64);
    }

    let mut state = MiniState::new();
    help_args(args, &mut state);
}

fn run_file(state: &mut MiniState, path: &str) -> Result<()> {
    let content = fs::read_to_string(path)?;
    run(state, &content);
    
    if state.had_error {
        exit(65);
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