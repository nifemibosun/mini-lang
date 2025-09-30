mod parser;
mod scanner;

use std::env;
use std::fs;
use std::io::Result;
use std::process::exit;

use parser::Parser;
use scanner::{token::Position, Scanner};

fn help_msg() {
    println!("Usage: mini <file.mini>\n");
    println!("Options:");
    println!("  -h, --help         Shows this help message");
    println!("  -v, --version      Shows version information");
}

fn help_args(args: Vec<String>, state: &mut MiniState) {
    if args.len() > 2 {
        help_msg();
        exit(64);
    } else if args.len() == 2 {
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
                let filename = &args[1];
                if !filename.ends_with(".mini") {
                    eprintln!("Error: file must end with `.mini`");
                    exit(65);
                }

                if let Err(err) = run_file(state, filename) {
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

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct MiniState {
    pub had_error: bool,
}

impl MiniState {
    pub fn new() -> Self {
        MiniState { had_error: false }
    }

    pub fn error(&mut self, pos: Position, message: &str) {
        self.report(pos, message);
    }

    fn report(&mut self, pos: Position, message: &str) {
        eprintln!("Error at {}:{}: {}", pos.line, pos.col, message);
        self.had_error = true;
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
    let mut parser = Parser::new(tokens);

    println!("{:#?}", parser.parse().unwrap());
}
