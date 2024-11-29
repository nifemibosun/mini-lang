mod ast;
mod mini;
mod parser;
mod scanner;
mod tokens;

use std::env::args;
use std::process;
use mini::Mini;

fn main() {
    let args: Vec<String> = args().collect();

    if args.len() > 2 {
        eprintln!("Usage: mini [script]");
        process::exit(64);
    } else if args.len() == 2 {
        // Run a file
        let filename = &args[1];
        if let Err(err) = Mini::run_file(filename) {
            eprintln!("Error running file: {}", err);
            process::exit(1);
        }
    } else {
        // Start REPL
        if let Err(err) = Mini::run_prompt() {
            eprintln!("Error in REPL: {}", err);
            process::exit(1);
        }
    }
}