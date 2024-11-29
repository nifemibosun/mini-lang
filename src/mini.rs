use crate::scanner::Scanner;
use crate::parser::Parser;
use crate::ast::{Expr, Statement, Value};

use std::fs;
use std::io::{BufRead, Write, stdin, stdout, Result, Error, ErrorKind};
use std::path::Path;
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub struct Mini {
    has_error: bool,
    environment: HashMap<String, Value>,
}

impl Mini {
    pub fn new() -> Self {
        Mini { 
            has_error: false,
            environment: HashMap::new(),
        }
    }

    /// Run a file with the `.mini` extension.
    pub fn run_file(path: &str) -> Result<()> {
        // Check if the file ends with the '.mini' extension
        if !path.ends_with(".mini") {
            return Err(Error::new(ErrorKind::InvalidInput, "Error: Only files with '.mini' extension are supported."));
        }
    
        // Read the content of the file
        let bytes = fs::read(Path::new(path))?;
        let content = String::from_utf8(bytes).map_err(|_| {
            Error::new(ErrorKind::InvalidData, "Failed to parse bytes into a valid UTF-8 string")
        })?;
    
        // Create a new instance of the interpreter
        let mut mini = Mini::new();
        mini.run(content);
    
        // Check if there were errors during execution
        if mini.has_error {
            return Err(Error::new(ErrorKind::Other, "Execution finished with errors."));
        }
    
        Ok(())
    }

    /// Start an interactive REPL prompt.
    pub fn run_prompt() -> Result<()> {
        let stdin = stdin();
        let mut reader = stdin.lock();
        let mut mini = Mini::new();
    
        loop {
            print!("> ");
            stdout().flush()?;
    
            let mut line = String::new();
            let bytes_read = reader.read_line(&mut line)?;
            if bytes_read == 0 {
                break; // Exit on EOF
            }
    
            let trimmed_line = line.trim();
            if !trimmed_line.is_empty() {
                mini.run(trimmed_line.to_string());
            }
            mini.has_error = false; // Reset errors for the next input
        }
        Ok(())
    }

    /// Core method to execute a source code string.
    fn run(&mut self, source: String) {
        let mut scanner = Scanner::new(source); // Generate tokens
        let tokens = scanner.scan_tokens();
    
        let mut parser = Parser::new(&tokens); // Initialize parser with tokens
        match parser.parse() { // Assume you add a `parse` method to parse the entire input
            Ok(statements) => {
                for statement in statements {
                    let result = self.execute_statement(statement);
                    if let Value::Return(_) = result {
                        // If there's a return value, exit early
                        return;
                    }
                }
            }
            Err(e) => {
                eprintln!("Parser error: {}", e);
            }
        }
    }

    pub fn execute_statement(&mut self, stmt: Statement) -> Value {
        match stmt {
            Statement::VariableDeclaration(name, value_expr) => {
                let value = self.evaluate_expr(value_expr);
                self.environment.insert(name, value);
                Value::Nil // Return nothing on variable declaration
            }
            Statement::IfStatement(condition, if_body, else_body) => {
                let condition_value = self.evaluate_expr(condition);
                if condition_value.is_truthy() {
                    for stmt in if_body {
                        let return_value = self.execute_statement(stmt);
                        if let Value::Return(_) = return_value {
                            return return_value;
                        }
                    }
                } else {
                    for stmt in else_body {
                        let return_value = self.execute_statement(stmt);
                        if let Value::Return(_) = return_value {
                            return return_value;
                        }
                    }
                }
                Value::Nil
            }
            Statement::Return(expr) => {
                let return_value = self.evaluate_expr(expr);
                Value::Return(Box::new(return_value)) // Return the evaluated value
            }
            Statement::Expression(expr) => {
                self.evaluate_expr(expr);
                Value::Nil // Return nothing for an expression statement
            }
            _ => {
                // Handle unimplemented or unknown statements gracefully
                self.report_error("Unhandled statement type encountered.");
                Value::Nil
            }
        }
    }    

    pub fn evaluate_expr(&mut self, expr: Expr) -> Value {
        match expr {
            Expr::Number(n) => Value::Number(n),
            Expr::String(s) => Value::String(s),
            Expr::Variable(name) => {
                self.environment.get(&name).cloned().unwrap_or(Value::Nil)
            }
            Expr::FunctionCall(name, args) => {
                // Look up the function in the environment
                if let Some(Value::Function(_, params, body)) = self.environment.get(&name) {
                    // Ensure the number of arguments matches the number of parameters
                    if args.len() != params.len() {
                        self.report_error(&format!("Function '{}' called with incorrect number of arguments.", name));
                        return Value::Nil;
                    }
    
                    // Evaluate each argument and bind it to the corresponding parameter
                    let mut local_env = self.environment.clone(); // Make a copy to isolate function calls
                    for (param, arg) in params.iter().zip(args.iter()) {
                        let arg_value = self.clone().evaluate_expr(arg.clone());
                        local_env.insert(param.clone(), arg_value);
                    }
    
                    // Execute the function's body
                    let mut return_value = Value::Nil;
                    for stmt in body {
                        return_value = self.clone().execute_statement(stmt.clone());
                        // If the statement is a return, break early and return the value
                        if let Value::Return(_) = return_value {
                            return return_value;
                        }
                    }
                    return return_value;
                } else {
                    self.report_error(&format!("Function '{}' is not defined.", name));
                    return Value::Nil;
                }
            }
            _ => {
                self.report_error("Unhandled expression type.");
                Value::Nil
            }
        }
    }

    /// Report an error with a specific line and message.
    pub fn error(line: usize, message: &str) {
        Mini::report(line, "", message);
    }

    /// Format and display error messages.
    fn report_error(&self, message: &str) {
        eprintln!("Error: {}", message);
    }

    /// Report a generic error message
    fn report(line: usize, position: &str, message: &str) {
        eprintln!("[Line {}] Error {}: {}", line, position, message);
    }
}
