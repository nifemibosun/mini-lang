use std::collections::HashMap;
use std::env;
use std::fs;

// Represents a value in MiniLang
#[derive(Debug, Clone)]
#[allow(dead_code)]
enum Value {
    Int(i64),
    Float(f64),
    Bool(bool),
    String(String),
    Nil,
}

// Represents a function in MiniLang
#[derive(Debug, Clone)]
#[allow(dead_code)]
struct Function {
    name: String,
    return_type: String,
    params: Vec<String>,
    body: Vec<String>,
}

// Interpreter for MiniLang
struct Interpreter {
    variables: HashMap<String, Value>,
    constants: HashMap<String, Value>,
    functions: HashMap<String, Function>,
}

impl Interpreter {
    fn new() -> Self {
        Self {
            variables: HashMap::new(),
            constants: HashMap::new(),
            functions: HashMap::new(),
        }
    }

    fn interpret(&mut self, code: &str) {
        let lines: Vec<&str> = code.lines().collect();
        for line in lines.clone() {
            if line.trim().is_empty() || line.trim().starts_with("//") {
                continue;
            }

            if line.trim().starts_with("fun ") {
                self.parse_function(line, &lines);
            } else {
                self.execute(line);
            }
        }
    }

    fn parse_function(&mut self, header: &str, lines: &[&str]) {
        let header_parts: Vec<&str> = header.split_whitespace().collect();
        let name = header_parts[1].trim();
        let return_type = header_parts.last().unwrap().trim().replace("->", "");
        let mut body = Vec::new();
        let mut in_body = false;

        for line in lines {
            if line.trim() == "{" {
                in_body = true;
                continue;
            }
            if in_body && line.trim() == "}" {
                break;
            }
            if in_body {
                body.push(line.trim().to_string());
            }
        }

        self.functions.insert(
            name.to_string(),
            Function {
                name: name.to_string(),
                return_type,
                params: Vec::new(),
                body,
            },
        );
    }

    fn execute(&mut self, line: &str) {
        if line.starts_with("let ") || line.starts_with("const ") {
            self.parse_variable(line);
        } else if line.starts_with("print(") {
            self.execute_print(line);
        } else if line.ends_with("()") {
            self.call_function(line);
        } else {
            println!("Unknown statement: {}", line);
        }
    }

    fn parse_variable(&mut self, line: &str) {
        let parts: Vec<&str> = line.split('=').collect();
        let declaration = parts[0].trim();
        let value = parts[1].trim().replace(";", "");
        let (modifier, name_and_type) = declaration.split_at(3);
        let mut name_type = name_and_type.trim().split(':');
        let name = name_type.next().unwrap().trim();
        let var_type = name_type.next().unwrap().trim();

        let value = match var_type {
            "int" => Value::Int(value.parse().unwrap()),
            "float" => Value::Float(value.parse().unwrap()),
            "bool" => Value::Bool(value.parse::<bool>().unwrap()),
            "string" => Value::String(value.replace("\"", "")),
            _ => Value::Nil,
        };

        if modifier == "let" {
            self.variables.insert(name.to_string(), value);
        } else if modifier == "const" {
            self.constants.insert(name.to_string(), value);
        } else {
            println!("Invalid variable declaration: {}", line);
        }
    }

    fn execute_print(&self, line: &str) {
        let content = line
            .trim_start_matches("print(")
            .trim_end_matches(");")
            .trim();
        if let Some(value) = self.variables.get(content) {
            println!("{:?}", value);
        } else {
            println!("{}", content.replace("\"", ""));
        }
    }

    fn call_function(&self, line: &str) {
        let function_name = line.trim_end_matches("()");
        if let Some(function) = self.functions.get(function_name) {
            println!("Calling function: {}", function.name);
            for statement in &function.body {
                println!("Executing: {}", statement);
                // Simplified execution
            }
        } else {
            println!("Function '{}' not found", function_name);
        }
    }
}

fn main() {
    // Get command-line arguments
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        eprintln!("Usage: mini <filename.mini>");
        return;
    }

    let filename = &args[1];
    if !filename.ends_with(".mini") {
        eprintln!("Error: File must have a .mini extension.");
        return;
    }

    // Read the specified file
    let code = fs::read_to_string(filename).unwrap_or_else(|_| {
        eprintln!("Error: Could not read file '{}'.", filename);
        std::process::exit(1);
    });

    // Create and run the interpreter
    let mut interpreter = Interpreter::new();
    interpreter.interpret(&code);
}
