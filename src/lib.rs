pub mod scanner;
pub mod parser;

pub struct MiniState {
    pub had_error: bool,
}

impl MiniState {
    pub fn new() -> Self {
        MiniState { had_error: false }
    }
    
    pub fn error(&mut self, line: usize, message: &str) {
        self.report(line, "", message);
    }
    
    fn report(&mut self, line: usize, pos: &str, message: &str) {
        eprintln!("[line {}] Error{}: {}", line, pos, message);
        self.had_error = true;
    }
}