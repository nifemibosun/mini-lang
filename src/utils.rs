use std::fmt;

/// Utility for printing errors
pub fn print_error(message: &str) {
    eprintln!("Error: {}", message);
}

/// Utility for checking if a string is a number
pub fn is_number(s: &str) -> bool {
    s.chars().all(|c| c.is_numeric())
}

/// Utility to trim whitespace
pub fn trim_whitespace(s: &str) -> String {
    s.trim().to_string()
}

/// Utility to convert strings to lowercase
pub fn to_lowercase(s: &str) -> String {
    s.to_lowercase()
}

/// Debugging utility
pub fn debug_log<T: fmt::Debug>(item: T) {
    println!("{:?}", item);
}
