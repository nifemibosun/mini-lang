#[cfg(test)]
mod tests {
    use super::*;
    use crate::scanner::Scanner;
    use crate::scanner::token::{ Token, TokenType };
    
    fn create_mini_state()-> MiniState {
        MiniState::new()
    }
    
    #[test]
    fn test_empty_source() {
        let source = "";
        let mut state = create_mini_state();
        let mut scanner = Scanner::new(source, &mut state);
        let tokens = scanner.scan_tokens();
        
        assert_eq!(tokens.len(), 1);
        assert_eq!(tokens[0].token_type, TokenType::EOF);
    }
    
    #[test]
    fn test_single_character_tokens() {
        let source = "(){},.-+;*/:";
        let mut state = create_mini_state();
        let mut scanner = Scanner::new(source, &mut state);
        let tokens = scanner.scan_tokens();
        
        let expected_types = vec![
            TokenType::LParen, TokenType::RParen, 
            TokenType::LBrace, TokenType::RBrace, 
            TokenType::Comma, TokenType::Dot, 
            TokenType::Minus, TokenType::Plus, 
            TokenType::SemiColon, TokenType::Star, 
            TokenType::Slash, TokenType::Colon,
            TokenType::EOF
        ];
        
        assert_eq!(tokens.len(), expected_types.len());
        
        for (i, token_type) in expected_types.iter().enumerate() {
            assert_eq!(tokens[i].token_type, *token_type);
        }
    }
    
    #[test]
    fn test_double_character_tokens() {
        let source = "== != <= >= :: += -= *= /=";
        let mut state = create_mini_state();
        let mut scanner = Scanner::new(source, &mut state);
        let tokens = scanner.scan_tokens();
        
        let expected_types = vec![
            TokenType::EqualEqual, TokenType::BangEqual, 
            TokenType::LessEqual, TokenType::GreaterEqual, 
            TokenType::ColonColon, TokenType::PlusEqual, 
            TokenType::MinusEqual, TokenType::StarEqual, 
            TokenType::SlashEqual, TokenType::EOF
        ];
        
        assert_eq!(tokens.len(), expected_types.len());
        
        for (i, token_type) in expected_types.iter().enumerate() {
            assert_eq!(tokens[i].token_type, *token_type);
        }
    }
    
    #[test]
    fn test_logical_operators() {
        let source = "|| && ! < >";
        let mut state = create_mini_state();
        let mut scanner = Scanner::new(source, &mut state);
        let tokens = scanner.scan_tokens();
        
        let expected_types = vec![
            TokenType::Or, TokenType::And, 
            TokenType::Bang, TokenType::LessThan, 
            TokenType::GreaterThan, TokenType::EOF
        ];
        
        assert_eq!(tokens.len(), expected_types.len());
        
        for (i, token_type) in expected_types.iter().enumerate() {
            assert_eq!(tokens[i].token_type, *token_type);
        }
    }
    
    #[test]
    fn test_string_literals() {
        let source = r#""hello world" "test""#;
        let mut state = create_mini_state();
        let mut scanner = Scanner::new(source, &mut state);
        let tokens = scanner.scan_tokens();
        
        assert_eq!(tokens.len(), 3);
        assert_eq!(tokens[0].token_type, TokenType::String);
        assert_eq!(tokens[0].literal.as_ref().unwrap(), "hello world");
        assert_eq!(tokens[1].token_type, TokenType::String);
        assert_eq!(tokens[1].literal.as_ref().unwrap(), "test");
    }
    
    #[test]
    fn test_number_literals() {
        let source = "123 45.67";
        let mut state = create_mini_state();
        let mut scanner = Scanner::new(source, &mut state);
        let tokens = scanner.scan_tokens();
        
        assert_eq!(tokens.len(), 3);
        assert_eq!(tokens[0].token_type, TokenType::Number);
        assert_eq!(tokens[0].literal.as_ref().unwrap(), "123");
        assert_eq!(tokens[1].token_type, TokenType::Number);
        assert_eq!(tokens[1].literal.as_ref().unwrap(), "45.67");
    }
    
    #[test]
    fn test_keywords() {
        let source = "if else while func return let const mut true false";
        let mut state = create_mini_state();
        let mut scanner = Scanner::new(source, &mut state);
        let tokens = scanner.scan_tokens();
        
        let expected_types = vec![
            TokenType::If, TokenType::Else, 
            TokenType::While, TokenType::Func, 
            TokenType::Return, TokenType::Let, 
            TokenType::Const, TokenType::Mut, 
            TokenType::True, TokenType::False,
            TokenType::EOF
        ];
        
        assert_eq!(tokens.len(), expected_types.len());
        
        for (i, token_type) in expected_types.iter().enumerate() {
            assert_eq!(tokens[i].token_type, *token_type);
        }
    }
    
    #[test]
    fn test_type_keywords() {
        let source = "bool int32 float64 uint8";
        let mut state = create_mini_state();
        let mut scanner = Scanner::new(source, &mut state);
        let tokens = scanner.scan_tokens();
        
        let expected_types = vec![
            TokenType::Bool, TokenType::Int32, 
            TokenType::Float64, TokenType::UInt8,
            TokenType::EOF
        ];
        
        assert_eq!(tokens.len(), expected_types.len());
        
        for (i, token_type) in expected_types.iter().enumerate() {
            assert_eq!(tokens[i].token_type, *token_type);
        }
    }
    
    #[test]
    fn test_identifiers() {
        let source = "x y123 _test identifier";
        let mut state = create_mini_state();
        let mut scanner = Scanner::new(source, &mut state);
        let tokens = scanner.scan_tokens();
        
        let expected_types = vec![
            TokenType::Identifier, TokenType::Identifier, 
            TokenType::Identifier, TokenType::Identifier,
            TokenType::EOF
        ];
        
        assert_eq!(tokens.len(), expected_types.len());
        
        for (i, token_type) in expected_types.iter().enumerate() {
            assert_eq!(tokens[i].token_type, *token_type);
        }
        
        assert_eq!(tokens[0].lexeme, "x");
        assert_eq!(tokens[1].lexeme, "y123");
        assert_eq!(tokens[2].lexeme, "_test");
        assert_eq!(tokens[3].lexeme, "identifier");
    }
    
    #[test]
    fn test_comments() {
        let source = "// This is a comment\nx // Another comment";
        let mut state = create_mini_state();
        let mut scanner = Scanner::new(source, &mut state);
        let tokens = scanner.scan_tokens();
        
        assert_eq!(tokens.len(), 2);
        assert_eq!(tokens[0].token_type, TokenType::Identifier);
        assert_eq!(tokens[0].lexeme, "x");
    }
    
    #[test]
    fn test_whitespace() {
        let source = "   \t\r\n  x  \n  y  ";
        let mut state = create_mini_state();
        let mut scanner = Scanner::new(source, &mut state);
        let tokens = scanner.scan_tokens();
        
        assert_eq!(tokens.len(), 3);
        assert_eq!(tokens[0].token_type, TokenType::Identifier);
        assert_eq!(tokens[0].lexeme, "x");
        assert_eq!(tokens[1].token_type, TokenType::Identifier);
        assert_eq!(tokens[1].lexeme, "y");
    }
    
    #[test]
    fn test_line_tracking() {
        let source = "x\ny\n\nz";
        let mut state = create_mini_state();
        let mut scanner = Scanner::new(source, &mut state);
        let tokens = scanner.scan_tokens();
        
        assert_eq!(tokens.len(), 4);
        assert_eq!(tokens[0].line, 1);
        assert_eq!(tokens[1].line, 2);
        assert_eq!(tokens[2].line, 4);
    }
    
    #[test]
    fn test_unterminated_string() {
        let source = r#""unterminated"#;
        let mut state = create_mini_state();
        let mut scanner = Scanner::new(source, &mut state);
        let tokens = scanner.scan_tokens();
        
        assert_eq!(tokens.len(), 1);
        assert!(state.had_error);
    }
    
    #[test]
    fn test_complex_program() {
        let source = r#"
        func fibonacci(n: int32): int32 {
            if n <= 1 {
                return n;
            }
            return fibonacci(n - 1) + fibonacci(n - 2);
        }
        
        let mut x = 10;
        println("Fibonacci sequence:");
        while x > 0 {
            println(fibonacci(x));
            x -= 1;
        }
        "#;
        
        let mut state = create_mini_state();
        let mut scanner = Scanner::new(source, &mut state);
        let tokens = scanner.scan_tokens();
        
        assert!(!state.had_error);
        assert!(tokens.len() > 30);
        
        let func_index = tokens.iter().position(|t| t.token_type == TokenType::Func).unwrap();
        let fibonacci_index = func_index + 1;
        assert_eq!(tokens[fibonacci_index].token_type, TokenType::Identifier);
        assert_eq!(tokens[fibonacci_index].lexeme, "fibonacci");
    }
}