#[cfg(test)]
#[allow(unused)]
mod tests {
    use super::*;
    use mini::tokens::TokenType;
    use mini::scanner::Scanner;

    #[test]
    fn test_basic_tokens() {
        let source = "let x = 5;";
        let mut scanner = Scanner::new(source.to_string());
        let tokens = scanner.scan_tokens();

        let expected_types = vec![
            TokenType::Let,
            TokenType::Identifier,
            TokenType::Equal,
            TokenType::Number,
            TokenType::SemiColon,
            TokenType::EOF,
        ];

        let actual_types: Vec<TokenType> = tokens.iter().map(|t| t.token_type.clone()).collect();
        assert_eq!(actual_types, expected_types);
    }

    #[test]
    fn test_string_literal() {
        let source = "let name = \"Bosun\";";
        let mut scanner = Scanner::new(source.to_string());
        let tokens = scanner.scan_tokens();

        assert_eq!(tokens[2].token_type, TokenType::Equal);
        assert_eq!(tokens[3].token_type, TokenType::String);
        assert_eq!(tokens[3].literal.as_ref().unwrap(), "Bosun");
    }

    #[test]
    fn test_keyword_detection() {
        let source = "if true { return false; }";
        let mut scanner = Scanner::new(source.to_string());
        let tokens = scanner.scan_tokens();

        let types = tokens.iter().map(|t| t.token_type.clone()).collect::<Vec<_>>();
        assert_eq!(
            types,
            vec![
                TokenType::If,
                TokenType::True,
                TokenType::LBrace,
                TokenType::Return,
                TokenType::False,
                TokenType::SemiColon,
                TokenType::RBrace,
                TokenType::EOF
            ]
        );
    }

    #[test]
    fn test_comment_skipping() {
        let source = "let x = 5; // this is a comment\nlet y = 10;";
        let mut scanner = Scanner::new(source.to_string());
        let tokens = scanner.scan_tokens();

        let types = tokens.iter().map(|t| t.token_type.clone()).collect::<Vec<_>>();
        assert_eq!(
            types,
            vec![
                TokenType::Let,
                TokenType::Identifier,
                TokenType::Equal,
                TokenType::Number,
                TokenType::SemiColon,
                TokenType::Let,
                TokenType::Identifier,
                TokenType::Equal,
                TokenType::Number,
                TokenType::SemiColon,
                TokenType::EOF
            ]
        );
    }

    #[test]
    fn test_unterminated_string_error() {
        let source = "\"Hello";
        let mut scanner = Scanner::new(source.to_string());

        // Capture panic or error if your Mini::error throws one
        // This assumes it doesn't panic but logs error (you can enhance error handling if needed)
        scanner.scan_tokens();
        // There's no assertion here unless you track errors inside `Mini`.
    }

    #[test]
    fn test_example_control_flow_file() {
        use std::fs;

        let contents = fs::read_to_string("examples/control_flow.mini")
            .expect("Failed to read control_flow.mini");
        let mut scanner = Scanner::new(contents);
        let tokens = scanner.scan_tokens();

        assert!(tokens.len() > 0); // At least something was scanned
        assert_eq!(tokens.last().unwrap().token_type, TokenType::EOF);
    }

    #[test]
    fn test_multi_char_operators() {
        let source = "x == 5 != 3 <= 7 >= 2";
        let mut scanner = Scanner::new(source.to_string());
        let tokens = scanner.scan_tokens();

        let expected_types = vec![
            TokenType::Identifier,
            TokenType::EqualEqual,
            TokenType::Number,
            TokenType::BangEqual,
            TokenType::Number,
            TokenType::LessEqual,
            TokenType::Number,
            TokenType::GreaterEqual,
            TokenType::Number,
            TokenType::EOF
        ];

        let actual_type: Vec<TokenType> = tokens.iter().map(|t| t.token_type.clone()).collect();
        assert_eq!(actual_type, expected_types);
    }

    fn unterminated_str_mid_file() {
        let source = "let a = \"Hello;\nlet b = 2;";
        let mut scanner = Scanner::new(source.to_string());
        let tokens = scanner.scan_tokens();

        assert!(tokens.iter().any(|t| t.token_type == TokenType::EOF));
    }
}
