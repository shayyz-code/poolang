//! Lexer specs: tokens, literals, comments, and lexical diagnostics.

use crate::support::assert_parse_error;
use poo::lexer::{Lexer, Token};

#[test]
fn spec_lexer_skips_inline_comment_block() {
    let mut lexer = Lexer::new("poo x <: 1; // comment // poo y <: 2;".to_string());

    let mut tokens = Vec::new();
    loop {
        let token = lexer.next_token();
        tokens.push(token.clone());
        if token == Token::EOF {
            break;
        }
    }

    assert_eq!(
        tokens,
        vec![
            Token::Poo,
            Token::Identifier("x".to_string()),
            Token::ShortAssignment,
            Token::Int(1),
            Token::SemiColon,
            Token::Poo,
            Token::Identifier("y".to_string()),
            Token::ShortAssignment,
            Token::Int(2),
            Token::SemiColon,
            Token::EOF,
        ]
    );
}

#[test]
fn spec_lexer_reads_utf8_string_literal() {
    let mut lexer = Lexer::new(r#""မင်္ဂလာပါ""#.to_string());

    assert_eq!(lexer.next_token(), Token::String("မင်္ဂလာပါ".to_string()));
    assert_eq!(lexer.next_token(), Token::EOF);
}

#[test]
fn spec_lexer_reads_utf8_character_literal() {
    let mut lexer = Lexer::new("'ပ'".to_string());

    assert_eq!(lexer.next_token(), Token::Char('ပ'));
    assert_eq!(lexer.next_token(), Token::EOF);
}

#[test]
fn spec_lexer_skips_utf8_comment_containing_single_slash() {
    let mut lexer = Lexer::new("poo x <: 1; // မှတ်ချက် / slash ပါသည် // return x;".to_string());

    let mut tokens = Vec::new();
    loop {
        let token = lexer.next_token();
        tokens.push(token.clone());
        if token == Token::EOF {
            break;
        }
    }

    assert_eq!(
        tokens,
        vec![
            Token::Poo,
            Token::Identifier("x".to_string()),
            Token::ShortAssignment,
            Token::Int(1),
            Token::SemiColon,
            Token::Return,
            Token::Identifier("x".to_string()),
            Token::SemiColon,
            Token::EOF,
        ]
    );
}

#[test]
fn spec_checked_api_reports_parse_error_for_unterminated_string() {
    assert_parse_error(r#"return "unfinished;"#, "Unterminated string literal");
}

#[test]
fn spec_checked_api_reports_parse_error_for_unknown_string_escape() {
    assert_parse_error(r#"return "\q";"#, "Unknown escape sequence: \\q");
}

#[test]
fn spec_checked_api_reports_parse_error_for_unterminated_character_literal() {
    assert_parse_error("return 'ပ;", "Unterminated character literal");
}

#[test]
fn spec_checked_api_reports_parse_error_for_unterminated_comment() {
    assert_parse_error(
        "poo x <: 1; // comment never closes",
        "Unterminated comment",
    );
}

#[test]
fn spec_checked_api_rejects_non_ascii_identifier() {
    assert_parse_error("poo နာမည် <: 1;", "Unexpected character: န");
}
