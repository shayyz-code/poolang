//! Parser specs: precedence, syntax diagnostics, and parse-time type checks.

use crate::support::assert_parse_error;
use poo::ast::{Expr, Stmt};
use poo::errors::LangErrorKind;
use poo::lexer::{Lexer, Token};
use poo::parser::Parser;
use poo::run_source_checked;

#[test]
fn spec_checked_api_reports_unexpected_eof_in_parenthesized_expression() {
    assert_parse_error("return (1 + 2", "Unexpected end of input");
}

#[test]
fn spec_checked_api_reports_unexpected_eof_before_closing_block() {
    assert_parse_error("if true { return 1;", "Unexpected end of input");
}

#[test]
fn spec_parser_respects_multiplication_precedence() {
    let lexer = Lexer::new("poo result <: 1 + 2 * 3;".to_string());
    let mut parser = Parser::new(lexer);
    let ast = parser.parse();

    assert_eq!(ast.len(), 1);

    let Stmt::Assignment(_, expr, _, _) = &ast[0] else {
        panic!("expected an assignment statement");
    };

    let Expr::BinaryOp(left, Token::Plus, right) = expr else {
        panic!("expected '+' at top-level expression");
    };

    assert_eq!(**left, Expr::Int(1));
    assert_eq!(
        **right,
        Expr::BinaryOp(
            Box::new(Expr::Int(2)),
            Token::Multiply,
            Box::new(Expr::Int(3))
        )
    );
}

#[test]
fn spec_checked_api_reports_parse_error_for_missing_semicolon() {
    let result = run_source_checked("poo x <: 1 return x;".to_string());
    let error = result.expect_err("expected parse error");
    assert_eq!(error.kind, LangErrorKind::Parse);
    assert!(error.message.contains("Unexpected token"));
}

#[test]
fn spec_checked_api_reports_parse_error_for_function_return_type_mismatch() {
    let result = run_source_checked(
        r#"
        poof bad() >> int {
            return "oops";
        }
        return bad();
        "#
        .to_string(),
    );

    let error = result.expect_err("expected parse type error");
    assert_eq!(error.kind, LangErrorKind::Parse);
    assert!(error.message.contains("Return type mismatch"));
}
