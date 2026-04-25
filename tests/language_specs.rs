use poo::ast::{Expr, Stmt};
use poo::errors::LangErrorKind;
use poo::interpreter::Value;
use poo::lexer::{Lexer, Token};
use poo::parser::Parser;
use poo::{run_file_checked, run_source, run_source_checked};
use std::fs;

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
fn spec_interpreter_executes_program_to_return_value() {
    let result = run_source(
        r#"
        poof add(a int, b int) >> int {
            return a + b;
        }
        return add(3, 4);
        "#
        .to_string(),
    );

    assert_eq!(result, Some(Value::Int(7)));
}

#[test]
fn spec_checked_api_returns_typed_error_on_parse_failure() {
    let result = run_source_checked("poo x <: 1".to_string());
    let error = result.expect_err("expected parse error");
    assert_eq!(error.kind, LangErrorKind::Parse);
    assert!(!error.message.is_empty());
}

#[test]
fn spec_checked_api_returns_typed_error_on_runtime_failure() {
    let result = run_source_checked("return unknown_identifier;".to_string());
    let error = result.expect_err("expected runtime error");
    assert_eq!(error.kind, LangErrorKind::Runtime);
    assert!(error.message.contains("Undefined variable"));
}

#[test]
fn spec_checked_file_api_returns_io_error_for_missing_file() {
    let file_path = format!(
        "/tmp/poolang-missing-{}-{}.poo",
        std::process::id(),
        std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .expect("system clock before unix epoch")
            .as_nanos()
    );

    let result = run_file_checked(&file_path);
    let error = result.expect_err("expected io error");
    assert_eq!(error.kind, LangErrorKind::Io);
}

#[test]
fn spec_checked_file_api_executes_valid_file() {
    let file_path = format!(
        "/tmp/poolang-valid-{}-{}.poo",
        std::process::id(),
        std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .expect("system clock before unix epoch")
            .as_nanos()
    );

    fs::write(
        &file_path,
        r#"
        poof add(a int, b int) >> int {
            return a + b;
        }
        return add(4, 5);
        "#,
    )
    .expect("failed to write temp source");

    let result = run_file_checked(&file_path);
    let _ = fs::remove_file(&file_path);

    assert_eq!(
        result.expect("expected successful file run"),
        Some(Value::Int(9))
    );
}

#[test]
fn spec_for_range_loop_accumulates_values() {
    let result = run_source_checked(
        r#"
        mut total <: 0;
        for i in 0..5 {
            total = total + i;
        }
        return total;
        "#
        .to_string(),
    );

    assert_eq!(
        result.expect("expected successful execution"),
        Some(Value::Int(10))
    );
}

#[test]
fn spec_for_range_loop_honors_step_value() {
    let result = run_source_checked(
        r#"
        mut total <: 0;
        for i in 0..10 step 2 {
            total = total + i;
        }
        return total;
        "#
        .to_string(),
    );

    assert_eq!(
        result.expect("expected successful execution"),
        Some(Value::Int(20))
    );
}
