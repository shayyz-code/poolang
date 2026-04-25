use poo::ast::{Expr, Stmt};
use poo::interpreter::Value;
use poo::lexer::{Lexer, Token};
use poo::parser::Parser;
use poo::{run_source, run_source_checked};

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
    assert!(result.is_err());
}

#[test]
fn spec_checked_api_returns_typed_error_on_runtime_failure() {
    let result = run_source_checked("return unknown_identifier;".to_string());
    assert!(result.is_err());
}
