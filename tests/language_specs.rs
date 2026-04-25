use poo::ast::{Expr, Stmt};
use poo::errors::{LangError, LangErrorKind, panic_payload_to_message};
use poo::interpreter::Value;
use poo::lexer::{Lexer, Token};
use poo::parser::Parser;
use poo::{run_file, run_file_checked, run_source, run_source_checked};
use std::fs;

fn unique_temp_file_path(label: &str) -> String {
    format!(
        "/tmp/poolang-{}-{}-{}.poo",
        label,
        std::process::id(),
        std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .expect("system clock before unix epoch")
            .as_nanos()
    )
}

fn run_checked_with_temp_file_path(
    label: &str,
    source: &str,
) -> (String, Result<Option<Value>, LangError>) {
    let file_path = unique_temp_file_path(label);
    fs::write(&file_path, source).expect("failed to write temp source");
    let result = run_file_checked(&file_path);
    let _ = fs::remove_file(&file_path);
    (file_path, result)
}

fn run_checked_with_temp_file(label: &str, source: &str) -> Result<Option<Value>, LangError> {
    let (_, result) = run_checked_with_temp_file_path(label, source);
    result
}

fn unchecked_file_panic_message(file_path: &str) -> String {
    let payload =
        std::panic::catch_unwind(|| run_file(file_path)).expect_err("expected panic payload");
    panic_payload_to_message(payload)
}

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
fn spec_unchecked_api_panics_on_parse_failure() {
    let result = std::panic::catch_unwind(|| run_source("poo x <: 1".to_string()));
    assert!(result.is_err());
}

#[test]
fn spec_unchecked_api_panics_on_runtime_failure() {
    let result = std::panic::catch_unwind(|| run_source("return unknown_identifier;".to_string()));
    assert!(result.is_err());
}

#[test]
fn spec_unchecked_file_api_panics_on_parse_failure() {
    let file_path = unique_temp_file_path("unchecked-parse");
    fs::write(&file_path, "poo x <: 1").expect("failed to write temp source");

    let result = std::panic::catch_unwind(|| run_file(&file_path));
    let _ = fs::remove_file(&file_path);

    assert!(result.is_err());
}

#[test]
fn spec_unchecked_file_api_panics_on_runtime_failure() {
    let file_path = unique_temp_file_path("unchecked-runtime");
    fs::write(&file_path, "return unknown_identifier;").expect("failed to write temp source");

    let result = std::panic::catch_unwind(|| run_file(&file_path));
    let _ = fs::remove_file(&file_path);

    assert!(result.is_err());
}

#[test]
fn spec_unchecked_file_api_panics_on_missing_file() {
    let file_path = unique_temp_file_path("unchecked-missing");
    let result = std::panic::catch_unwind(|| run_file(&file_path));
    assert!(result.is_err());
}

#[test]
fn spec_unchecked_file_api_missing_file_panic_message_includes_file_path() {
    let file_path = unique_temp_file_path("unchecked-missing-msg");
    let message = unchecked_file_panic_message(&file_path);

    assert!(message.contains(&file_path));
}

#[test]
fn spec_unchecked_file_api_parse_panic_message_includes_file_path() {
    let file_path = unique_temp_file_path("unchecked-parse-msg");
    fs::write(&file_path, "poo x <: 1").expect("failed to write temp source");
    let message = unchecked_file_panic_message(&file_path);
    let _ = fs::remove_file(&file_path);

    assert!(message.contains(&file_path));
}

#[test]
fn spec_checked_api_reports_parse_error_for_missing_semicolon() {
    let result = run_source_checked("poo x <: 1 return x;".to_string());
    let error = result.expect_err("expected parse error");
    assert_eq!(error.kind, LangErrorKind::Parse);
    assert!(error.message.contains("Unexpected token"));
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
    let file_path = unique_temp_file_path("missing");

    let result = run_file_checked(&file_path);
    let error = result.expect_err("expected io error");
    assert_eq!(error.kind, LangErrorKind::Io);
    assert!(error.message.contains(&file_path));
}

#[test]
fn spec_checked_file_api_executes_valid_file() {
    let result = run_checked_with_temp_file(
        "valid",
        r#"
        poof add(a int, b int) >> int {
            return a + b;
        }
        return add(4, 5);
        "#,
    );

    assert_eq!(
        result.expect("expected successful file run"),
        Some(Value::Int(9))
    );
}

#[test]
fn spec_unchecked_file_api_executes_valid_file() {
    let file_path = unique_temp_file_path("unchecked-valid");
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

    let result = run_file(&file_path);
    let _ = fs::remove_file(&file_path);

    assert_eq!(result, Some(Value::Int(9)));
}

#[test]
fn spec_checked_file_api_returns_parse_error_for_invalid_file_content() {
    let result = run_checked_with_temp_file("invalid", "poo x <: 1");

    let error = result.expect_err("expected parse error");
    assert_eq!(error.kind, LangErrorKind::Parse);
    assert!(!error.message.is_empty());
}

#[test]
fn spec_checked_file_api_includes_file_path_in_parse_errors() {
    let (file_path, result) = run_checked_with_temp_file_path("parse-path", "poo x <: 1");

    let error = result.expect_err("expected parse error");
    assert_eq!(error.kind, LangErrorKind::Parse);
    assert!(error.message.contains(&file_path));
}

#[test]
fn spec_checked_file_api_returns_runtime_error_for_invalid_runtime_content() {
    let result = run_checked_with_temp_file("runtime", "return unknown_identifier;");

    let error = result.expect_err("expected runtime error");
    assert_eq!(error.kind, LangErrorKind::Runtime);
    assert!(error.message.contains("Undefined variable"));
}

#[test]
fn spec_checked_file_api_includes_file_path_in_runtime_errors() {
    let (file_path, result) =
        run_checked_with_temp_file_path("runtime-path", "return unknown_identifier;");

    let error = result.expect_err("expected runtime error");
    assert_eq!(error.kind, LangErrorKind::Runtime);
    assert!(error.message.contains(&file_path));
}

#[test]
fn spec_checked_file_api_includes_file_path_for_function_return_type_mismatch() {
    let (file_path, result) = run_checked_with_temp_file_path(
        "return-type-path",
        r#"
        poof bad() >> int {
            return "oops";
        }
        return bad();
        "#,
    );

    let error = result.expect_err("expected parse error");
    assert_eq!(error.kind, LangErrorKind::Parse);
    assert!(error.message.contains("Return type mismatch"));
    assert!(error.message.contains(&file_path));
}

#[test]
fn spec_checked_file_api_returns_parse_error_for_function_return_type_mismatch() {
    let result = run_checked_with_temp_file(
        "return-type",
        r#"
        poof bad() >> int {
            return "oops";
        }
        return bad();
        "#,
    );

    let error = result.expect_err("expected parse error");
    assert_eq!(error.kind, LangErrorKind::Parse);
    assert!(error.message.contains("Return type mismatch"));
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

#[test]
fn spec_checked_api_reports_runtime_error_for_zero_for_step() {
    let result = run_source_checked(
        r#"
        mut total <: 0;
        for i in 0..10 step 0 {
            total = total + i;
        }
        return total;
        "#
        .to_string(),
    );

    let error = result.expect_err("expected runtime error");
    assert_eq!(error.kind, LangErrorKind::Runtime);
    assert!(
        error
            .message
            .contains("For loop step must be a positive integer")
    );
}

#[test]
fn spec_checked_api_reports_runtime_error_for_non_integer_for_step() {
    let result = run_source_checked(
        r#"
        mut total <: 0;
        for i in 0..10 step 1.5 {
            total = total + i;
        }
        return total;
        "#
        .to_string(),
    );

    let error = result.expect_err("expected runtime error");
    assert_eq!(error.kind, LangErrorKind::Runtime);
    assert!(error.message.contains("For loop step must be type Int"));
}

#[test]
fn spec_checked_api_reports_runtime_error_for_negative_for_step() {
    let result = run_source_checked(
        r#"
        mut total <: 0;
        for i in 0..10 step -2 {
            total = total + i;
        }
        return total;
        "#
        .to_string(),
    );

    let error = result.expect_err("expected runtime error");
    assert_eq!(error.kind, LangErrorKind::Runtime);
    assert!(
        error
            .message
            .contains("For loop step must be a positive integer")
    );
}

#[test]
fn spec_checked_api_reports_runtime_error_for_non_integer_for_start() {
    let result = run_source_checked(
        r#"
        mut total <: 0;
        for i in 0.5..10 {
            total = total + i;
        }
        return total;
        "#
        .to_string(),
    );

    let error = result.expect_err("expected runtime error");
    assert_eq!(error.kind, LangErrorKind::Runtime);
    assert!(error.message.contains("For loop start must be type Int"));
}

#[test]
fn spec_checked_api_reports_runtime_error_for_non_integer_for_end() {
    let result = run_source_checked(
        r#"
        mut total <: 0;
        for i in 0..10.5 {
            total = total + i;
        }
        return total;
        "#
        .to_string(),
    );

    let error = result.expect_err("expected runtime error");
    assert_eq!(error.kind, LangErrorKind::Runtime);
    assert!(error.message.contains("For loop end must be type Int"));
}

#[test]
fn spec_checked_api_reports_runtime_error_when_reassigning_for_iterator() {
    let result = run_source_checked(
        r#"
        for i in 0..3 {
            i = i + 1;
        }
        return 0;
        "#
        .to_string(),
    );

    let error = result.expect_err("expected runtime error");
    assert_eq!(error.kind, LangErrorKind::Runtime);
    assert!(error.message.contains("Variable 'i' is not mutable."));
}

#[test]
fn spec_checked_api_reports_runtime_error_when_reassigning_vector_for_iterator() {
    let result = run_source_checked(
        r#"
        for item in [1, 2, 3] {
            item = item + 1;
        }
        return 0;
        "#
        .to_string(),
    );

    let error = result.expect_err("expected runtime error");
    assert_eq!(error.kind, LangErrorKind::Runtime);
    assert!(error.message.contains("Variable 'item' is not mutable."));
}

#[test]
fn spec_for_vector_loop_accumulates_values() {
    let result = run_source_checked(
        r#"
        mut total <: 0;
        for i in [1, 2, 3, 4] {
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
fn spec_if_elif_else_executes_first_truthy_branch() {
    let result = run_source_checked(
        r#"
        mut value <: 1;
        if false {
            value = 2;
        } elif true {
            value = 3;
        } else {
            value = 4;
        }
        return value;
        "#
        .to_string(),
    );

    assert_eq!(
        result.expect("expected successful execution"),
        Some(Value::Int(3))
    );
}

#[test]
fn spec_checked_api_reports_runtime_error_for_non_boolean_if_condition() {
    let result = run_source_checked(
        r#"
        if "poolang" {
            return 1;
        }
        return 0;
        "#
        .to_string(),
    );

    let error = result.expect_err("expected runtime error");
    assert_eq!(error.kind, LangErrorKind::Runtime);
    assert!(
        error
            .message
            .contains("Condition in if statement must be a boolean")
    );
}

#[test]
fn spec_checked_api_reports_runtime_error_for_non_boolean_elif_condition() {
    let result = run_source_checked(
        r#"
        if false {
            return 0;
        } elif 1 {
            return 1;
        } else {
            return 2;
        }
        "#
        .to_string(),
    );

    let error = result.expect_err("expected runtime error");
    assert_eq!(error.kind, LangErrorKind::Runtime);
    assert!(
        error
            .message
            .contains("Condition in if statement must be a boolean")
    );
}

#[test]
fn spec_while_loop_accumulates_until_condition_fails() {
    let result = run_source_checked(
        r#"
        mut i <: 0;
        mut total <: 0;
        while i < 4 {
            total = total + i;
            i = i + 1;
        }
        return total;
        "#
        .to_string(),
    );

    assert_eq!(
        result.expect("expected successful execution"),
        Some(Value::Int(6))
    );
}

#[test]
fn spec_checked_api_reports_runtime_error_for_non_boolean_while_condition() {
    let result = run_source_checked(
        r#"
        while 1 {
            return 1;
        }
        return 0;
        "#
        .to_string(),
    );

    let error = result.expect_err("expected runtime error");
    assert_eq!(error.kind, LangErrorKind::Runtime);
    assert!(
        error
            .message
            .contains("Condition in while statement must be a boolean")
    );
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

#[test]
fn spec_struct_instance_method_can_access_self_properties() {
    let result = run_source_checked(
        r#"
        struct Person {
            name str

            impl {
                poof get_name() >> str {
                    return self.name;
                }
            }
        }

        poo person <: Person::{
            name: "Shayy"
        };

        return person.get_name();
        "#
        .to_string(),
    );

    assert_eq!(
        result.expect("expected successful execution"),
        Some(Value::String("Shayy".to_string()))
    );
}

#[test]
fn spec_struct_inheritance_exposes_parent_methods_on_child_instance() {
    let result = run_source_checked(
        r#"
        struct Person {
            name str

            impl {
                poof get_name() >> str {
                    return self.name;
                }
            }
        }

        struct Student inherits Person {
            student_id int
        }

        poo student <: Student::{
            name: "Ada",
            student_id: 7
        };

        return student.get_name();
        "#
        .to_string(),
    );

    assert_eq!(
        result.expect("expected successful execution"),
        Some(Value::String("Ada".to_string()))
    );
}

#[test]
fn spec_function_return_inside_loop_preserves_outer_scope() {
    let result = run_source_checked(
        r#"
        poof pick() >> int {
            while true {
                return 1;
            }
            return 0;
        }

        poo x <: pick();
        poo y <: 2;
        return x + y;
        "#
        .to_string(),
    );

    assert_eq!(
        result.expect("expected successful execution"),
        Some(Value::Int(3))
    );
}

#[test]
fn spec_for_loop_iterator_is_scoped_to_loop_body() {
    let result = run_source_checked(
        r#"
        for i in 0..3 {
            poo inside <: i;
        }
        return i;
        "#
        .to_string(),
    );

    let error = result.expect_err("expected runtime error");
    assert_eq!(error.kind, LangErrorKind::Runtime);
    assert!(error.message.contains("Undefined variable: i"));
}

#[test]
fn spec_while_body_variable_is_scoped_to_loop() {
    let result = run_source_checked(
        r#"
        mut i <: 0;
        while i < 1 {
            poo inner <: 42;
            i = i + 1;
        }
        return inner;
        "#
        .to_string(),
    );

    let error = result.expect_err("expected runtime error");
    assert_eq!(error.kind, LangErrorKind::Runtime);
    assert!(error.message.contains("Undefined variable: inner"));
}

#[test]
fn spec_if_body_variable_is_scoped_to_block() {
    let result = run_source_checked(
        r#"
        if true {
            poo only_if <: 10;
        } else {
            poo only_else <: 20;
        }
        return only_if;
        "#
        .to_string(),
    );

    let error = result.expect_err("expected runtime error");
    assert_eq!(error.kind, LangErrorKind::Runtime);
    assert!(error.message.contains("Undefined variable: only_if"));
}

#[test]
fn spec_for_iterator_shadowing_does_not_overwrite_outer_variable() {
    let result = run_source_checked(
        r#"
        mut i <: 100;
        for i in 0..3 {
            poo seen <: i;
        }
        return i;
        "#
        .to_string(),
    );

    assert_eq!(
        result.expect("expected successful execution"),
        Some(Value::Int(100))
    );
}

#[test]
fn spec_elif_body_variable_is_scoped_to_block() {
    let result = run_source_checked(
        r#"
        if false {
            poo only_if <: 10;
        } elif true {
            poo only_elif <: 20;
        } else {
            poo only_else <: 30;
        }
        return only_elif;
        "#
        .to_string(),
    );

    let error = result.expect_err("expected runtime error");
    assert_eq!(error.kind, LangErrorKind::Runtime);
    assert!(error.message.contains("Undefined variable: only_elif"));
}

#[test]
fn spec_for_vector_iterator_is_scoped_to_loop_body() {
    let result = run_source_checked(
        r#"
        for item in [1, 2, 3] {
            poo seen <: item;
        }
        return item;
        "#
        .to_string(),
    );

    let error = result.expect_err("expected runtime error");
    assert_eq!(error.kind, LangErrorKind::Runtime);
    assert!(error.message.contains("Undefined variable: item"));
}
