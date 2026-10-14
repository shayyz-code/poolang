//! Embedding API specs: checked and unchecked source and file entrypoints.

use crate::support::{
    run_checked_with_temp_file, run_checked_with_temp_file_path, run_unchecked_with_temp_file_path,
    unchecked_file_panic_message, unchecked_file_panic_message_with_temp_file,
    unchecked_file_panics_with_temp_file, unique_temp_file_path,
};
use poo::errors::LangErrorKind;
use poo::interpreter::Value;
use poo::{run_file, run_file_checked, run_source, run_source_checked};

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
    assert!(unchecked_file_panics_with_temp_file(
        "unchecked-parse",
        "poo x <: 1",
    ));
}

#[test]
fn spec_unchecked_file_api_panics_on_runtime_failure() {
    assert!(unchecked_file_panics_with_temp_file(
        "unchecked-runtime",
        "return unknown_identifier;",
    ));
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
    let (file_path, message) =
        unchecked_file_panic_message_with_temp_file("unchecked-parse-msg", "poo x <: 1");

    assert!(message.contains(&file_path));
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
    let (_, result) = run_unchecked_with_temp_file_path(
        "unchecked-valid",
        r#"
        poof add(a int, b int) >> int {
            return a + b;
        }
        return add(4, 5);
        "#,
    );

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
