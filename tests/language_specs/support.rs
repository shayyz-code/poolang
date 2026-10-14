//! Shared helpers for the language spec modules.

use poo::errors::{LangError, LangErrorKind, panic_payload_to_message};
use poo::interpreter::Value;
use poo::{run_file, run_file_checked, run_source_checked};
use std::fs;

pub fn unique_temp_file_path(label: &str) -> String {
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

pub fn run_checked_with_temp_file_path(
    label: &str,
    source: &str,
) -> (String, Result<Option<Value>, LangError>) {
    let file_path = unique_temp_file_path(label);
    fs::write(&file_path, source).expect("failed to write temp source");
    let result = run_file_checked(&file_path);
    let _ = fs::remove_file(&file_path);
    (file_path, result)
}

pub fn run_checked_with_temp_file(label: &str, source: &str) -> Result<Option<Value>, LangError> {
    let (_, result) = run_checked_with_temp_file_path(label, source);
    result
}

pub fn assert_parse_error(source: &str, expected_message: &str) {
    let error = run_source_checked(source.to_string()).expect_err("expected parse error");
    assert_eq!(error.kind, LangErrorKind::Parse);
    assert!(
        error.message.contains(expected_message),
        "expected error message {:?} to contain {:?}",
        error.message,
        expected_message
    );
}

pub fn run_unchecked_with_temp_file_path(label: &str, source: &str) -> (String, Option<Value>) {
    let file_path = unique_temp_file_path(label);
    fs::write(&file_path, source).expect("failed to write temp source");
    let result = run_file(&file_path);
    let _ = fs::remove_file(&file_path);
    (file_path, result)
}

pub fn unchecked_file_panics_with_temp_file(label: &str, source: &str) -> bool {
    let file_path = unique_temp_file_path(label);
    fs::write(&file_path, source).expect("failed to write temp source");
    let result = std::panic::catch_unwind(|| run_file(&file_path));
    let _ = fs::remove_file(&file_path);
    result.is_err()
}

pub fn unchecked_file_panic_message_with_temp_file(label: &str, source: &str) -> (String, String) {
    let file_path = unique_temp_file_path(label);
    fs::write(&file_path, source).expect("failed to write temp source");
    let message = unchecked_file_panic_message(&file_path);
    let _ = fs::remove_file(&file_path);
    (file_path, message)
}

pub fn unchecked_file_panic_message(file_path: &str) -> String {
    let payload =
        std::panic::catch_unwind(|| run_file(file_path)).expect_err("expected panic payload");
    panic_payload_to_message(payload)
}
