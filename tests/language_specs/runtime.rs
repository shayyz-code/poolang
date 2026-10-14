//! Runtime specs: values, evaluation, control flow, scoping, and runtime
//! diagnostics.

use poo::errors::LangErrorKind;
use poo::interpreter::Value;
use poo::{run_source, run_source_checked};

#[test]
fn spec_value_display_preserves_legacy_output() {
    assert_eq!(Value::Int(10).to_string(), "10");
    assert_eq!(format!("{}", Value::String("poo".to_string())), "poo");
    assert_eq!(
        Value::Vector(vec![Value::Int(1), Value::Boolean(true)]).to_string(),
        r#"["1", "true"]"#
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
