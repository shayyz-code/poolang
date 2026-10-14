//! Executable PooLang language specifications.
//!
//! Every module below compiles into the single `language_specs` integration-test
//! binary, so `cargo test --test language_specs <filter>` runs any spec by name.
//! Tests are named `spec_<component>_<behavior>`; the full path a runner lists is
//! `<module>::spec_<component>_<behavior>`.
//!
//! Add a spec to the module for the subsystem it exercises:
//!
//! - `support`: shared helpers (temp-file runners, `assert_parse_error`). No tests.
//! - `lexer`: `spec_lexer_*`, plus `spec_checked_api_*` cases for lexical
//!   diagnostics (unterminated literals and comments, unknown escapes,
//!   non-ASCII identifiers).
//! - `parser`: `spec_parser_*`, plus `spec_checked_api_*` cases for syntax and
//!   parse-time type diagnostics (missing semicolon, unexpected end of input,
//!   function return type mismatch).
//! - `runtime`: `spec_value_*`, `spec_interpreter_*`, control flow and scoping
//!   (`spec_for_*`, `spec_if_*`, `spec_elif_*`, `spec_while_*`,
//!   `spec_function_*`), plus `spec_checked_api_reports_runtime_error_*`.
//! - `structs`: `spec_struct_*`.
//! - `modules`: `use` module imports (`std`, `math`, `time`). No specs yet.
//! - `cli`: `spec_cli_*`, which run the `poo` binary and pin down its exit
//!   codes, stdout, and stderr.
//! - `api`: embedding entrypoints `spec_checked_api_returns_typed_error_*`,
//!   `spec_unchecked_api_*`, `spec_checked_file_api_*`, and
//!   `spec_unchecked_file_api_*`.

mod support;

mod api;
mod cli;
mod lexer;
mod modules;
mod parser;
mod runtime;
mod structs;
