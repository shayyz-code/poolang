//! Command-line specs: characterize how the `poo` binary behaves as of v0.1.11.
//!
//! These specs pin down exit codes, stdout, and stderr so that CLI changes are
//! deliberate. Assertions stick to stable substrings and never depend on the
//! absolute temp path or on timing values. Exit code 101 means the binary
//! panicked; error paths must exit with 1 instead.

use std::fs;
use std::path::{Path, PathBuf};
use std::process::{Command, Output};
use std::sync::atomic::{AtomicUsize, Ordering};

/// A fresh, empty directory under the system temp dir, removed on drop.
///
/// Every command runs with this directory as its working directory, so
/// arguments such as `--help` cannot accidentally name an existing file.
struct TempDir {
    path: PathBuf,
}

impl TempDir {
    fn new(label: &str) -> Self {
        static COUNTER: AtomicUsize = AtomicUsize::new(0);
        let path = std::env::temp_dir().join(format!(
            "poolang-cli-{}-{}-{}",
            label,
            std::process::id(),
            COUNTER.fetch_add(1, Ordering::Relaxed)
        ));
        let _ = fs::remove_dir_all(&path);
        fs::create_dir_all(&path).expect("failed to create temp dir");
        TempDir { path }
    }

    fn write(&self, name: &str, source: &str) -> PathBuf {
        let file = self.path.join(name);
        fs::write(&file, source).expect("failed to write temp source");
        file
    }

    fn path(&self) -> &Path {
        &self.path
    }
}

impl Drop for TempDir {
    fn drop(&mut self) {
        let _ = fs::remove_dir_all(&self.path);
    }
}

fn run_poo<I, S>(dir: &TempDir, args: I) -> Output
where
    I: IntoIterator<Item = S>,
    S: AsRef<std::ffi::OsStr>,
{
    Command::new(env!("CARGO_BIN_EXE_poo"))
        .args(args)
        .current_dir(dir.path())
        .output()
        .expect("failed to spawn poo")
}

/// Writes `source` to a temp file and runs `poo <file> <extra_args...>`.
fn run_source(label: &str, source: &str, extra_args: &[&str]) -> Output {
    let dir = TempDir::new(label);
    let file = dir.write("main.poo", source);
    let mut args = vec![file.into_os_string()];
    args.extend(extra_args.iter().map(Into::into));
    run_poo(&dir, args)
}

fn stdout(output: &Output) -> String {
    String::from_utf8_lossy(&output.stdout).replace("\r\n", "\n")
}

fn stderr(output: &Output) -> String {
    String::from_utf8_lossy(&output.stderr).replace("\r\n", "\n")
}

fn assert_success(output: &Output) {
    assert_eq!(
        output.status.code(),
        Some(0),
        "expected exit code 0; stderr: {:?}",
        stderr(output)
    );
    assert_eq!(stderr(output), "", "expected empty stderr");
}

/// Asserts the documented failure contract: exit code 1 (never the panic code
/// 101), nothing on stdout, and stderr containing `expected`.
fn assert_failure(output: &Output, expected: &str) {
    assert_ne!(
        output.status.code(),
        Some(101),
        "poo panicked instead of reporting an error; stderr: {:?}",
        stderr(output)
    );
    assert_eq!(
        output.status.code(),
        Some(1),
        "expected exit code 1; stderr: {:?}",
        stderr(output)
    );
    assert_eq!(stdout(output), "", "expected empty stdout");
    assert_stderr_contains(output, expected);
}

fn assert_stderr_contains(output: &Output, expected: &str) {
    let stderr = stderr(output);
    assert!(
        stderr.contains(expected),
        "expected stderr {stderr:?} to contain {expected:?}"
    );
}

const HELLO_WITH_RETURN: &str = "use std::poutln;\npoutln(\"hello\");\nreturn 1 + 2;\n";

#[test]
fn spec_cli_runs_program_and_exits_zero() {
    let output = run_source("ok", HELLO_WITH_RETURN, &[]);
    assert_success(&output);
    assert_eq!(stdout(&output), "hello\nResult: Int(3)\n");
}

#[test]
fn spec_cli_prints_top_level_return_value_in_debug_form() {
    let output = run_source("return-string", "return \"pig\";", &[]);
    assert_success(&output);
    assert_eq!(stdout(&output), "Result: String(\"pig\")\n");

    let output = run_source("return-vector", "return [1, 2];", &[]);
    assert_success(&output);
    assert_eq!(stdout(&output), "Result: Vector([Int(1), Int(2)])\n");
}

#[test]
fn spec_cli_prints_nothing_extra_without_top_level_return() {
    let output = run_source("no-return", "use std::poutln;\npoutln(\"hello\");\n", &[]);
    assert_success(&output);
    assert_eq!(stdout(&output), "hello\n");

    let output = run_source("empty", "", &[]);
    assert_success(&output);
    assert_eq!(stdout(&output), "");
}

#[test]
fn spec_cli_reports_parse_error_on_stderr_with_exit_code_1() {
    let output = run_source("parse", "poo x <: 1", &[]);
    assert_failure(&output, "Parse error: in '");
    assert_stderr_contains(
        &output,
        "main.poo': Unexpected end of input, expected: SemiColon",
    );
}

#[test]
fn spec_cli_reports_runtime_error_on_stderr_with_exit_code_1() {
    let output = run_source("runtime", "return unknown_identifier;\n", &[]);
    assert_failure(&output, "Runtime error: in '");
    assert_stderr_contains(&output, "main.poo': Undefined variable: unknown_identifier");
}

#[test]
fn spec_cli_keeps_output_printed_before_runtime_error() {
    let output = run_source(
        "partial",
        "use std::poutln;\npoutln(\"before\");\nreturn unknown_identifier;\n",
        &[],
    );
    assert_eq!(output.status.code(), Some(1));
    assert_eq!(stdout(&output), "before\n");
    assert_stderr_contains(&output, "Undefined variable: unknown_identifier");
}

#[test]
fn spec_cli_reports_missing_file_as_io_error() {
    let dir = TempDir::new("missing");
    let output = run_poo(&dir, ["missing.poo"]);
    assert_failure(&output, "Io error: failed to read 'missing.poo': ");
}

#[test]
fn spec_cli_reports_directory_path_as_io_error() {
    let dir = TempDir::new("directory");
    let output = run_poo(&dir, ["."]);
    assert_failure(&output, "Io error: failed to read '.': ");
}

#[test]
fn spec_cli_prints_usage_without_arguments() {
    let dir = TempDir::new("no-args");
    let output = run_poo(&dir, std::iter::empty::<&str>());
    assert_failure(&output, "Usage: <program> <file_path>");
    assert_eq!(stderr(&output), "Usage: <program> <file_path>\n");
}

#[test]
fn spec_cli_treats_help_flag_as_file_path() {
    let dir = TempDir::new("help");
    let output = run_poo(&dir, ["--help"]);
    assert_failure(&output, "Io error: failed to read '--help'");
}

#[test]
fn spec_cli_treats_version_flag_as_file_path() {
    let dir = TempDir::new("version");
    let output = run_poo(&dir, ["--version"]);
    assert_failure(&output, "Io error: failed to read '--version'");
}

#[test]
fn spec_cli_speed_flag_appends_timing_line() {
    let output = run_source("speed", HELLO_WITH_RETURN, &["--speed"]);
    assert_success(&output);
    let stdout = stdout(&output);
    let prefix = "hello\nResult: Int(3)\n\nInterpreted within: ";
    assert!(
        stdout.starts_with(prefix),
        "expected stdout {stdout:?} to start with {prefix:?}"
    );
    let duration = stdout[prefix.len()..]
        .strip_suffix('\n')
        .expect("timing line should end with a newline");
    assert!(
        duration.starts_with(|c: char| c.is_ascii_digit()) && duration.ends_with('s'),
        "expected a Debug-formatted duration such as `207.125µs`, got {duration:?}"
    );
    assert!(!duration.contains('\n'), "expected a single timing line");
}

#[test]
fn spec_cli_speed_flag_omits_timing_line_on_error() {
    let output = run_source("speed-error", "poo x <: 1", &["--speed"]);
    assert_failure(&output, "Parse error: ");
    let stderr = stderr(&output);
    assert!(
        !stderr.contains("Interpreted within"),
        "expected no timing line in stderr {stderr:?}"
    );
}

#[test]
fn spec_cli_speed_flag_before_path_is_treated_as_file_path() {
    let dir = TempDir::new("speed-first");
    let file = dir.write("main.poo", HELLO_WITH_RETURN);
    let output = run_poo(&dir, [std::ffi::OsString::from("--speed"), file.into()]);
    assert_failure(&output, "Io error: failed to read '--speed'");
}

#[test]
fn spec_cli_ignores_extra_unknown_arguments() {
    let output = run_source("extra-args", HELLO_WITH_RETURN, &["extra", "--bogus"]);
    assert_success(&output);
    assert_eq!(stdout(&output), "hello\nResult: Int(3)\n");
}

#[test]
fn spec_cli_error_cases_exit_with_code_1_not_101() {
    let cases: &[(&str, &str, &[&str])] = &[
        ("parse", "poo x <: 1", &["Parse error: "]),
        (
            "runtime",
            "return unknown_identifier;",
            &["Runtime error: "],
        ),
        (
            "lexer",
            "return \"unterminated;",
            &["Parse error: ", "Unterminated string literal"],
        ),
    ];
    for (label, source, expected) in cases {
        let output = run_source(label, source, &[]);
        for needle in *expected {
            assert_failure(&output, needle);
        }
    }

    let dir = TempDir::new("error-args");
    let arg_cases: &[(&[&str], &str)] = &[
        (&[], "Usage: "),
        (&["missing.poo"], "Io error: failed to read 'missing.poo': "),
        (&["--help"], "Io error: failed to read '--help': "),
    ];
    for (args, expected) in arg_cases {
        let output = run_poo(&dir, *args);
        assert_failure(&output, expected);
    }
}

#[cfg(unix)]
#[test]
#[ignore = "#110: CLI panics with exit 101 on non-UTF-8 arguments"]
fn spec_cli_reports_non_utf8_argument_without_panicking() {
    use std::ffi::OsStr;
    use std::os::unix::ffi::OsStrExt;

    let dir = TempDir::new("non-utf8");
    let output = run_poo(&dir, [OsStr::from_bytes(b"\xff.poo")]);
    assert_ne!(
        output.status.code(),
        Some(101),
        "poo panicked instead of reporting an error; stderr: {:?}",
        stderr(&output)
    );
    assert_eq!(
        output.status.code(),
        Some(1),
        "expected exit code 1; stderr: {:?}",
        stderr(&output)
    );
    assert_eq!(stdout(&output), "", "expected empty stdout");
    let stderr = stderr(&output);
    assert!(
        stderr.contains("Io error: ") || stderr.contains("Usage: "),
        "expected an Io or usage error in stderr {stderr:?}"
    );
}

#[test]
#[ignore = "#45: interpreter panics with exit 101 on recursive calls"]
fn spec_cli_runs_recursive_function() {
    let output = run_source(
        "recursion",
        "poof fact(n int) >> int { if n < 2 { return 1; } return n * fact(n - 1); } return fact(5);\n",
        &[],
    );
    assert_success(&output);
    assert_eq!(stdout(&output), "Result: Int(120)\n");
}
