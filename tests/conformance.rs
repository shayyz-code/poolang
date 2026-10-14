//! Conformance corpus harness.
//!
//! Runs the `poo` binary on every `tests/conformance/<area>/<case>.poo` and
//! compares the result with the sibling expectation files. See
//! `tests/conformance/README.md` for the file format, `BLESS=1`, and
//! `POO_CASE=<substring>`.

use std::borrow::Cow;
use std::fmt::Write as _;
use std::fs;
use std::io::{self, Read};
use std::path::{Path, PathBuf};
use std::process::{Child, Command, ExitStatus, Stdio};
use std::thread;
use std::time::{Duration, Instant};

/// Wall-clock budget for a single case before the process is killed.
const CASE_TIMEOUT: Duration = Duration::from_secs(10);
/// How often a running case is polled for completion.
const POLL_INTERVAL: Duration = Duration::from_millis(5);
/// Exit code the CLI uses for reported (typed) errors.
const ERROR_EXIT_CODE: i32 = 1;
/// Exit code of a Rust process that panicked.
const PANIC_EXIT_CODE: i32 = 101;
/// Maximum bytes of each output stream shown in a failure report.
const REPORT_STREAM_LIMIT: usize = 4096;

#[test]
fn conformance_corpus() {
    let manifest_dir = Path::new(env!("CARGO_MANIFEST_DIR"));
    let corpus_dir = manifest_dir.join("tests").join("conformance");
    let bless = std::env::var_os("BLESS").is_some_and(|value| value == "1");
    let filter = std::env::var("POO_CASE")
        .ok()
        .filter(|value| !value.is_empty());

    let mut cases = Vec::new();
    if let Err(error) = collect_cases(&corpus_dir, &mut cases) {
        panic!("cannot read corpus {}: {error}", corpus_dir.display());
    }
    cases.sort();

    let mut report = Report::default();
    for case in &cases {
        let name = case_name(&corpus_dir, case);
        if filter.as_deref().is_some_and(|f| !name.contains(f)) {
            continue;
        }
        report.total += 1;
        run_case(manifest_dir, case, &name, bless, &mut report);
    }

    let summary = report.summary(filter.as_deref());
    if report.total == 0 {
        panic!("no conformance cases matched\n{summary}");
    }
    if !report.failed.is_empty() {
        panic!("{summary}");
    }
    println!("{summary}");
}

#[derive(Default)]
struct Report {
    total: usize,
    passed: usize,
    blessed: Vec<String>,
    known_failures: Vec<String>,
    failed: Vec<String>,
}

impl Report {
    fn summary(&self, filter: Option<&str>) -> String {
        let mut out = String::new();
        for name in &self.blessed {
            let _ = writeln!(out, "BLESSED {name}");
        }
        for line in &self.known_failures {
            let _ = writeln!(out, "KNOWN FAILURE {line}");
        }
        for line in &self.failed {
            let _ = writeln!(out, "FAILED {line}");
        }
        let _ = write!(
            out,
            "conformance: {} passed, {} known failures, {} failed",
            self.passed,
            self.known_failures.len(),
            self.failed.len()
        );
        if !self.blessed.is_empty() {
            let _ = write!(out, ", {} blessed", self.blessed.len());
        }
        if let Some(filter) = filter {
            let _ = write!(out, " (POO_CASE={filter:?}, {} matched)", self.total);
        }
        out
    }
}

/// Collects every `.poo` file below `dir`, recursively.
fn collect_cases(dir: &Path, cases: &mut Vec<PathBuf>) -> io::Result<()> {
    for entry in fs::read_dir(dir)? {
        let path = entry?.path();
        if path.is_dir() {
            collect_cases(&path, cases)?;
        } else if path.extension().is_some_and(|ext| ext == "poo") {
            cases.push(path);
        }
    }
    Ok(())
}

/// Corpus-relative case name with `/` separators and no extension, such as
/// `basics/hello`.
fn case_name(corpus_dir: &Path, case: &Path) -> String {
    let relative = case.strip_prefix(corpus_dir).unwrap_or(case);
    let parts: Vec<_> = relative
        .with_extension("")
        .components()
        .map(|part| part.as_os_str().to_string_lossy().into_owned())
        .collect();
    parts.join("/")
}

struct Outcome {
    /// The case path exactly as passed to `poo`.
    path: String,
    status: ExitStatus,
    stdout: String,
    stderr: String,
}

fn run_case(manifest_dir: &Path, case: &Path, name: &str, bless: bool, report: &mut Report) {
    let ignore = read_optional(&case.with_extension("ignore"));
    let tracking_issue = match &ignore {
        Ok(Some(text)) => match issue_reference(text) {
            Some(issue) => Some(issue),
            None => {
                report.failed.push(format!(
                    "{name}: {name}.ignore must name the tracking issue, such as `#45`"
                ));
                return;
            }
        },
        Ok(None) => None,
        Err(error) => {
            report.failed.push(format!("{name}: {error}"));
            return;
        }
    };

    let blessing = bless && tracking_issue.is_none();
    if !blessing && let Err(problem) = validate_case(case, name, tracking_issue.is_some()) {
        report.failed.push(format!("{name}: {problem}"));
        return;
    }

    let outcome = run_poo(manifest_dir, case);

    if blessing {
        match outcome.and_then(|outcome| bless_case(case, &outcome)) {
            Ok(()) => report.blessed.push(name.to_owned()),
            Err(error) => report.failed.push(format!("{name}: {error}")),
        }
        return;
    }

    let verdict = outcome.and_then(|outcome| check_case(case, &outcome));
    match (tracking_issue, verdict) {
        (None, Ok(())) => report.passed += 1,
        (None, Err(problem)) => report.failed.push(format!("{name}: {problem}")),
        (Some(issue), Err(problem)) => {
            let first_line = problem.lines().next().unwrap_or_default();
            report
                .known_failures
                .push(format!("{name} ({issue}): {first_line}"));
        }
        (Some(issue), Ok(())) => report.failed.push(format!(
            "{name}: passes but is marked as a known failure; delete {name}.ignore and close {issue}"
        )),
    }
}

/// Rejects malformed expectation files before the case runs, so a problem
/// is reported even when the case is a known failure.
fn validate_case(case: &Path, name: &str, ignored: bool) -> Result<(), String> {
    let expected_error = read_optional(&case.with_extension("error"))?;
    if expected_error
        .as_deref()
        .is_some_and(|text| text.trim().is_empty())
    {
        return Err(format!(
            "{name}.error is empty; put the expected stderr substring in it"
        ));
    }
    if expected_error.is_none() && !case.with_extension("stdout").exists() {
        let hint = if ignored {
            "write the intended output by hand"
        } else {
            "run with BLESS=1 to create one"
        };
        return Err(format!(
            "has neither {name}.stdout nor {name}.error; {hint}"
        ));
    }
    Ok(())
}

/// Returns the first `#<digits>` reference in an `.ignore` file.
fn issue_reference(text: &str) -> Option<String> {
    text.split(|c: char| !(c == '#' || c.is_ascii_digit()))
        .filter_map(|word| word.strip_prefix('#'))
        .find(|digits| !digits.is_empty() && digits.bytes().all(|b| b.is_ascii_digit()))
        .map(|digits| format!("#{digits}"))
}

/// Runs the interpreter on one case with a deadline.
fn run_poo(manifest_dir: &Path, case: &Path) -> Result<Outcome, String> {
    // A path relative to the crate root keeps paths in diagnostics stable
    // across machines.
    let arg = case.strip_prefix(manifest_dir).unwrap_or(case);
    let mut child = Command::new(env!("CARGO_BIN_EXE_poo"))
        .arg(arg)
        .current_dir(manifest_dir)
        .stdin(Stdio::null())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .map_err(|error| format!("cannot start poo: {error}"))?;

    // Drain both pipes on their own threads so a chatty case cannot block on
    // a full pipe while it is being polled.
    let stdout = drain(child.stdout.take());
    let stderr = drain(child.stderr.take());

    let status = wait_with_deadline(&mut child)?;
    let stdout = join_output(stdout, "stdout")?;
    let stderr = join_output(stderr, "stderr")?;
    match status {
        Some(status) => Ok(Outcome {
            path: arg.display().to_string(),
            status,
            stdout,
            stderr,
        }),
        None => Err(format!(
            "timed out after {}s and was killed",
            CASE_TIMEOUT.as_secs()
        )),
    }
}

fn drain<R: Read + Send + 'static>(pipe: Option<R>) -> thread::JoinHandle<io::Result<Vec<u8>>> {
    thread::spawn(move || {
        let mut buffer = Vec::new();
        if let Some(mut pipe) = pipe {
            pipe.read_to_end(&mut buffer)?;
        }
        Ok(buffer)
    })
}

fn join_output(
    handle: thread::JoinHandle<io::Result<Vec<u8>>>,
    stream: &str,
) -> Result<String, String> {
    match handle.join() {
        Ok(Ok(bytes)) => Ok(normalize(&String::from_utf8_lossy(&bytes))),
        Ok(Err(error)) => Err(format!("cannot read {stream}: {error}")),
        Err(_) => Err(format!("{stream} reader thread panicked")),
    }
}

/// Waits for the child until [`CASE_TIMEOUT`]; kills it and returns `None`
/// when the deadline passes.
fn wait_with_deadline(child: &mut Child) -> Result<Option<ExitStatus>, String> {
    let deadline = Instant::now() + CASE_TIMEOUT;
    loop {
        match child.try_wait() {
            Ok(Some(status)) => return Ok(Some(status)),
            Ok(None) if Instant::now() >= deadline => {
                let _ = child.kill();
                let _ = child.wait();
                return Ok(None);
            }
            Ok(None) => thread::sleep(POLL_INTERVAL),
            Err(error) => {
                let _ = child.kill();
                let _ = child.wait();
                return Err(format!("cannot wait for poo: {error}"));
            }
        }
    }
}

/// Compares an outcome with the case's expectation files.
fn check_case(case: &Path, outcome: &Outcome) -> Result<(), String> {
    check_status(outcome)?;
    let expected_error = read_optional(&case.with_extension("error"))?;
    let expected_stdout = read_optional(&case.with_extension("stdout"))?;
    let code = outcome.status.code();

    match &expected_error {
        Some(expected) => {
            if code != Some(ERROR_EXIT_CODE) {
                return Err(format!(
                    "expected exit code {ERROR_EXIT_CODE} with an error, got {}\n{}",
                    describe_status(outcome.status),
                    streams(outcome)
                ));
            }
            let needle = expected.trim();
            if !outcome.stderr.contains(needle) {
                return Err(format!(
                    "stderr does not contain {needle:?}\n{}",
                    streams(outcome)
                ));
            }
        }
        None => {
            if code != Some(0) {
                return Err(format!(
                    "expected exit code 0, got {}\n{}",
                    describe_status(outcome.status),
                    streams(outcome)
                ));
            }
        }
    }

    if let Some(expected) = expected_stdout {
        let expected = normalize(&expected);
        if outcome.stdout != expected {
            return Err(stdout_mismatch(&expected, &outcome.stdout));
        }
    }
    Ok(())
}

/// Rejects outcomes that never count as a pass, blessed or not.
fn check_status(outcome: &Outcome) -> Result<(), String> {
    match outcome.status.code() {
        Some(PANIC_EXIT_CODE) => Err(format!(
            "the interpreter panicked (exit code {PANIC_EXIT_CODE})\n{}",
            streams(outcome)
        )),
        Some(0) | Some(ERROR_EXIT_CODE) => Ok(()),
        _ => Err(format!(
            "unexpected termination: {}\n{}",
            describe_status(outcome.status),
            streams(outcome)
        )),
    }
}

/// Rewrites the expectation files from the actual outcome.
///
/// A successful run writes `.stdout` and removes a stale `.error`. An error
/// run writes stderr's first line, without the `in '<case path>': ` prefix,
/// to `.error`, unless the existing `.error` still matches (so hand-trimmed
/// substrings survive), and refreshes `.stdout` only when the case already
/// has one.
fn bless_case(case: &Path, outcome: &Outcome) -> Result<(), String> {
    check_status(outcome)?;
    let stdout_path = case.with_extension("stdout");
    let error_path = case.with_extension("error");
    if outcome.status.success() {
        if error_path.exists() {
            fs::remove_file(&error_path)
                .map_err(|error| format!("cannot remove {}: {error}", error_path.display()))?;
        }
        return write_file(&stdout_path, &outcome.stdout);
    }

    let still_matches = read_optional(&error_path)?.is_some_and(|expected| {
        let needle = expected.trim();
        !needle.is_empty() && outcome.stderr.contains(needle)
    });
    if !still_matches {
        let first_line = outcome
            .stderr
            .lines()
            .find(|line| !line.trim().is_empty())
            .unwrap_or_default();
        // Drop the path so the expectation survives moving the case.
        let path_prefix = format!("'{}': ", outcome.path);
        let message = first_line
            .split_once(&path_prefix)
            .map_or(first_line, |(_, message)| message);
        write_file(&error_path, &format!("{message}\n"))?;
    }
    if stdout_path.exists() {
        write_file(&stdout_path, &outcome.stdout)?;
    }
    Ok(())
}

fn stdout_mismatch(expected: &str, actual: &str) -> String {
    let expected_lines = output_lines(expected);
    let actual_lines = output_lines(actual);
    let line = expected_lines
        .iter()
        .zip(&actual_lines)
        .position(|(e, a)| e != a)
        .unwrap_or_else(|| expected_lines.len().min(actual_lines.len()));
    let headline = match (expected_lines.get(line), actual_lines.get(line)) {
        (None, None) => "stdout differs only in its trailing newline".to_owned(),
        (expected_line, actual_line) => format!(
            "stdout differs at line {}: expected {}, got {}",
            line + 1,
            describe_line(expected_line),
            describe_line(actual_line)
        ),
    };
    format!(
        "{headline}\n--- expected stdout\n{}\n--- actual stdout\n{}\n---",
        cap(expected),
        cap(actual)
    )
}

/// Splits output into lines; a final newline does not start an extra line.
fn output_lines(text: &str) -> Vec<&str> {
    text.strip_suffix('\n')
        .unwrap_or(text)
        .split('\n')
        .collect()
}

fn describe_line(line: Option<&&str>) -> String {
    match line {
        Some(text) => format!("{text:?}"),
        None => "<end of output>".to_owned(),
    }
}

fn streams(outcome: &Outcome) -> String {
    format!(
        "--- stdout\n{}\n--- stderr\n{}\n---",
        cap(&outcome.stdout),
        cap(&outcome.stderr)
    )
}

/// Shortens a stream for a failure report to at most [`REPORT_STREAM_LIMIT`]
/// bytes, noting how much was cut.
fn cap(text: &str) -> Cow<'_, str> {
    if text.len() <= REPORT_STREAM_LIMIT {
        return Cow::Borrowed(text);
    }
    let mut end = REPORT_STREAM_LIMIT;
    while !text.is_char_boundary(end) {
        end -= 1;
    }
    Cow::Owned(format!(
        "{}\n[... {} more bytes truncated]",
        &text[..end],
        text.len() - end
    ))
}

fn describe_status(status: ExitStatus) -> String {
    match status.code() {
        Some(code) => format!("exit code {code}"),
        None => format!("termination without exit code ({status})"),
    }
}

/// Normalizes line endings so Windows checkouts compare equal.
fn normalize(text: &str) -> String {
    text.replace("\r\n", "\n")
}

fn read_optional(path: &Path) -> Result<Option<String>, String> {
    match fs::read_to_string(path) {
        Ok(text) => Ok(Some(text)),
        Err(error) if error.kind() == io::ErrorKind::NotFound => Ok(None),
        Err(error) => Err(format!("cannot read {}: {error}", path.display())),
    }
}

fn write_file(path: &Path, contents: &str) -> Result<(), String> {
    fs::write(path, contents).map_err(|error| format!("cannot write {}: {error}", path.display()))
}
