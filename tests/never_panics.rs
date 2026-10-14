//! Never-panic harness for the PooLang lexer and parser.
//!
//! Untrusted, model-written source must never crash the host, so lexing and
//! parsing must return a `LangError` instead of panicking on any input. This
//! harness feeds the checked front end with:
//!
//! - every char-boundary prefix of every `examples/*.poo` and
//!   `tests/conformance/**/*.poo` file,
//! - random edits of those files,
//! - random sequences of v0.1 tokens and statement fragments,
//! - random Unicode text, and
//! - pathological inputs (deep nesting, ~1 MB of whitespace or comments,
//!   30-digit literals, a 100k-char identifier, unterminated literals).
//!
//! Every input runs inside `catch_unwind` on a worker thread with a fixed
//! stack. A panic fails the test and prints the input, minimized by greedy
//! deletion, with its panic message. Known crashers are listed with their
//! issue in `tests/never_panics/known_crashers.txt`.
//!
//! The entry point is `Parser::parse_checked`, which drives the lexer; the
//! lexer's own checked API is crate-private. Execution is out of scope until
//! the interpreter has resource budgets.
//!
//! Run `cargo test --test never_panics -- --nocapture` to see the summary.
//! Environment variables:
//!
//! - `POO_FUZZ_SEED`: PRNG seed, decimal or `0x` hex (default `DEFAULT_SEED`).
//! - `POO_FUZZ_ITERS`: random inputs per random generator
//!   (default `DEFAULT_ITERS`).
//! - `POO_FUZZ_INCLUDE_KNOWN=1`: also run generators listed as known crashers,
//!   which is how to confirm a fix. A stack overflow still aborts the process.
//! - `POO_FUZZ_ONLY=<generator>`: run only that generator, for example
//!   `POO_FUZZ_ONLY=huge_whitespace POO_FUZZ_INCLUDE_KNOWN=1`.

use poo::lexer::Lexer;
use poo::parser::Parser;
use std::cell::RefCell;
use std::fmt::Write as _;
use std::panic::{self, AssertUnwindSafe};
use std::path::{Path, PathBuf};
use std::sync::mpsc::{self, Receiver, RecvTimeoutError, Sender};
use std::thread;
use std::time::{Duration, Instant};

const DEFAULT_SEED: u64 = 0xB00C_AFE5_EED0_0080;
const DEFAULT_ITERS: usize = 5_000;
const WORKER_STACK_BYTES: usize = 8 * 1024 * 1024;
const HANG_TIMEOUT: Duration = Duration::from_secs(30);
const MAX_REPORTED_FAILURES: usize = 10;
const MINIMIZE_BUDGET: Duration = Duration::from_secs(2);
const MINIMIZE_MAX_ATTEMPTS: usize = 5_000;
const KNOWN_CRASHERS: &str = include_str!("never_panics/known_crashers.txt");

/// Parses `source` with the checked front end, discarding the result.
fn parse_source(source: &str) {
    let mut parser = Parser::new(Lexer::new(source.to_string()));
    let _ = parser.parse_checked();
}

// ---------------------------------------------------------------------------
// Deterministic PRNG
// ---------------------------------------------------------------------------

/// xorshift64* (Vigna 2014): tiny, fast, and reproducible across platforms.
struct Rng(u64);

impl Rng {
    fn new(seed: u64) -> Self {
        // A zero state would stay zero forever.
        Rng(if seed == 0 {
            0x9E37_79B9_7F4A_7C15
        } else {
            seed
        })
    }

    fn next_u64(&mut self) -> u64 {
        let mut x = self.0;
        x ^= x >> 12;
        x ^= x << 25;
        x ^= x >> 27;
        self.0 = x;
        x.wrapping_mul(0x2545_F491_4F6C_DD1D)
    }

    /// Uniform-enough integer in `0..bound` (`bound` must be non-zero).
    fn below(&mut self, bound: usize) -> usize {
        (self.next_u64() % bound as u64) as usize
    }

    /// Integer in `low..=high`.
    fn range(&mut self, low: usize, high: usize) -> usize {
        low + self.below(high - low + 1)
    }

    fn pick<'a, T>(&mut self, items: &'a [T]) -> &'a T {
        &items[self.below(items.len())]
    }
}

/// FNV-1a, used to give each generator its own stream derived from the seed,
/// so skipping one generator does not change the inputs of another.
fn fnv1a(text: &str) -> u64 {
    text.bytes().fold(0xCBF2_9CE4_8422_2325, |hash, byte| {
        (hash ^ u64::from(byte)).wrapping_mul(0x0100_0000_01B3)
    })
}

// ---------------------------------------------------------------------------
// Configuration
// ---------------------------------------------------------------------------

struct Config {
    seed: u64,
    iters: usize,
    include_known: bool,
    only: Option<String>,
}

impl Config {
    fn from_env() -> Result<Self, String> {
        let seed = match std::env::var("POO_FUZZ_SEED") {
            Ok(raw) => parse_u64(&raw).ok_or(format!("POO_FUZZ_SEED is not a u64: {raw:?}"))?,
            Err(_) => DEFAULT_SEED,
        };
        let iters = match std::env::var("POO_FUZZ_ITERS") {
            Ok(raw) => raw
                .trim()
                .parse()
                .map_err(|_| format!("POO_FUZZ_ITERS is not a usize: {raw:?}"))?,
            Err(_) => DEFAULT_ITERS,
        };
        let include_known = std::env::var("POO_FUZZ_INCLUDE_KNOWN").is_ok_and(|value| value == "1");
        let only = std::env::var("POO_FUZZ_ONLY").ok();
        Ok(Config {
            seed,
            iters,
            include_known,
            only,
        })
    }
}

fn parse_u64(raw: &str) -> Option<u64> {
    let raw = raw.trim();
    match raw.strip_prefix("0x").or_else(|| raw.strip_prefix("0X")) {
        Some(hex) => u64::from_str_radix(&hex.replace('_', ""), 16).ok(),
        None => raw.replace('_', "").parse().ok(),
    }
}

// ---------------------------------------------------------------------------
// Known crashers
// ---------------------------------------------------------------------------

#[derive(Debug)]
enum KnownKind {
    /// Skip every input from this generator (stack overflows cannot be caught).
    Generator,
    /// A caught panic whose message contains this text is reported as known.
    Panic,
}

#[derive(Debug)]
struct KnownEntry {
    kind: KnownKind,
    key: String,
    issue: String,
    description: String,
}

fn parse_known_crashers(text: &str) -> Result<Vec<KnownEntry>, String> {
    let mut entries = Vec::new();
    for (index, line) in text.lines().enumerate() {
        let line = line.trim();
        if line.is_empty() || line.starts_with('#') {
            continue;
        }
        let fields: Vec<&str> = line.splitn(4, '|').map(str::trim).collect();
        let [kind, key, issue, description] = fields[..] else {
            return Err(format!(
                "known_crashers.txt:{}: expected `kind | key | issue | description`",
                index + 1
            ));
        };
        let kind = match kind {
            "generator" => KnownKind::Generator,
            "panic" => KnownKind::Panic,
            other => {
                return Err(format!(
                    "known_crashers.txt:{}: unknown kind {other:?}",
                    index + 1
                ));
            }
        };
        if key.is_empty() || !(issue.starts_with('#') && issue.len() > 1) {
            return Err(format!(
                "known_crashers.txt:{}: key must be non-empty and issue must look like #48 or #TBD",
                index + 1
            ));
        }
        entries.push(KnownEntry {
            kind,
            key: key.to_string(),
            issue: issue.to_string(),
            description: description.to_string(),
        });
    }
    Ok(entries)
}

// ---------------------------------------------------------------------------
// Corpus
// ---------------------------------------------------------------------------

fn collect_poo_files(dir: &Path, recursive: bool, out: &mut Vec<PathBuf>) {
    let Ok(entries) = std::fs::read_dir(dir) else {
        return;
    };
    for entry in entries.flatten() {
        let path = entry.path();
        if path.is_dir() {
            if recursive {
                collect_poo_files(&path, recursive, out);
            }
        } else if path.extension().is_some_and(|ext| ext == "poo") {
            out.push(path);
        }
    }
}

/// Every `.poo` file in `examples/` and `tests/conformance/**`, sorted so the
/// run is deterministic regardless of directory iteration order.
fn load_corpus() -> Vec<(String, String)> {
    let root = Path::new(env!("CARGO_MANIFEST_DIR"));
    let mut paths = Vec::new();
    collect_poo_files(&root.join("examples"), false, &mut paths);
    collect_poo_files(&root.join("tests").join("conformance"), true, &mut paths);
    paths.sort();
    paths
        .into_iter()
        .filter_map(|path| {
            let text = std::fs::read_to_string(&path).ok()?;
            let name = path
                .strip_prefix(root)
                .unwrap_or(&path)
                .display()
                .to_string();
            Some((name, text))
        })
        .collect()
}

// ---------------------------------------------------------------------------
// Generators
// ---------------------------------------------------------------------------

/// v0.1 tokens, plus a few lexically interesting fragments.
const TOKENS: &[&str] = &[
    "x",
    "y",
    "foo",
    "self",
    "pout",
    "poutln",
    "_tmp",
    "<:",
    "=",
    ";",
    "{",
    "}",
    "(",
    ")",
    "[",
    "]",
    ":",
    "::",
    ".",
    "..",
    ",",
    ">>",
    "use",
    "pub",
    "struct",
    "inherits",
    "impl",
    "trait",
    "if",
    "else",
    "elif",
    "return",
    "while",
    "for",
    "in",
    "step",
    "poo",
    "poof",
    "mut",
    "otherwise",
    "true",
    "false",
    "void",
    "bool",
    "int",
    "float",
    "char",
    "str",
    "vec",
    "map",
    "Self",
    "0",
    "1",
    "42",
    "3.14",
    "1.",
    "'a'",
    "'\\n'",
    "\"hi\"",
    "\"\"",
    "\"a\\tb\"",
    "+",
    "-",
    "*",
    "/",
    "<",
    ">",
    "==",
    "!",
    "!=",
    "and",
    "or",
    "not",
    "// note //",
    "\n",
];

/// Fragments inserted by the mutator, biased towards lexer edge cases.
const MUTATION_FRAGMENTS: &[&str] = &[
    "\"",
    "'",
    "/",
    "//",
    "\\",
    "(",
    ")",
    "[",
    "]",
    "{",
    "}",
    "0",
    "7",
    "9",
    "<:",
    ">>",
    "..",
    "::",
    ".",
    ";",
    ",",
    " ",
    "\n",
    "\t",
    "\r",
    "a",
    "_",
    "=",
    "!",
    "<",
    ">",
    "+",
    "-",
    "*",
    "မ",
    "é",
    "😀",
    "\u{200B}",
    "\u{FEFF}",
    "poo",
    "poof",
    "return",
    "if",
    "for",
    "struct",
    "impl",
    "99999999999999999999",
    "1.5",
    "'\\'",
    "\"\\",
];

struct Case {
    generator: &'static str,
    label: String,
    input: String,
}

fn char_prefixes(name: &str, text: &str, out: &mut Vec<Case>) {
    let mut boundaries: Vec<usize> = text.char_indices().map(|(index, _)| index).collect();
    boundaries.push(text.len());
    for end in boundaries {
        out.push(Case {
            generator: "prefixes",
            label: format!("{name}[..{end}]"),
            input: text[..end].to_string(),
        });
    }
}

fn mutate(rng: &mut Rng, text: &str) -> String {
    let mut chars: Vec<char> = text.chars().collect();
    for _ in 0..rng.range(1, 8) {
        let at = rng.below(chars.len() + 1);
        match rng.below(4) {
            0 => {
                let fragment = rng.pick(MUTATION_FRAGMENTS);
                chars.splice(at..at, fragment.chars());
            }
            1 if !chars.is_empty() => {
                let start = at.min(chars.len() - 1);
                let end = (start + rng.range(1, 8)).min(chars.len());
                chars.drain(start..end);
            }
            2 if !chars.is_empty() => {
                let start = at.min(chars.len() - 1);
                let fragment = rng.pick(MUTATION_FRAGMENTS);
                chars.splice(start..=start, fragment.chars());
            }
            _ if !chars.is_empty() => {
                // Duplicate a short range, which tends to build nesting.
                let start = at.min(chars.len() - 1);
                let end = (start + rng.range(1, 16)).min(chars.len());
                let copy: Vec<char> = chars[start..end].to_vec();
                chars.splice(start..start, copy);
            }
            _ => {}
        }
    }
    chars.into_iter().collect()
}

/// Statement-shaped fragments, so token sequences get past the first few
/// tokens and reach deeper parser states (function bodies, structs, loops).
const PHRASES: &[&str] = &[
    "poo x <: ",
    "mut y int = ",
    "poo v vec:int = ",
    "poof f(a int, b vec:char) >> int {",
    "poof g() >> void {",
    "poof h() >> Self {",
    "return ",
    "}",
    "struct S {",
    "struct T inherits S {",
    "impl {",
    "pub n int",
    "if x > 1 {",
    "} elif x == 2 {",
    "} else {",
    "while x < 3 {",
    "for i in 0..3 step 1 {",
    "use std::(pout, poutln);",
    "use math::PI;",
    "f(1, 2)",
    "x.push(1)",
    "self.n",
    "[1, 2][0]",
    "{ k: 1, s: \"v\" }",
    "1 + 2 * 3",
    "a otherwise b",
    "not true and false or x",
    ";",
];

fn token_sequence(rng: &mut Rng) -> String {
    if rng.below(2) == 0 {
        // A short expression inside a typed function body reaches the
        // parse-time type checks, which a long random sequence rarely does.
        let mut out = String::from("poof f(a int, s str) >> int { return ");
        for _ in 0..rng.range(1, 6) {
            let list = if rng.below(4) == 0 { PHRASES } else { TOKENS };
            out.push_str(rng.pick(list));
            out.push(' ');
        }
        out.push_str("; }");
        return out;
    }
    let mut out = String::new();
    for _ in 0..rng.range(1, 64) {
        if rng.below(2) == 0 {
            out.push_str(rng.pick(PHRASES));
        } else {
            out.push_str(rng.pick(TOKENS));
        }
        out.push_str(rng.pick(&[" ", " ", " ", "", "\n"]));
    }
    out
}

/// Random scalar values from the whole Unicode range (never surrogates,
/// since a Rust `String` cannot hold them), biased towards ASCII.
fn random_unicode(rng: &mut Rng) -> String {
    let len = rng.range(0, 128);
    let mut out = String::with_capacity(len);
    while out.chars().count() < len {
        let candidate = match rng.below(4) {
            0 => rng.range(0x20, 0x7E) as u32,
            1 => rng.range(0, 0x7F) as u32,
            2 => rng.range(0x80, 0xFFFF) as u32,
            _ => rng.range(0x1_0000, 0x10_FFFF) as u32,
        };
        if let Some(ch) = char::from_u32(candidate) {
            out.push(ch);
        }
    }
    out
}

const ONE_MIB: usize = 1024 * 1024;
const NESTING_DEPTH: usize = 10_000;

fn nested(open: &str, inner: &str, close: &str) -> String {
    format!(
        "poo x <: {}{}{};",
        open.repeat(NESTING_DEPTH),
        inner,
        close.repeat(NESTING_DEPTH)
    )
}

/// One case per pathological shape, each under its own generator name so a
/// single known crasher can be skipped without hiding the others.
fn pathological_cases() -> Vec<Case> {
    let thirty_digits = "123456789012345678901234567890";
    let cases: Vec<(&'static str, String)> = vec![
        ("deep_paren_unclosed", "(".repeat(NESTING_DEPTH)),
        ("deep_paren_balanced", nested("(", "1", ")")),
        ("deep_bracket_unclosed", "[".repeat(NESTING_DEPTH)),
        ("deep_bracket_balanced", nested("[", "1", "]")),
        ("deep_curly_unclosed", "{".repeat(NESTING_DEPTH)),
        ("deep_curly_balanced", nested("{", "", "}")),
        ("deep_if_blocks", "if true {".repeat(NESTING_DEPTH)),
        (
            "deep_unary_not",
            format!("poo x <: {}true;", "not ".repeat(NESTING_DEPTH)),
        ),
        (
            "deep_unary_minus",
            format!("poo x <: {}1;", "-".repeat(NESTING_DEPTH)),
        ),
        (
            "long_binary_chain",
            format!("poo x <: 1{};", " + 1".repeat(NESTING_DEPTH)),
        ),
        (
            "huge_whitespace",
            format!("{}return 1;", " ".repeat(ONE_MIB)),
        ),
        (
            "huge_comments",
            format!("{}return 1;", "// //".repeat(ONE_MIB / 5)),
        ),
        (
            "huge_comment_body",
            format!("//{}//return 1;", "x".repeat(ONE_MIB)),
        ),
        (
            "int_literal_30_digits",
            format!("poo x <: {thirty_digits};"),
        ),
        (
            "int_literal_30_digits_range",
            format!("for i in {thirty_digits}..1 {{}}"),
        ),
        (
            "float_literal_30_digits",
            format!("poo x <: {thirty_digits}.{thirty_digits};"),
        ),
        (
            "identifier_100k",
            format!("poo {} <: 1;", "a".repeat(100_000)),
        ),
        (
            "string_1mib",
            format!("poo x <: \"{}\";", "s".repeat(ONE_MIB)),
        ),
        ("unterminated_string_eof", "poo x <: \"abc".to_string()),
        (
            "unterminated_string_escape_eof",
            "poo x <: \"abc\\".to_string(),
        ),
        ("unterminated_char_eof", "poo x <: 'a".to_string()),
        ("unterminated_comment_eof", "// never closed".to_string()),
        ("lone_quote", "\"".to_string()),
        ("non_ascii_identifier", "poo မြန်မာ <: 1;".to_string()),
        ("non_ascii_string", "poo x <: \"မြန်မာ 😀\";".to_string()),
        (
            "nul_and_controls",
            "poo x <: 1;\u{0}\u{1}\u{7F}".to_string(),
        ),
        ("bom_prefix", "\u{FEFF}poo x <: 1;".to_string()),
        ("empty", String::new()),
    ];
    cases
        .into_iter()
        .map(|(generator, input)| Case {
            generator,
            label: generator.to_string(),
            input,
        })
        .collect()
}

/// Builds every input. Random generators each draw from their own stream.
fn build_cases(config: &Config, corpus: &[(String, String)]) -> Vec<Case> {
    let mut cases = Vec::new();

    for (name, text) in corpus {
        char_prefixes(name, text, &mut cases);
    }

    if !corpus.is_empty() {
        let mut rng = Rng::new(config.seed ^ fnv1a("mutations"));
        for index in 0..config.iters {
            let (name, text) = rng.pick(corpus);
            cases.push(Case {
                generator: "mutations",
                label: format!("mutation #{index} of {name}"),
                input: mutate(&mut rng, text),
            });
        }
    }

    let mut rng = Rng::new(config.seed ^ fnv1a("tokens"));
    for index in 0..config.iters {
        cases.push(Case {
            generator: "tokens",
            label: format!("token sequence #{index}"),
            input: token_sequence(&mut rng),
        });
    }

    let mut rng = Rng::new(config.seed ^ fnv1a("unicode"));
    for index in 0..config.iters / 4 {
        cases.push(Case {
            generator: "unicode",
            label: format!("unicode #{index}"),
            input: random_unicode(&mut rng),
        });
    }

    cases.extend(pathological_cases());
    cases
}

// ---------------------------------------------------------------------------
// Worker thread
// ---------------------------------------------------------------------------

#[derive(Clone, Debug, PartialEq)]
enum Outcome {
    Ok,
    Panic { location: String, message: String },
}

thread_local! {
    static LAST_PANIC: RefCell<Option<(String, String)>> = const { RefCell::new(None) };
}

const WORKER_NAME: &str = "never-panics-worker";

/// Records a worker panic instead of printing it, so thousands of expected
/// panics do not flood the output. Panics on other threads still print.
fn quiet_hook(info: &panic::PanicHookInfo<'_>) {
    if thread::current().name() != Some(WORKER_NAME) {
        eprintln!("{info}");
        return;
    }
    let location = info
        .location()
        .map(|location| format!("{}:{}", location.file(), location.line()))
        .unwrap_or_else(|| "<unknown location>".to_string());
    let message = info
        .payload_as_str()
        .unwrap_or("<non-string panic payload>")
        .to_string();
    LAST_PANIC.with(|slot| *slot.borrow_mut() = Some((location, message)));
}

/// A long-lived thread with a fixed stack that parses one input at a time.
struct Worker {
    jobs: Sender<String>,
    results: Receiver<Outcome>,
}

impl Worker {
    fn spawn() -> Self {
        let (jobs, job_rx) = mpsc::channel::<String>();
        let (result_tx, results) = mpsc::channel();
        thread::Builder::new()
            .name(WORKER_NAME.to_string())
            .stack_size(WORKER_STACK_BYTES)
            .spawn(move || {
                for input in job_rx {
                    LAST_PANIC.with(|slot| *slot.borrow_mut() = None);
                    let outcome = match panic::catch_unwind(AssertUnwindSafe(|| {
                        parse_source(&input);
                    })) {
                        Ok(()) => Outcome::Ok,
                        Err(_) => {
                            let (location, message) = LAST_PANIC
                                .with(|slot| slot.borrow_mut().take())
                                .unwrap_or_default();
                            Outcome::Panic { location, message }
                        }
                    };
                    if result_tx.send(outcome).is_err() {
                        break;
                    }
                }
            })
            .unwrap_or_else(|error| panic!("failed to spawn the worker thread: {error}"));
        Worker { jobs, results }
    }

    /// Runs one input. `Err` means the worker hung or died.
    fn run(&self, input: &str) -> Result<Outcome, String> {
        self.jobs
            .send(input.to_string())
            .map_err(|_| "worker thread exited".to_string())?;
        match self.results.recv_timeout(HANG_TIMEOUT) {
            Ok(outcome) => Ok(outcome),
            Err(RecvTimeoutError::Timeout) => Err(format!(
                "parsing did not finish within {HANG_TIMEOUT:?} (possible infinite loop)"
            )),
            Err(RecvTimeoutError::Disconnected) => Err("worker thread exited".to_string()),
        }
    }
}

// ---------------------------------------------------------------------------
// Minimization and reporting
// ---------------------------------------------------------------------------

/// Shrinks a panicking input while it still panics at the same location.
/// Bounded by `MINIMIZE_BUDGET` and `MINIMIZE_MAX_ATTEMPTS`.
struct Minimizer<'a> {
    worker: &'a Worker,
    location: &'a str,
    started: Instant,
    attempts: usize,
}

impl Minimizer<'_> {
    /// `None` once the budget is spent.
    fn still_fails(&mut self, candidate: &[char]) -> Option<bool> {
        if self.attempts >= MINIMIZE_MAX_ATTEMPTS || self.started.elapsed() > MINIMIZE_BUDGET {
            return None;
        }
        self.attempts += 1;
        let text: String = candidate.iter().collect();
        Some(matches!(
            self.worker.run(&text),
            Ok(Outcome::Panic { location, .. }) if location == self.location
        ))
    }

    /// Greedy chunk deletion with halving chunk sizes.
    fn delete_chunks(&mut self, chars: &mut Vec<char>) -> Option<()> {
        let mut chunk = chars.len().div_ceil(2).max(1);
        loop {
            let mut start = 0;
            while start < chars.len() {
                let end = (start + chunk).min(chars.len());
                let mut candidate = chars.clone();
                candidate.drain(start..end);
                if self.still_fails(&candidate)? {
                    *chars = candidate;
                } else {
                    start += chunk;
                }
            }
            if chunk == 1 {
                return Some(());
            }
            chunk = chunk.div_ceil(2);
        }
    }

    /// Keeps the shortest failing suffix, then the shortest failing prefix.
    /// Chunk deletion alone gets stuck when every smaller deletion breaks the
    /// syntax that leads the parser to the panic.
    fn trim_ends(&mut self, chars: &mut Vec<char>) -> Option<()> {
        for start in (1..chars.len()).rev() {
            if self.still_fails(&chars[start..])? {
                chars.drain(..start);
                break;
            }
        }
        for end in 1..chars.len() {
            if self.still_fails(&chars[..end])? {
                chars.truncate(end);
                break;
            }
        }
        Some(())
    }
}

fn minimize(worker: &Worker, input: &str, location: &str) -> String {
    let mut minimizer = Minimizer {
        worker,
        location,
        started: Instant::now(),
        attempts: 0,
    };
    let mut chars: Vec<char> = input.chars().collect();
    loop {
        let before = chars.len();
        if minimizer.delete_chunks(&mut chars).is_none()
            || minimizer.trim_ends(&mut chars).is_none()
            || chars.len() == before
        {
            break;
        }
    }
    chars.into_iter().collect()
}

fn escaped_preview(text: &str) -> String {
    const LIMIT: usize = 400;
    let count = text.chars().count();
    if count <= LIMIT {
        format!("{text:?}")
    } else {
        let head: String = text.chars().take(LIMIT).collect();
        format!("{head:?}... ({count} chars total)")
    }
}

/// An unknown panic, grouped by its source location.
struct Failure {
    label: String,
    location: String,
    message: String,
    minimized: String,
    minimized_message: String,
    hits: usize,
}

// ---------------------------------------------------------------------------
// The test
// ---------------------------------------------------------------------------

#[test]
fn lexer_and_parser_never_panic() {
    let started = Instant::now();
    let config = Config::from_env().unwrap_or_else(|error| panic!("{error}"));
    let known = parse_known_crashers(KNOWN_CRASHERS).unwrap_or_else(|error| panic!("{error}"));
    let corpus = load_corpus();
    let cases = build_cases(&config, &corpus);

    if let Some(only) = &config.only {
        assert!(
            cases.iter().any(|case| case.generator == only.as_str()),
            "POO_FUZZ_ONLY names an unknown generator: {only:?}"
        );
    }

    let skipped_generator = |generator: &str| {
        if config.only.as_deref().is_some_and(|only| only != generator) {
            return true;
        }
        !config.include_known
            && known
                .iter()
                .any(|entry| matches!(entry.kind, KnownKind::Generator) && entry.key == generator)
    };
    let known_panic = |message: &str| {
        known.iter().position(|entry| {
            matches!(entry.kind, KnownKind::Panic) && message.contains(&entry.key)
        })
    };

    let previous_hook = panic::take_hook();
    panic::set_hook(Box::new(quiet_hook));

    let worker = Worker::spawn();
    let mut tried = 0usize;
    let mut skipped = 0usize;
    let mut known_hits = vec![0usize; known.len()];
    let mut failures: Vec<Failure> = Vec::new();
    let mut unreported: Vec<(String, usize)> = Vec::new();
    let mut hang: Option<String> = None;

    for case in &cases {
        if skipped_generator(case.generator) {
            skipped += 1;
            continue;
        }
        tried += 1;
        let (location, message) = match worker.run(&case.input) {
            Ok(Outcome::Ok) => continue,
            Ok(Outcome::Panic { location, message }) => (location, message),
            Err(error) => {
                hang = Some(format!(
                    "[{}] {}: {error}\n  input: {}",
                    case.generator,
                    case.label,
                    escaped_preview(&case.input)
                ));
                break;
            }
        };
        if let Some(index) = known_panic(&message) {
            known_hits[index] += 1;
        } else if let Some(failure) = failures.iter_mut().find(|f| f.location == location) {
            failure.hits += 1;
        } else if let Some(entry) = unreported.iter_mut().find(|(at, _)| *at == location) {
            entry.1 += 1;
        } else if failures.len() < MAX_REPORTED_FAILURES {
            let minimized = minimize(&worker, &case.input, &location);
            let minimized_message = match worker.run(&minimized) {
                Ok(Outcome::Panic { message, .. }) => message,
                _ => message.clone(),
            };
            failures.push(Failure {
                label: format!("[{}] {}", case.generator, case.label),
                location,
                message,
                minimized,
                minimized_message,
                hits: 1,
            });
        } else {
            unreported.push((location, 1));
        }
    }

    panic::set_hook(previous_hook);

    let failure_hits = failures.iter().map(|failure| failure.hits).sum::<usize>()
        + unreported.iter().map(|(_, hits)| hits).sum::<usize>();
    let mut report = String::new();
    let _ = writeln!(
        report,
        "never_panics: seed={:#x} iters={} corpus_files={} inputs_tried={tried} \
         known_skipped={skipped} known_panics={} failures={failure_hits} elapsed={:.2?}",
        config.seed,
        config.iters,
        corpus.len(),
        known_hits.iter().sum::<usize>(),
        started.elapsed(),
    );
    for (entry, hits) in known.iter().zip(&known_hits) {
        match entry.kind {
            KnownKind::Generator if !config.include_known => {
                let _ = writeln!(
                    report,
                    "  known {} (skipped generator {}): {}",
                    entry.issue, entry.key, entry.description
                );
            }
            KnownKind::Panic if *hits > 0 => {
                let _ = writeln!(
                    report,
                    "  known {} ({hits} panics matching {:?}): {}",
                    entry.issue, entry.key, entry.description
                );
            }
            _ => {}
        }
    }
    println!("{report}");

    if let Some(hang) = hang {
        panic!("the parser hung or its worker died:\n{hang}");
    }

    if failure_hits > 0 {
        let mut message = format!(
            "{} distinct panic location(s) in the lexer/parser (seed {:#x}):\n",
            failures.len() + unreported.len(),
            config.seed
        );
        for failure in &failures {
            let _ = writeln!(
                message,
                "\n- {} panicked at {} ({} hit(s))\n  panic: {}\n  minimized input: {}\n  \
                 minimized panic: {}",
                failure.label,
                failure.location,
                failure.hits,
                failure.message,
                escaped_preview(&failure.minimized),
                failure.minimized_message,
            );
        }
        for (location, hits) in &unreported {
            let _ = writeln!(message, "\n- not minimized: {location} ({hits} hit(s))");
        }
        let _ = writeln!(
            message,
            "\nFix the panic, or list it in tests/never_panics/known_crashers.txt with its issue."
        );
        panic!("{message}");
    }
}
