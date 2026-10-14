# Conformance corpus

Language behaviour is specified here as PooLang programs plus their expected
output. The cases test the `poo` binary end to end, so they do not depend on the
Rust API and can be rewritten when the syntax changes while keeping the same
expectations.

The harness is `tests/conformance.rs`. It uses only the standard library.

## Running

```sh
cargo test --test conformance                 # run every case
cargo test --test conformance -- --nocapture  # also print the summary on success
POO_CASE=basics/ cargo test --test conformance  # only cases whose name contains "basics/"
BLESS=1 cargo test --test conformance         # rewrite expected files from actual output
```

A case's name is its path inside the corpus without the extension, such as
`basics/hello`. `POO_CASE` keeps the cases whose name contains its value, and
the run fails if no case matches. `cargo test` and `just check` run the corpus
too.

## Layout

Each case is `tests/conformance/<area>/<case>.poo`. Cases run in sorted path
order, one `poo` process per case, with the crate root as the working directory
and the case path (such as `tests/conformance/basics/hello.poo`) as the only
argument. A case that runs longer than 10 seconds is killed and fails.

Files next to the case describe what it must do:

| File | Meaning |
| --- | --- |
| `<case>.stdout` | Exact expected stdout. Required unless `<case>.error` exists. |
| `<case>.error` | The case must fail: exit code 1, and stderr must contain this file's trimmed content. An empty or whitespace-only `.error` is rejected, because it would accept any error. If `<case>.stdout` also exists, stdout must match it too. |
| `<case>.ignore` | Known failure. Must contain the tracking issue, such as `#45`, plus an optional note. The case still needs `<case>.stdout` or `<case>.error`. |

Rules that apply to every case:

- Without `.error`, the case must exit with code 0.
- Exit code 101 means the interpreter panicked. That is always a failure, even
  for an `.error` case, because PooLang must report errors instead of
  panicking.
- `\r\n` is compared as `\n`, so Windows checkouts still match.
- A top-level `return` prints `Result: <value>` on stdout, which is a handy way
  to check a value without importing `std::pout`.
- Keep `.error` to the stable part of the message (for example
  `Undefined variable: x`), not the file path.
- Keep output deterministic. For example, do not print maps until #69 makes
  their display order stable.

## Known failures

A case with `.ignore` documents behaviour that is wanted but not yet
implemented. Write the expected files by hand, as they should be once the issue
is fixed. Malformed expectation files (an `.ignore` without an issue number, an
empty `.error`, or neither `.stdout` nor `.error`) fail the run even for an
ignored case.

- If the case fails, the harness reports `KNOWN FAILURE <case> (#<issue>)` and
  the run still passes.
- If the case passes, the run fails: the fix has landed, so delete the
  `.ignore` file and close the issue.
- `BLESS=1` never rewrites the expected files of an ignored case.

## Blessing

With `BLESS=1`, each case that is not ignored is run and its expected files are
rewritten instead of compared:

- Exit code 0 writes `<case>.stdout` and removes a stale `<case>.error`.
- Exit code 1 writes the first line of stderr to `<case>.error`, with the
  `in '<case path>': ` prefix removed so only the message remains. An existing
  `.error` that still matches is kept, and `<case>.stdout` is refreshed only if
  it already exists.
- A panic or timeout is still reported as a failure and nothing is written.

The summary lists each case as `BLESSED <case>` and counts blessed cases
separately from passed ones.

Review the diff before committing blessed files.
