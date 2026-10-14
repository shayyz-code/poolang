# PooLang Roadmap

PooLang is becoming a sandboxed scripting language for AI agents: LLM agents write PooLang, and a host program runs it safely. The v0.1.11 tree-walking interpreter is the base and becomes the product; there is no LLVM rewrite.

Every item below must become a GitHub issue before implementation. Progress is tracked in the GitHub milestones [P0 Agentic dev setup through P7 Docs truth](https://github.com/shayyz-code/poolang/milestones), not with checkboxes in this file.

## Current Audit

Snapshot: 2026-10-14.

### What works

- `cargo fmt`, `cargo clippy -- -D warnings`, and all 58 integration specifications pass and are enforced in CI.
- Lexer, parser, interpreter, examples, mdBook sources, and cargo-dist release assets exist.
- The crate exposes checked file/source execution APIs with typed I/O, parse, and runtime error categories.
- Releases, dependencies, and supply-chain policy are pinned and checked (see Completed History).

### Interpreter defects

- The checked API still crashes: `run_source_checked` exits with code 101 on recursion, on a global read inside a function, on `return v[0]`, on huge integer literals, and on about 20k whitespace characters (stack overflow).
- Parse-time type inference in `type_inference.rs` is partial and rejects valid programs.
- Scoping is dynamic: a called function can read and change the caller's locals.
- Inheritance is unreliable: method overrides resolve in nondeterministic order, and a parent without an `impl` panics.
- `and`/`or` evaluate both operands.
- Declared types are never enforced.
- `!=` on `char` values panics.
- Integer arithmetic overflow is unchecked.
- The empty vector literal `[]` panics.
- About 70 `panic!` sites are converted to error strings by `catch_unwind` rather than removed.
- Tokens and AST nodes carry no source spans.
- Dead code remains: `visitor.rs` and 13 panicking parser wrappers.

### Documentation drift

- Almost every mdBook snippet fails to run.
- The book documents syntax that does not exist (`<<`, `#`, `&&`, `>=`, `poo mut`, `string`).
- The README calls PooLang a compiled backend language; the book calls it an interpreted pedagogical one.

## Locked Product Decisions

1. **Target: a sandboxed scripting language for AI agents.** Pillars:
   - Zero ambient authority: only capabilities and tools granted by the host are callable.
   - Budgets for steps, call depth, memory, and wall-clock deadline.
   - Deterministic execution.
   - Never panics on any input.
   - LLM-readable diagnostics: stable code, span, hint, and JSON output.
   - Rust embedding first; Python and WASM bindings and an MCP server later.
2. **Merging: rebase-merge only.** Squash and merge commits are disabled, history stays linear, and every commit is a self-contained Conventional Commit that passes lint and builds on its own.
3. **v0.2 syntax: conventional syntax, keeping `poo` and `poof`.**
   - Bindings: `poo x = 1;` and `poo mut x = 0;`.
   - `name: type` annotations are optional and enforced at runtime.
   - Functions: `poof f(a: int) -> int { ... }`.
   - Operators: `=`, `==`, `!=`, `<`, `<=`, `>`, `>=`, `&&`, `||`, `!`, `%`, `??`.
   - Comments: `//` and `/* ... */`.
   - Control flow: `else if`, `break`, `continue`, ranges `a..b`.
   - Values: `null`, JSON-style literals, single- and double-quoted strings; no `char` type.
   - Types: `int`, `float`, `bool`, `str`, `vec[T]`, `map[K, V]`.
   - Output: `print` and `println`.
   - Reserved slip words (`let`, `fn`, `def`, `var`, `const`, `function`, `elif`, `and`, `or`, `not`) produce diagnostics with fix hints.
   - No legacy compatibility mode.
4. **No inheritance or traits.** `inherits` and `trait` are removed.
5. **`type_inference.rs` is deleted**, not fixed. A non-panicking `check` pass is rebuilt later.
6. **Zero-dependency core crate.** Binding crates may add vetted dependencies under `DEPENDENCY_POLICY.md`.

Diagnostic code ranges: E01 lex, E02 parse, E03 check, E04 runtime, E05 budget, E06 capability, E07 host, E08 io/cli.

## Phases

### P0 — Agentic dev setup

Goal: one local check command, CI that mirrors it, and the rebase-merge workflow.

- Pin the Rust toolchain and add `justfile` check recipes (#23).
- Run `just` recipes in the pull-request workflow (#24).
- Pin oranda and scope web workflow permissions (#25).
- Lint every commit message and build every commit in a pull request (#26).
- Add Claude Code project configuration: `CLAUDE.md`, `.claude/` settings, agents, and commands (#27).
- Adopt the rebase-merge and self-contained commit policy across contributor docs (#28).
- Add issue templates (#29).
- Switch repository settings to rebase-merge only and require the `rust`, `docs`, `dist`, and `commit-lint` checks.

Release hygiene carried over from the previous roadmap:

- Migrate npm publishing to trusted publishing for `v-release.yml`, then revoke and remove `NPM_TOKEN`.
- Replace or rotate the Homebrew credential with a fine-grained, tap-only Contents-write token.
- Add platform code signing before the v0.2.0 release.

### P1 — Product repositioning

Goal: every top-level document describes the sandbox target truthfully.

- Rewrite this roadmap for the agent sandbox language (#30).
- Reposition `README.md` and `README.mm.md` for AI-agent scripting (#31).
- Align crate metadata, `lib.rs` docs, contributing and dependency policies, and `oranda.json` (#32).
- Mark the mdBook as legacy pending rewrite (#33).
- Specify the v0.2 language decisions and diagnostic code ranges in `docs/src/reference/language.md` (#34).

### P2 — Safety net

Goal: characterize current behavior before changing it. Stop for review after this phase.

- File one issue per audit defect, labelled `bug` plus an `area:` label.
- Split `tests/language_specs.rs` into per-component modules without changing test names.
- Add a zero-dependency conformance corpus harness (`tests/conformance/`) with expected stdout/error files, `.ignore` markers for known bugs, and `BLESS=1`.
- Add a deterministic never-panic harness for the lexer and parser with a known-crashers list.
- Characterize bindings and operators, control flow, functions and scoping, collections, structs, std modules, lexer and parser errors, and CLI exit codes.

### P3 — Refactor

Goal: a structure that can return typed errors everywhere.

- Delete `type_inference.rs` and parse-time inference.
- Delete dead code and the panicking parser wrappers.
- Make whitespace lexing iterative and check numeric literal ranges.
- Move sources into `syntax/` and `runtime/` modules.
- Add source spans and a `Diagnostic { code, kind, message, span, hint }` type.
- Introduce AST v2 with named structs and `BinOp`/`UnOp` enums.
- Frontend track: result-only parser with a nesting limit, new tokens, reserved-word hints, and the v0.2 syntax cutover with the corpus translated.
- Runtime track: checked arithmetic, propagated method errors, `Result`-based control flow replacing `catch_unwind`, lexical environments, ordered maps, and a lint wall against `unwrap`, `panic!`, and indexing.

### P4 — Bug fixes

Goal: the language behaves as specified.

- Short-circuit `&&` and `||`.
- Enforce annotated types and struct field types.
- Report a missing return correctly.
- Allow empty vectors.
- Define range semantics and count string length in characters.
- Display values deterministically.
- Implement `break`/`continue`, `null`/`??`, and `%`.
- Remove inheritance and traits.
- Fix the donut `sin_b` bug and the discarded `push`/`pop` results in `examples/app.poo`.

### P5 — Sandbox core

Goal: a host can embed PooLang with bounded, capability-gated execution.

- `Engine` builder with an output sink.
- Step, call-depth, memory, and deadline budgets plus an interrupt handle.
- Capability-gated `time` and `rand`.
- Host tools registered with `register_fn`.
- Determinism tests.
- JSON diagnostics.
- CLI: `poo run` and `poo check` with `--json`, `--allow`, and `--max-*` flags.
- `std::json`.
- `SECURITY.md` threat model.

### P6 — Revalidate

Goal: evidence that the reference, corpus, and sandbox agree.

- Coverage matrix mapping the language reference to corpus cases.
- Nightly never-panic run.
- Examples for agent use cases and a Rust embedding example.

### P7 — Docs truth

Goal: every documented example runs, and v0.2.0 is ready to release.

- `tests/doc_snippets.rs` runs `poo` code fences from the book and READMEs.
- Rewrite each book section with examples included from the corpus.
- A "PooLang for LLMs" cheat sheet, also printed by `poo --prompt`.
- A README/README.mm code-block sync test.
- Prepare the v0.2.0 release.

## Later

Not scheduled; each needs its own issue and design first.

- Static `check` pass (non-panicking type checker).
- MCP server.
- Python (PyO3) bindings.
- JavaScript/WASM bindings.
- Tree-sitter grammar and language server.
- Editor extensions.
- Package and module loading.

## Out of Scope

LLVM or native compilation, an async runtime, an HTTP/web framework, databases, gRPC/microservices, and garbage-collector work. PooLang is a sandbox: the host provides I/O through granted tools, so these belong to the host, not the language.

## Completed History

Completed under the previous roadmap (compiled backend platform, snapshot 2026-07-29).

### Phase 0 — Governance and branch baseline

- [x] Rename the local branch to `main`, push it, and set it as GitHub's default.
- [x] Add issue-first, small-PR, Conventional Commit, and squash-only rules to contributor and agent guidance. *Squash-only superseded on 2026-10-14 by the rebase-merge policy.*
- [x] Add a pull-request template and validate exactly one pre-existing issue reference.
- [x] Add a passing `cargo check --all-targets` pull-request baseline.
- [x] Protect `main`, require PR checks and resolved conversations, enforce rules for administrators, and block force pushes/deletion.
- [x] Disable merge commits and rebase merges; keep squash merge. *Superseded on 2026-10-14 by the rebase-merge policy.*
- [x] Delete remote `master` after `main`, workflows, default-branch state, and protection are verified.

### Phase 1 — Clean legacy baselines

- [x] Apply rustfmt and enforce `cargo fmt --all -- --check` in CI.
- [x] Fix the library doctest, make `cargo test` green, and require it in CI.
- [x] Resolve strict Clippy findings without suppressions and require `-D warnings` in CI.
- [x] Separate generated mdBook output from sources with one reproducible build command.
- [x] Reconcile README commands, branch names, CI claims, features, and examples with executable behavior.
- [x] Add focused lexer/parser error tests for malformed strings, comments, UTF-8 input, and unexpected EOF.
- [x] Audit release workflow actions, permissions, secrets, installers, and cargo-dist configuration.
- [x] Add dependency, license, and supply-chain checks.

The three open Phase 1 items (npm trusted publishing, Homebrew credential, code signing) moved to P0 release hygiene.

## Reference Sources

- Language Server Protocol: <https://microsoft.github.io/language-server-protocol/>
- VS Code language extensions: <https://code.visualstudio.com/api/language-extensions/overview>
- Zed language extensions: <https://zed.dev/docs/extensions/languages>
