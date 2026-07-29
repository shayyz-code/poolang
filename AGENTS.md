# Repository Guidelines

## Project Structure & Module Organization

PooLang is currently a Rust 2024 interpreter. Core code lives in `src/`: the lexer tokenizes source, the parser builds the AST, and the interpreter executes it. `lib.rs` exposes library entrypoints and `main.rs` provides the CLI. Executable language specifications are in `tests/language_specs.rs`; sample `.poo` programs belong in `examples/`.

Edit documentation sources under `docs/src/` and navigation in `docs/src/SUMMARY.md`; treat `docs/book/` as generated output. Release and site configuration lives in `dist-workspace.toml`, `oranda.json`, `wix/`, and `.github/workflows/`.

## Build, Test, and Development Commands

- `cargo check --all-targets` performs the required baseline compile check.
- `cargo build` compiles the debug binary and library.
- `cargo run -- examples/app.poo` runs a sample program.
- `cargo test` runs unit, integration, and documentation tests.
- `cargo fmt --all -- --check` checks formatting; `cargo fmt --all` applies it.
- `cargo clippy --all-targets --all-features -- -D warnings` runs strict linting.
- `mdbook build docs` is the canonical documentation build; `mdbook test docs` tests its Rust examples.

All established formatting, compilation, test, and lint checks must pass before review.

## Coding Style & Testing

Use rustfmt defaults and idiomatic Rust names: `snake_case` for functions and modules, `PascalCase` for types and traits, and `SCREAMING_SNAKE_CASE` for constants. Keep subsystem boundaries clear and return actionable typed errors instead of adding panic-driven paths.

Follow TDD for features and fixes. Add focused cases to `tests/language_specs.rs`, named `spec_<component>_<behavior>`, and cover success plus relevant failure paths.

## Issue, Commit & Pull Request Rules

Every change starts with an existing GitHub issue. Create a branch from `main` and keep the PR limited to one issue and one reviewable concern. The PR body must contain exactly one closing reference such as `Closes #123`, describe verification, and note documentation or screenshot impact.

Use imperative Conventional Commits such as `feat: add route groups`, `fix(parser): report missing delimiter`, or `docs: clarify variables`. Do not merge directly, create merge commits, or rebase-merge PRs. Repository changes land through squash merge only. Agents must respect the same issue-first and small-PR workflow.
