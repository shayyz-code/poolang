# Contributing to PooLang

Thank you for helping build PooLang. PooLang is becoming a sandboxed scripting language for AI agents: LLM agents write it, and a host program runs it with only the capabilities it grants, under resource budgets, with deterministic results and readable diagnostics. Keep contributions focused, reproducible, and aligned with the roadmap in `TODO.md`.

## Code of Conduct

By participating in this project, you agree to abide by the standard open-source community terms: be respectful, inclusive, and professional.

## How Can I Contribute?

### Reporting Bugs
- Use the [GitHub Issues](https://github.com/shayyz-code/poolang/issues) tab.
- Describe the expected behavior and the actual behavior.
- Provide a minimal reproduction script (a `.poo` file) if possible.

### Suggesting Features
- Open an issue with the **Feature request** [issue form](https://github.com/shayyz-code/poolang/issues/new/choose), which applies the `enhancement` label. Use the **Language change** form for syntax or semantics proposals.
- Explain why this feature would be useful for PooLang.
- Explain how it supports the sandboxed agent-scripting roadmap in `TODO.md`.

### Pull Requests

1. Open an issue before writing code. Agree on the problem and intended outcome there.
2. Fork the repository and create a branch from `main`.
3. Keep the PR small: address one issue and one independently reviewable concern.
4. Follow TDD for features and fixes by adding a focused test first.
5. Include exactly one closing reference in the PR body, such as `Closes #123`.
6. Describe verification and note documentation, command-output, or screenshot impact.
7. Write every commit, not just the PR title, as an imperative [Conventional Commit](https://www.conventionalcommits.org/) with a header of at most 72 characters, such as `feat: add modulo operator` or `fix(parser): report missing semicolon`.

Pull requests are rebase-merged only, so each commit on your branch lands on `main` as written. Squash merges and merge commits are disabled, and a branch must be up to date with `main` before it merges.

Every commit must be self-contained and pass `just check` on its own; CI lints each commit message and builds and tests each commit (`cargo check` and `cargo test`). Before requesting review, sync your fork's branch with upstream `main` and fold follow-up fixes into the commits they correct:

```bash
git remote add upstream https://github.com/shayyz-code/poolang.git  # once
git fetch upstream
git commit --fixup <sha>
git rebase -i --autosquash upstream/main
git push --force-with-lease
```

Force-push only your own branch.

## Development Setup

### Prerequisites
- [Rust](https://www.rust-lang.org/tools/install) via rustup. The crate uses the 2024 edition, and `rust-toolchain.toml` pins Rust 1.97.1 with rustfmt and Clippy, so rustup installs the right toolchain automatically.
- [just 1.58.0](https://just.systems/): `cargo install --locked just --version 1.58.0`
- [mdBook 0.5.2](https://rust-lang.github.io/mdBook/guide/installation.html): `cargo install mdbook --locked --version 0.5.2`
- [cargo-deny 0.20.2](https://embarkstudios.github.io/cargo-deny/): `cargo install --locked cargo-deny --version 0.20.2`
- [cargo-dist 0.31.0](https://axodotdev.github.io/cargo-dist/) (the `dist` command): see its [installation guide](https://axodotdev.github.io/cargo-dist/book/install.html)
- [oranda 0.6.5](https://axodotdev.github.io/oranda/) for project site builds only (pinned in `.github/workflows/web.yml`)

Run `just tools` to compare installed versions against the pinned ones.

### Workflow
1. **Clone your fork and enable the commit-msg hook**:
   ```bash
   git clone https://github.com/YOUR_USERNAME/poolang.git
   cd poolang
   just setup
   ```
2. **Run a sample program**:
   ```bash
   cargo run -- examples/app.poo
   ```
3. **Run the checks**:
   ```bash
   just check-fast  # formatting, compilation, Clippy, and tests
   just check       # everything required before review
   ```

`just check` adds the mdBook build and tests, the release workflow check, and `cargo deny check` to `just check-fast`; all of it must pass before review. Run `just` to list the individual recipes. `mdbook build docs` is the canonical documentation build command.

Dependency changes must follow `DEPENDENCY_POLICY.md`, update `Cargo.lock`, and pass the advisory, license, ban, and source checks without undocumented exceptions.

## Release Changes

Configure releases in `dist-workspace.toml`; `.github/workflows/v-release.yml` is generated and must not be edited by hand. Run `dist generate --mode=ci` after changing release configuration and `dist generate --check` before review. Maintainers must follow `RELEASING.md`; release tags are reserved for reviewed release commits on `main`.

## Architectural Overview

- **Lexer (`src/lexer.rs`)**: Converts source text into a stream of tokens.
- **Parser (`src/parser.rs`)**: Consumes tokens and builds an Abstract Syntax Tree (AST).
- **Interpreter (`src/interpreter.rs`)**: Walks the AST and executes the logic.
- **AST (`src/ast.rs`)**: Defines the structure of the language.

## Style Guide

- **Naming**: We use standard Rust naming conventions (`snake_case` for variables/functions, `PascalCase` for types).
- **Terminology**: The name `Poo` comes from the Burmese word for Guinea Pig. Feel free to use Guinea Pig related puns in your documentation!

## Feedback
If you have questions, feel free to reach out via GitHub issues. Happy coding!
