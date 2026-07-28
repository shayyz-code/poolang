# Contributing to PooLang

Thank you for helping build PooLang. Keep contributions focused, reproducible, and aligned with the project's backend-language roadmap.

## Code of Conduct

By participating in this project, you agree to abide by the standard open-source community terms: be respectful, inclusive, and professional.

## How Can I Contribute?

### Reporting Bugs
- Use the [GitHub Issues](https://github.com/shayyz-code/poolang/issues) tab.
- Describe the expected behavior and the actual behavior.
- Provide a minimal reproduction script (a `.poo` file) if possible.

### Suggesting Features
- Open an issue with the "feature request" tag.
- Explain why this feature would be useful for PooLang.
- Explain how it supports the compiled, backend-focused language and tooling roadmap in `TODO.md`.

### Pull Requests

1. Open an issue before writing code. Agree on the problem and intended outcome there.
2. Fork the repository and create a branch from `main`.
3. Keep the PR small: address one issue and one independently reviewable concern.
4. Follow TDD for features and fixes by adding a focused test first.
5. Include exactly one closing reference in the PR body, such as `Closes #123`.
6. Describe verification and note documentation, command-output, or screenshot impact.
7. Use an imperative [Conventional Commit](https://www.conventionalcommits.org/) title, such as `feat: add modulo operator` or `fix(parser): report missing semicolon`.

Pull requests are squash-merged only. Do not use merge commits or rebase merges.

## Development Setup

### Prerequisites
- [Rust](https://www.rust-lang.org/tools/install) (Edition 2024 or later)
- [Cargo](https://doc.rust-lang.org/cargo/)

### Workflow
1. **Clone your fork**:
   ```bash
   git clone https://github.com/YOUR_USERNAME/poolang.git
   cd poolang
   ```
2. **Build and Run**:
   ```bash
   cargo check --all-targets
   cargo run -- examples/app.poo
   ```
3. **Run Specs**:
   ```bash
   cargo test
   ```

Before review, also run `cargo fmt --all -- --check` and `cargo clippy --all-targets --all-features -- -D warnings`. Known legacy failures are recorded in `TODO.md`; do not add new failures while cleanup issues are open.

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
