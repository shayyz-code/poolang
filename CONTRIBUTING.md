# Contributing to PooLang

First off, thank you for considering contributing to PooLang! It's people like you that make PooLang such a fun project to build.

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
- Note that PooLang aims to stay "tiny" and "minimalist," so we prioritize features that align with that philosophy.

### Pull Requests
1. **Fork the repo** and create your branch from `main`.
2. **Follow the TDD approach**: If you're adding a feature or fixing a bug, add a test case to `tests/language_specs.rs` first.
3. **Ensure tests pass**: Run `cargo test` to verify your changes.
4. **Format your code**: Use `cargo fmt` to keep the codebase clean.
5. **Write a good commit message**: We prefer [Conventional Commits](https://www.conventionalcommits.org/) (e.g., `feat: add support for modulo operator`).

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
   cargo build
   cargo run examples/app.poo
   ```
3. **Run Specs**:
   ```bash
   cargo test
   ```

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
