<p align="center">
  <img width="500" height="281" alt="poo-banner-transparent" src="https://github.com/user-attachments/assets/70181432-f458-4949-beb2-51e6e9eb8549" />
</p>

<h1 align="center">PooLang</h1>

<p align="center">
  <a href="https://www.rust-lang.org">
    <img src="https://img.shields.io/badge/rust-1.75%2B-orange.svg?style=for-the-badge&logo=rust" alt="Rust" />
  </a>
  <a href="CONTRIBUTING.md">
    <img src="https://img.shields.io/badge/PRs-welcome-brightgreen.svg?style=for-the-badge" alt="PRs Welcome" />
  </a>
  <a href="LICENSE">
    <img src="https://img.shields.io/badge/license-MIT-blue.svg?style=for-the-badge" alt="License MIT" />
  </a>
</p>

<p align="center" class="oranda-hide">
  <b><a href="#features">Features</a></b> •
  <b><a href="#get-started">Get Started</a></b> •
  <b><a href="#installation">Installation</a></b> •
  <b><a href="#usage">Usage</a></b> •
  <b><a href="#syntax-overview">Syntax</a></b> •
  <b><a href="#example-code">Example</a></b> •
  <b><a href="#development">Dev</a></b>
</p>

<p align="center">
A Tiny Interpreted language written in Rust, featuring variable declarations, arithmetic operations, conditional statements, and control flow. The name <code>Poo</code> originates from Guinea Pig translated from <code>Burmese</code>.
</p>

## Features

- **Arithmetic Expressions**: Supports addition, subtraction, multiplication, and division with correct operator precedence.
- **Variable Declarations**: Uses `poo` keyword for variable declarations.
- **Mutable Variables**: Like in Rust, all variables are immutable by default. Uses `mut` for mutable variables.
- **Conditional Statements**: Includes `if`, `else`, and `elif` for branching.
- **Control Flow**: Supports `while` and `for in` loops and `return` statements.
- **Custom Operators**:
  - Assignment operator: `<<`
  - Arrow operator: `>>`
- **Lexer, Parser, and Interpreter**: A full pipeline from tokenizing source code to executing it.

## Get Started

For the best experience and automated installers for all platforms, visit our project site:

<p align="center">
  <b>👉 <a href="https://shayyz-code.github.io/poolang/">shayyz-code.github.io/poolang</a></b>
</p>

### Quick Install

**Linux / macOS**
```bash
curl --proto '=https' --tlsv1.2 -LsSf https://github.com/shayyz-code/poolang/releases/latest/download/poo-installer.sh | sh
```

**Windows**
```powershell
powershell -c "irm https://github.com/shayyz-code/poolang/releases/latest/download/poo-installer.ps1 | iex"
```

### Verify Installation

```bash
poo --help
```

## Installation

### Prerequisites

- Rust and Cargo installed. If you don't have them installed, follow the [Rust installation guide](https://www.rust-lang.org/tools/install).

### Steps

1. Clone the repository:

   ```bash
   git clone https://github.com/shayyz-code/poolang.git
   cd poolang
   ```

2. Build the project:

   ```bash
   cargo build
   ```

3. Run tests:
   ```bash
   cargo test
   ```

## Usage

You can use the interpreter to run files containing your custom language code.

### Running the Interpreter

To run the interpreter on a source file:

```bash
cargo run <path_to_your_source_file>
```

Example:

```bash
cargo run app.poo
```

The CLI uses typed checked execution (`run_file_checked`) and prints structured error kinds (`Io`, `Parse`, `Runtime`) with a non-zero exit code on failure.

## Release

Releases are automated with GitHub Actions + [cargo-dist](https://opensource.axo.dev/cargo-dist/) and publish:

- Multi-platform binaries (Linux, macOS, Windows)
- Optimized installers (Shell, PowerShell)
- Homebrew formula updates to `shayyz-code/tap` (`shayyz-code/homebrew-tap`)
- Scoop manifest updates to `shayyz-code/scoop-bucket`

Release flow:

1. Update version in `Cargo.toml`.
2. Create and push a version tag (example: `v0.1.5`).
3. The `Release` workflow builds all artifacts, creates a GitHub Release, and handles downstream publishing.

CI checks:

- Pull requests and pushes to `main` run build + tests on Linux, macOS, and Windows.
- Unix runners also validate installer script syntax (`sh -n install.sh`).

## Syntax Overview

The language features basic syntax for arithmetic, variable declarations, and control flow:

### **Variable Declarations**

```poo
poo x << 10;
poo mut y << 5 + 2 * 3;
```

### **Arithmetic Operations**

```poo
poo result << x + y * 2 - 10 / 2;
```

### **Conditional Statements**

```poo
if x > y {
    return x;
} else {
    return y;
}
```

### **Loops**

```poo
use std::pout;

poo mut count << 0;

while count < 10 {
    count << count + 1;
}

for i in 0..3 {
    pout("Hello, World ", i);
}
```

### **Functions**

```poo
use std::pout;

poof getName () >> string {
    poo name << "Shayy";
    return name;
}

pout(getName());
```

## Example Code

Here is a sample program in my PooLang:

```poo
use std::pout;

poo a << 5.0 * 1.0 - 1.0 * 3.0;
poo b << 2 / 2;
poo mut d << true;
d << false;

poof getHelloWorld () >> string {
    return "Hello, World!";
}

for i in 0..2 {
    pout("Hello, Poo!", i);
}

pout(getHelloWorld());
```

Expected Output:

```poo
Hello, Poo!0
Hello, Poo!1
Hello, World!
```

## Development

### TDD Specs

Current executable specs live in `tests/language_specs.rs`:

- `spec_lexer_skips_inline_comment_block`
- `spec_parser_respects_multiplication_precedence`
- `spec_interpreter_executes_program_to_return_value`
- `spec_checked_api_returns_typed_error_on_parse_failure`
- `spec_checked_api_returns_typed_error_on_runtime_failure`
- `spec_checked_file_api_returns_io_error_for_missing_file`
- `spec_checked_file_api_executes_valid_file`
- loop coverage (`for` range, `for` range with `step`, `for` vector, `while`)
- control-flow coverage (`if` / `elif` / `else`)
- struct coverage (instance methods, inheritance method lookup)

Run them with:

```bash
cargo test
```

### Refactor TODOs

- [x] Expose core modules as a reusable library API (`src/lib.rs`).
- [x] Keep CLI thin by delegating execution to library entrypoints.
- [x] Upgrade crate to Rust Edition 2024.
- [x] Introduce checked execution APIs with typed error kinds (`Io`, `Parse`, `Runtime`).
- [ ] Replace panic-driven parser/interpreter internals with native `Result` propagation.
- [ ] Split large parser and interpreter files into focused submodules.
- [x] Add integration specs for structs, methods, inheritance, and loops.

### Project Structure

```
.
├── src
│   ├── lib.rs           # Reusable library API
│   ├── lexer.rs         # Lexical analysis (tokenizer)
│   ├── parser.rs        # Parsing logic
│   ├── interpreter.rs   # Interpreter for executing code
│   ├── ast.rs           # Abstract Syntax Tree (AST) definitions
│   ├── errors.rs        # Typed error definitions
│   └── main.rs          # Entry point
├── examples             # Sample code
│   ├── donut.poo
│   └── app.poo
├── tests
│   └── language_specs.rs # TDD integration specs
└── Cargo.toml           # Project configuration
```

## Contributing

Contributions are welcome! Please see [CONTRIBUTING.md](CONTRIBUTING.md) for guidelines.

## License

PooLang is licensed under the MIT License. See [LICENSE](LICENSE) for details.

<p align="center">Hand-Crafted by <em><b><a href="https://github.com/shayyz-code">Shayy</a></b></em></p>
