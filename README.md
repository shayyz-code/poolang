<p align="center">
  <img width="500" height="281" alt="PooLang banner" src="https://github.com/user-attachments/assets/70181432-f458-4949-beb2-51e6e9eb8549" />
</p>

<h1 align="center">PooLang</h1>

<p align="center">
  <a href="https://www.rust-lang.org">
    <img src="https://img.shields.io/badge/Rust-stable-orange.svg?style=for-the-badge&logo=rust" alt="Rust stable" />
  </a>
  <a href="https://github.com/shayyz-code/poolang/releases/tag/v0.1.11">
    <img src="https://img.shields.io/badge/release-v0.1.11-blue.svg?style=for-the-badge" alt="Release v0.1.11" />
  </a>
  <a href="LICENSE">
    <img src="https://img.shields.io/badge/license-MIT-blue.svg?style=for-the-badge" alt="MIT License" />
  </a>
</p>

<p align="center">
  <a href="https://github.com/shayyz-code/poolang#readme">English</a> |
  <a href="https://github.com/shayyz-code/poolang/blob/main/README.mm.md">မြန်မာ</a>
</p>

PooLang is being designed as a simple, compiled language for web backends and
infrastructure. Its direction combines Go-like deployment, approachable syntax,
and Rust-inspired explicitness without exposing ownership or lifetime syntax.
The name **Poo (ပူး)** comes from the Burmese word for guinea pig.

> [!IMPORTANT]
> The current release, **v0.1.11**, is a legacy tree-walking interpreter.
> The compiled backend platform described in the roadmap is planned for v0.2
> and is not implemented yet.

## Project Status

| Track | Availability | Purpose |
| --- | --- | --- |
| v0.1.11 | Available | Legacy Rust interpreter, executable examples, and checked library APIs |
| v0.2 | Planned | Clean-break LLVM compiler, native runtime, package tooling, async HTTP/JSON, analyzer, and editor support |
| v0.3 | Planned | Opinionated web application platform with data, auth, cache, queues, and framework tooling |
| v0.4 | Planned | Protobuf/gRPC, microservices, resilience, discovery, and OpenTelemetry |

There is no compatibility promise between the legacy syntax below and v0.2.
See [TODO.md](https://github.com/shayyz-code/poolang/blob/main/TODO.md) for the decision record, milestones, and implementation
gates.

## Current v0.1.11 Features

- Integer, float, Boolean, character, string, vector, and map values.
- Immutable inferred or typed declarations and mutable bindings.
- Arithmetic, comparison, `and`, `or`, `not`, and `otherwise` expressions.
- `if`/`elif`/`else`, `while`, range/vector `for`, `step`, and `return`.
- Typed functions, structs, instance methods, and legacy struct inheritance.
- Built-in `std`, `math`, and `time` modules.
- Checked source/file APIs with `Io`, `Parse`, and `Runtime` errors.
- A Rust lexer, parser, type-inference pass, and AST-walking interpreter.

Traits are tokenized but are not a complete supported feature. PooLang does not
currently provide native compilation, HTTP, JSON, databases, packages, gRPC, or
an editor analyzer.

## Installation

Current v0.1.11 packages and installers are published for macOS, Linux, and
Windows.

### Homebrew

```bash
brew install shayyz-code/tap/poo
```

### npm

```bash
npm install --global @shayyz-code/poo@latest
```

### Linux or macOS installer

```bash
curl --proto '=https' --tlsv1.2 -LsSf https://github.com/shayyz-code/poolang/releases/latest/download/poo-installer.sh | sh
```

### Windows PowerShell installer

```powershell
powershell -ExecutionPolicy Bypass -c "irm https://github.com/shayyz-code/poolang/releases/latest/download/poo-installer.ps1 | iex"
```

### Build from source

Install a stable Rust toolchain, then run:

```bash
git clone https://github.com/shayyz-code/poolang.git
cd poolang
cargo install --path .
```

## Running Programs

The v0.1 CLI accepts a Poo source file path:

```bash
poo path/to/program.poo
```

From a repository checkout:

```bash
cargo run -- examples/app.poo
```

Append `--speed` after the source path to print interpreter timing:

```bash
poo path/to/program.poo --speed
```

Successful top-level returns are printed as `Result: <Value>`. File, parse, and
runtime failures are written to stderr with a non-zero exit status. The legacy
CLI does not currently implement `--help` or `--version`.

## Legacy v0.1.11 Syntax

This example is executable with the current interpreter:

```poo
use std::poutln;

poo message str = "ready";
mut total <: 0;

for i in 0..5 {
    total = total + i;
}

poof double(value int) >> int {
    return value * 2;
}

if (not false and total > 5) or message == "never" {
    poutln(double(total));
}

return total;
```

Expected output:

```text
20
Result: Int(10)
```

Legacy declarations use `<:` for inferred types or `name type = value` for
explicit types. Mutable declarations begin with `mut`; reassignment uses
standard `=`. Functions use `poof`, typed parameters such as `value int`,
and `>>` for return types.

## Examples

`examples/app.poo` exercises collections, control flow, functions, structs,
methods, modules, and inheritance:

```bash
cargo run -- examples/app.poo
```

`examples/donut.poo` renders a continuously animated terminal donut. Run it
in a terminal and stop it with <kbd>Ctrl</kbd>+<kbd>C</kbd>:

```bash
cargo run --release -- examples/donut.poo
```

Both examples target the legacy v0.1.11 interpreter.

## Releases and CI

SemVer tags drive the cargo-dist `Release` workflow. The current configuration
produces checksummed archives for Apple Silicon and Intel macOS, ARM64 and
x86-64 Linux, and x86-64 Windows, plus shell, PowerShell, npm, Homebrew, and MSI
installers. Stable releases publish npm packages and Homebrew formula updates.

Pull requests currently run:

- The issue-link policy check.
- Rustfmt, Cargo check, strict Clippy, and the complete Cargo test suite on Ubuntu.
- The Oranda/mdBook site build and internal-link check.
- A cargo-dist release-plan validation.

Pushes to `main` rebuild and deploy the project site. Platform release builds
run for version tags, not for every pull request.

## Development

```bash
cargo build
cargo fmt --all -- --check
cargo check --all-targets
cargo clippy --all-targets --all-features -- -D warnings
cargo test
mdbook build docs
mdbook test docs
```

Core Rust code lives in `src/`, executable language specifications in
`tests/language_specs.rs`, sample programs in `examples/`, and documentation
sources in `docs/src/`. Generated `docs/book/` and `public/` output is not
tracked.

Every change starts with a GitHub issue and a branch from `main`. Pull requests
must remain small, contain exactly one closing issue reference, use Conventional
Commits, and are squash-merged only. See [CONTRIBUTING.md](CONTRIBUTING.md).

## License

PooLang is licensed under the [MIT License](LICENSE).

<p align="center">Hand-crafted by <a href="https://github.com/shayyz-code"><b>Shayy</b></a></p>
