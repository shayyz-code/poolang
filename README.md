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

PooLang is becoming a sandboxed scripting language for AI agents: an LLM agent
writes a PooLang script, and a host program runs it safely. From v0.2, a script
will be able to call only the capabilities and tools the host grants, will run
within budgets the host sets, and will report errors as diagnostics the agent
can read and fix.
The name **Poo (ပူး)** comes from the Burmese word for guinea pig.

> [!IMPORTANT]
> **Status:** v0.1.11 is a small tree-walking interpreter; the v0.2 agent
> sandbox is in progress — see the
> [roadmap (TODO.md)](https://github.com/shayyz-code/poolang/blob/main/TODO.md)
> and [milestones](https://github.com/shayyz-code/poolang/milestones).

## Why PooLang

Agents increasingly solve tasks by writing code, and a host has to run code it
did not write and cannot fully trust. General-purpose languages start with
access to files, the network, and the clock, and then have to be locked down.
PooLang v0.2 is being built the other way around. None of this exists in
v0.1.11 yet. Goals for v0.2:

- **Zero ambient authority.** A script will be able to call only the
  capabilities and tools the host grants it. Nothing else will be reachable.
- **Budgets.** The host will limit steps, call depth, memory, and wall-clock
  time, so a runaway loop will end with an error instead of stalling the agent.
- **Determinism.** The same script with the same inputs and tool results will
  give the same output, so runs can be replayed and tested.
- **Never panics.** Every input will end in a value or a typed error, never a
  crash of the host.
- **Diagnostics an LLM can act on.** Each error will carry a stable code, a
  source span, and a fix hint, and will be available as JSON, so the agent can
  repair its own script.

The first host interface will be a Rust embedding API. Python and WASM bindings
and an MCP server will follow later.

## Project Status

| Track | Availability | Purpose |
| --- | --- | --- |
| v0.1.11 | Available | Small Rust tree-walking interpreter with legacy syntax, executable examples, and checked library APIs; no sandbox controls |
| v0.2 | In progress | Agent sandbox: conventional syntax, host-granted capabilities and tools, budgets, deterministic execution, JSON diagnostics, Rust embedding API, and `poo run`/`poo check` |
| Later | Not scheduled | Static checker, Python and WASM bindings, MCP server, and editor support |

There is no compatibility promise between the legacy syntax below and v0.2,
and v0.2 will not include a legacy mode. See
[TODO.md](https://github.com/shayyz-code/poolang/blob/main/TODO.md) for the
locked decisions and phases, and the
[milestones](https://github.com/shayyz-code/poolang/milestones) for progress.

## v0.2 Preview

> [!NOTE]
> This syntax is **planned, not yet implemented**. v0.1.11 rejects it.

```text
use tools::search;            // host-granted tool
poo limit = 10;
poo mut hits: vec[str] = [];
for doc in search("rust sandbox", limit) {
    if doc["score"] >= 0.5 && doc["title"] != "" { hits.push(doc["title"]); }
}
println(hits.len());
```

In v0.2, `search` will exist only if the host registers it as a tool. Without
that grant, the script is rejected before it runs with a capability (E06)
diagnostic.

## Current v0.1.11 Features

- Integer, float, Boolean, character, string, vector, and map values.
- Immutable inferred or typed declarations and mutable bindings.
- Arithmetic, comparison, `and`, `or`, `not`, and `otherwise` expressions.
- `if`/`elif`/`else`, `while`, range/vector `for`, `step`, and `return`.
- Typed functions, structs, instance methods, and legacy struct inheritance.
- Built-in `std`, `math`, and `time` modules.
- Checked source/file APIs with `Io`, `Parse`, and `Runtime` errors.
- A Rust lexer, parser, type-inference pass, and AST-walking interpreter.

Traits are tokenized but are not a complete supported feature. v0.1.11 has none
of the sandbox controls described above: no capability gating, budgets, or JSON
diagnostics. Some inputs still crash the interpreter, so do not use it to run
untrusted code. Known defects are listed in the roadmap's
[current audit](https://github.com/shayyz-code/poolang/blob/main/TODO.md#current-audit).

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
- A commit policy check: Conventional Commit headers, plus Cargo check and
  tests on each commit.
- Rustfmt, Cargo check, strict Clippy, and the complete Cargo test suite on Ubuntu.
- The mdBook build and tests, and the Oranda/mdBook site build and internal-link check.
- A check that the generated release workflow matches `dist-workspace.toml`.
- `cargo deny check` for the dependency policy.
- A cargo-dist release-plan run (`dist plan`).

Pushes to `main` rebuild and deploy the project site. Platform release builds
run for version tags, not for every pull request.

## Development

```bash
just setup       # enable the commit-msg hook (once per clone)
just check-fast  # formatting, compilation, Clippy, and tests
just check       # every check required before review
```

`just check` also builds and tests the mdBook documentation, verifies the
generated release workflow, and runs `cargo deny check`. Required tools and
versions are listed in [CONTRIBUTING.md](CONTRIBUTING.md).

Core Rust code lives in `src/`, executable language specifications in
`tests/language_specs/`, sample programs in `examples/`, and documentation
sources in `docs/src/`. Generated `docs/book/` and `public/` output is not
tracked.

Every change starts with a GitHub issue and a branch from `main`. Pull requests
must remain small, contain exactly one closing issue reference, and are
rebase-merged only. Every commit lands on `main` as written, so each one must be
a self-contained Conventional Commit that passes `just check`. See
[CONTRIBUTING.md](CONTRIBUTING.md).

## License

PooLang is licensed under the [MIT License](LICENSE).

<p align="center">Hand-crafted by <a href="https://github.com/shayyz-code"><b>Shayy</b></a></p>
