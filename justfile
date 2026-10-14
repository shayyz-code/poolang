# PooLang development tasks. Run `just` to list recipes.
#
# The Rust toolchain is pinned in rust-toolchain.toml. The versions below are
# the external tools the checks were verified with; `just tools` compares them
# against what is installed.

mdbook_version := "0.5.2"
cargo_deny_version := "0.20.2"
dist_version := "0.31.0"

# List available recipes
default:
    @just --list

# Format all Rust sources
fmt:
    cargo fmt --all

# Check formatting without changing files
fmt-check:
    cargo fmt --all -- --check

# Compile every target
build-check:
    cargo check --all-targets

# Run Clippy with warnings denied
lint:
    cargo clippy --all-targets --all-features -- -D warnings

# Run unit, integration, and documentation tests
test:
    cargo test

# Build the book into a scratch directory and test its Rust examples
docs:
    #!/usr/bin/env bash
    set -euo pipefail
    out="$(mktemp -d)"
    trap 'rm -rf "$out"' EXIT
    mdbook build docs --dest-dir "$out"
    mdbook test docs

# Verify the generated release workflow matches dist-workspace.toml
dist-check:
    dist generate --check

# Enforce the dependency policy
deny:
    cargo deny check

# Run the quick local checks
check-fast: fmt-check build-check lint test

# Run every check required before review
check: check-fast docs dist-check deny

# Point git at the repository hooks
setup:
    git config core.hooksPath .githooks

# Show installed tool versions next to the pinned ones
tools:
    #!/usr/bin/env bash
    set -uo pipefail
    show() {
        local name="$1" pinned="$2"
        shift 2
        local installed
        installed="$("$@" 2>/dev/null | head -n 1)" || installed=""
        printf '%-12s pinned %-10s installed %s\n' "$name" "$pinned" "${installed:-not found}"
    }
    show rustc "$(sed -n 's/^channel = "\(.*\)"/\1/p' rust-toolchain.toml)" rustc --version
    show mdbook "{{ mdbook_version }}" mdbook --version
    show cargo-deny "{{ cargo_deny_version }}" cargo deny --version
    show dist "{{ dist_version }}" dist --version
