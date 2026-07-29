# Dependency Policy

PooLang keeps its dependency surface small, reproducible, and suitable for a compiled backend platform. Every dependency change follows the normal issue-first, focused pull-request workflow and must pass `cargo deny check`.

## Rust Dependencies

- Prefer maintained crates.io releases with the narrowest practical compatible SemVer requirement. Wildcards and unreviewed registries or Git sources are denied.
- Commit `Cargo.lock` changes with the manifest change. Review newly resolved transitive dependencies, enabled features, build scripts, and platform-specific code.
- An unavoidable Git dependency requires an explicit source exception in `deny.toml`, a full `rev` commit pin, and an issue explaining why no registry release is suitable.
- Globally accepted licenses are MIT, Apache-2.0, Apache-2.0 with LLVM exception, ISC, Unicode-3.0, and Zlib. Add other licenses only as package-and-version-specific exceptions with documented review.
- An advisory ignore must be temporary. Its reason must name the tracking issue, mitigation, owner, and expiry date; remove it as soon as a fixed dependency is available.

Install the CI-pinned policy tool with `cargo install --locked cargo-deny --version 0.20.2`, then run `cargo deny check` before review.

## Native Dependency Inventory

No native libraries are currently linked or shipped.

Before adding LLVM, Boehm GC, or another native component, record its upstream source, pinned version or commit, archive checksum or verified signature, SPDX license, static or dynamic linkage, supported targets, vulnerability advisory source, and update owner in this section. Preserve required license and notice files in source and release artifacts. The adoption issue must also explain how clean and reproducible builds obtain the dependency.

## Vulnerability Response

GitHub Dependabot alerts and the scheduled supply-chain workflow provide notification; dependency PRs are not automated. Triage each alert through a new issue, determine affected shipped versions, and submit the smallest compatible update or mitigation through normal review.
