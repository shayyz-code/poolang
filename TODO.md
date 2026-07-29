# PooLang Roadmap

This document is the issue source for PooLang's clean-break transition from the v0.1.11 tree-walking interpreter to a compiled, batteries-included backend platform. Every unchecked item must become a GitHub issue before implementation. Keep each issue and pull request focused on one independently reviewable outcome.

## Current Audit

Snapshot: 2026-07-29.

### What works

- `cargo fmt --all -- --check` passes and is enforced for pull requests.
- `cargo check --all-targets` passes.
- `cargo test` passes and is enforced for pull requests, including all 58 integration specifications and the library doctest.
- `cargo clippy --all-targets --all-features -- -D warnings` passes and is enforced for pull requests.
- `mdbook build docs` reproducibly generates ignored documentation output from `docs/src/`.
- English and Burmese READMEs distinguish the available v0.1.11 interpreter from the planned compiled backend platform.
- Both shipped examples execute against the v0.1.11 interpreter; the donut runs until interrupted.
- The crate exposes checked file/source execution APIs and typed I/O, parse, and runtime error categories.
- Checked parsing reports malformed literals, comments, non-ASCII identifiers, and unexpected EOF without panicking or hanging.
- Lexer, parser, interpreter, type-inference, examples, mdBook documentation, and cargo-dist release assets exist.
- cargo-dist 0.31.0 generates five platform targets and shell, PowerShell, npm, Homebrew, and MSI installers from reviewed configuration.
- Release actions use immutable commit pins, version tags are protected, and future release artifacts receive GitHub provenance attestations.
- Rust dependencies are gated by advisory, license, version, and source policy checks across all supported targets.
- Dependabot vulnerability alerts and a weekly supply-chain check monitor the currently dependency-free crate graph.

### Quality gaps

- Panic recovery wraps rather than removes many panic, `unwrap`, and `expect` paths; the interpreter alone contains roughly 90.
- `parser.rs` and `interpreter.rs` are approximately 975 and 1,217 lines and mix several responsibilities.
- Checked and unchecked parsing/execution paths duplicate logic.
- Tokens and AST nodes do not retain source spans, preventing precise diagnostics and editor features.
- Type inference is partial, panic-driven, and inconsistent for collections, calls, maps, and structs.

### Syntax and documentation drift

- Documentation advertises `<<`, while tested inferred declarations use `<:` and explicit declarations/reassignments use `=`.
- Documentation uses `poo mut`, `string`, `&&`, `||`, `#` comments, `>=`, and `<=`; the lexer currently recognizes top-level `mut`, `str`, `and`, `or`, unusual `// ... //` comments, and no inclusive comparisons.
- Function examples omit required parameter types or use return/type spelling that differs from the parser.
- `trait` is tokenized but not supported as a complete tested language feature.

### Missing platform capabilities

- There is no native compiler, typed intermediate representation, optimizer pipeline, linker integration, runtime ABI, package resolver, formatter, language server, editor grammar, or supported web framework.
- Imports select hard-coded interpreter modules rather than loading packages from a module graph.
- The standard library is embedded in the interpreter and cannot evolve independently.
- There are no HTTP, JSON, database, Redis, queue, gRPC, observability, or microservice contracts.

## Locked Product Decisions

- v0.1.11 remains the legacy interpreter anchor. v0.2 is a clean break with no syntax compatibility mode.
- Poo compiles ahead of time through LLVM to a native Linux x86_64 executable.
- The Poo runtime and Boehm GC are statically linked; LLVM/codegen libraries are not loaded by editor tooling.
- Memory is garbage-collected. User-visible ownership, borrowing, and lifetimes are out of scope.
- Executable statements are allowed at the top level of `src/main.poo`; library modules expose declarations.
- Statements require semicolons. A final expression without a semicolon is the containing block's value; explicit `return` supports early exit.
- Canonical declarations use `poo name = value;` and `poo mut count = 0;`. Functions use `poof add(a: i64, b: i64): i64 { ... }`.
- Core names are `i64`, `f64`, `bool`, `char`, `str`, `vec[T]`, `map[K, V]`, tuples, `error`, and explicit `T?`/`nil`.
- Fallible APIs return multiple values such as `(User?, error?)`; callers explicitly destructure and check the error.
- Use standard `=`, `==`, `!=`, `<`, `<=`, `>`, `>=`, `&&`, `||`, `!`, `else if`, `//`, and `/* ... */`.
- Use Rust-like module paths and private-by-default exports. Prefer structs, traits, and composition; remove inheritance.
- v0.2 supports typed built-in containers but not user-defined generics.
- `async poof` and `await` are required for the v0.2 HTTP milestone.
- Framework suggestions and refactors are deterministic, local, explainable, and telemetry-free.

## Phase 0 — Governance and Branch Baseline

- [x] Rename the local branch to `main`, push it, and set it as GitHub's default.
- [x] Add issue-first, small-PR, Conventional Commit, and squash-only rules to contributor and agent guidance.
- [x] Add a pull-request template and validate exactly one pre-existing issue reference.
- [x] Add a passing `cargo check --all-targets` pull-request baseline.
- [x] Protect `main`, require PR checks and resolved conversations, enforce rules for administrators, and block force pushes/deletion.
- [x] Disable merge commits and rebase merges; keep squash merge and automatic source-branch deletion.
- [x] Delete remote `master` after `main`, workflows, default-branch state, and protection are verified.

## Phase 1 — Clean Legacy Baselines

- [x] Open and complete an issue that applies rustfmt, adds `cargo fmt --all -- --check` to CI, and changes no behavior.
- [x] Fix the library doctest against the v0.1 API, make `cargo test` green, and require it in CI.
- [x] Resolve strict Clippy findings in the interpreter without lint suppressions or behavior changes.
- [x] Resolve the remaining strict Clippy findings in the lexer, parser, type inference, and symbol table.
- [x] Require `cargo clippy --all-targets --all-features -- -D warnings` in pull-request CI.
- [x] Separate generated mdBook output from sources and define one reproducible documentation build command.
- [x] Reconcile README commands, branch names, CI claims, supported features, and examples with executable behavior.
- [x] Add focused lexer/parser error tests for malformed strings, comments, UTF-8 input, and unexpected EOF.
- [x] Audit release workflow actions, permissions, secrets, installers, and generated cargo-dist configuration.
- [x] Add dependency, license, and supply-chain checks appropriate to Rust and future native runtime dependencies.
- [ ] Migrate npm publishing to trusted publishing for `v-release.yml`, then revoke and remove `NPM_TOKEN`.
- [ ] Replace or rotate the Homebrew credential with a fine-grained, tap-only Contents-write token.
- [ ] Add platform code signing before the production v0.2 release.

## Phase 2 — Normative v0.2 Language Specification

- [ ] Write a lexical grammar covering identifiers, literals, comments, punctuation, operators, keywords, and reserved words.
- [ ] Write statement, expression, precedence, block-value, and semicolon grammar with valid and invalid examples.
- [ ] Specify bindings, immutability, shadowing, scope, initialization, assignment, and destructuring.
- [ ] Specify primitive, tuple, optional, collection, struct, trait, function, async-task, and error types.
- [ ] Specify functions, multiple returns, early return, tail expressions, and error conventions.
- [ ] Specify `if` expressions, loops, ranges, iteration, pattern/destructuring behavior, and control-flow exits.
- [ ] Specify structs, methods, traits, visibility, composition, and dispatch without inheritance.
- [ ] Specify modules, imports, package boundaries, entrypoints, initialization order, and cyclic-import errors.
- [ ] Specify `async poof`, `await`, cancellation, task lifetime, thread safety, and blocking-call rules.
- [ ] Specify source encoding, integer overflow, floating-point behavior, string representation, equality, and deterministic map guarantees.
- [ ] Publish a shared valid/invalid conformance corpus used by compiler, analyzer, tree-sitter, documentation, and formatters.

## Phase 3 — Compiler Workspace and Frontend

- [ ] Convert the repository into workspace crates for `poo-syntax`, `poo-analysis`, `poo-compiler`, `poo-runtime`, `poo-analyzer`, and `poo-cli`.
- [ ] Build an immutable source database with file IDs, UTF-8-safe offsets, line indexes, spans, and edit revisions.
- [ ] Replace the lexer with a non-panicking, span-aware tokenizer and structured lexical diagnostics.
- [ ] Build a lossless, error-tolerant concrete syntax tree with stable node identities and incremental reparse boundaries.
- [ ] Lower the concrete tree into a compact AST/HIR while retaining source mappings.
- [ ] Build module discovery, import resolution, visibility checks, and deterministic dependency graphs.
- [ ] Build scope/name resolution with duplicate, unresolved, shadowing, and privacy diagnostics.
- [ ] Implement type inference/checking for all locked v0.2 types and operators.
- [ ] Implement trait conformance and method resolution without user-defined generics.
- [ ] Implement control-flow, definite-initialization, mutability, return, and unreachable-code analysis.
- [ ] Define stable diagnostic codes, labels, notes, suggestions, JSON output, and human-readable terminal rendering.

## Phase 4 — MIR, LLVM, Native Runtime

- [ ] Define typed MIR with explicit control-flow graphs, storage operations, calls, drops/runtime roots, and source maps.
- [ ] Lower block values, optional values, tuple returns, structs, traits, collections, and errors into MIR.
- [ ] Pin LLVM 22.1.x and `llvm-sys` behind a small safe internal codegen facade.
- [ ] Define ABI layouts and calling conventions for every runtime-visible v0.2 type.
- [ ] Generate verifiable LLVM IR and reject invalid modules before object emission.
- [ ] Implement target-machine configuration, optimization levels, object generation, and Linux x86_64 linking through LLD.
- [ ] Build a statically linked runtime ABI for allocation, strings, vectors, maps, errors, I/O, process startup, and shutdown.
- [ ] Integrate Boehm GC initialization, allocation, thread registration, finalization policy, and stress tests behind a replaceable allocator contract.
- [ ] Emit source-aware stack traces and minimum DWARF debug information.
- [ ] Add compile-pass, compile-fail, IR-verifier, native-execution, ABI, GC-stress, and deterministic-build suites.

## Phase 5 — Async Runtime

- [ ] Define task, waker, cancellation, deadline, and async-I/O runtime ABIs.
- [ ] Lower `async poof` into LLVM switched-resume coroutines with verified resume, completion, and destroy paths.
- [ ] Keep coroutine frames safe across suspension, GC, cancellation, and worker-thread migration.
- [ ] Implement a work-stealing scheduler with bounded blocking pools and structured task ownership.
- [ ] Implement async timers, TCP, DNS, file/process adapters, and cancellation-aware waits.
- [ ] Propagate deadlines and request context through nested async calls.
- [ ] Test cancellation races, task leaks, starvation, wake coalescing, shutdown, and high-concurrency workloads.

## Phase 6 — Project and Package Tooling

- [ ] Specify `poo.toml` package, target, feature, dependency, profile, and workspace sections.
- [ ] Specify deterministic `poo.lock` entries, source identities, checksums, and toolchain compatibility.
- [ ] Implement local/path and pinned Git dependency resolution with cycle/conflict diagnostics and an offline cache.
- [ ] Implement `poo new`, `init`, `check`, `build`, `run`, `test`, `fmt`, `add`, `remove`, and `update`.
- [ ] Generate conventional `src/main.poo`, `src/lib.poo`, `tests/`, and configuration layouts.
- [ ] Implement a canonical formatter from the lossless syntax tree with idempotence tests.
- [ ] Implement `test "description" { ... }`, filtering, parallel execution, fixtures, coverage hooks, and machine-readable output.
- [ ] Add reproducible debug/release profiles and clear compiler/runtime version reporting.

## Phase 7 — v0.2 Production API Foundation

- [ ] Define stable `std::http`, `std::json`, `std::config`, `std::log`, `std::validation`, `std::time`, and `std::test` contracts.
- [ ] Implement a production HTTP/1.1 server/client over the async runtime with limits, timeouts, keep-alive, streaming bodies, and graceful shutdown.
- [ ] Implement the explicit app/router API with route parameters, query parsing, route groups, middleware, typed request context, and error handlers.
- [ ] Implement JSON primitives, collections, and exact-public-field struct encoding plus `json.decode[Type]` returning `(Type?, error?)`.
- [ ] Implement reusable validation rules and structured field errors suitable for JSON APIs.
- [ ] Implement typed environment/configuration loading, profiles, defaults, redaction, and secret-provider hooks.
- [ ] Implement structured logs with request IDs and context propagation.
- [ ] Add health/readiness endpoints, CORS, secure headers, body limits, authentication primitives, and in-memory rate limiting.
- [ ] Add HTTP test clients, route/middleware tests, deterministic clocks, and network mocks.
- [ ] Ship a generated `web-api` example and verify build-to-first-response, error, shutdown, and load behavior.
- [ ] Gate v0.2 on documented security limits, concurrency benchmarks, fuzzing, and a Linux container deployment guide.

## Phase 8 — Fast `poo-analyzer`

- [ ] Build an incremental query database shared with the compiler for parse, module, scope, type, reference, and diagnostic results.
- [ ] Invalidate changed syntax items and their dependents rather than whole files or workspaces.
- [ ] Add cancellable, immutable request snapshots and prioritize open-file work over background indexing.
- [ ] Add a persistent symbol/index cache keyed by toolchain, manifest, lockfile, and content hashes.
- [ ] Ship `poo-analyzer --stdio` and `poo-analyzer --version` without linking LLVM/codegen libraries.
- [ ] Implement incremental diagnostics and compiler-equivalent diagnostic codes.
- [ ] Implement type/documentation-rich completion, fuzzy typo suggestions, and safe auto-import edits.
- [ ] Implement hover, signature help, definition, references, implementations, document/workspace symbols, and call hierarchy.
- [ ] Implement conflict-safe rename, formatting, semantic tokens, inlay hints, selection ranges, code actions, and explainable refactors.
- [ ] Add framework-aware route, configuration, DI, migration, repository, and generated-code intelligence as their APIs land.
- [ ] Meet warm p95 under 20 ms for interactive requests and under 75 ms for affected-file diagnostics on the 100k-line benchmark.
- [ ] Deliver first useful cold diagnostics within one second, keep idle memory below 250 MiB, and enforce nightly 10% regression thresholds.
- [ ] Add LSP transcript, incomplete-code, cancellation, multi-root, cache-corruption, rename-conflict, and compiler-equivalence tests.

## Phase 9 — Syntax Highlighting and Editors

- [ ] Create issue-tracked `shayyz-code/tree-sitter-poo`, `poo-vscode`, and `poo-zed` repositories with compatibility policies.
- [ ] Implement and fuzz a tree-sitter grammar against the normative syntax corpus; the compiler grammar remains authoritative.
- [ ] Define shared semantic token types/modifiers and highlighting fixtures for declarations, references, mutability, async, traits, and built-ins.
- [ ] Build the VS Code extension with language ID `poo`, `.poo` association, TextMate fallback, brackets, comments, indentation, snippets, and semantic tokens.
- [ ] Build the Zed extension with pinned tree-sitter revision, highlight/indent/outline/bracket/text-object queries, semantic-token rules, and LSP configuration.
- [ ] Resolve analyzer binaries from configured path, then `PATH`, then a checksum/signature-verified compatible download.
- [ ] Publish syntax-only previews after grammar freeze, then add LSP features incrementally.
- [ ] Publish stable releases to Visual Studio Marketplace, Open VSX, and Zed's extension gallery when the rich analyzer gate passes.
- [ ] Add theme-neutral highlight snapshots, extension packaging, install/startup, upgrade, offline, and analyzer-version compatibility tests.

## Phase 10 — v0.2 Release

- [ ] Rewrite README and mdBook around the compiled backend language and remove interpreter-era promises/examples.
- [ ] Publish the language specification, CLI reference, standard-library reference, deployment guide, and editor setup.
- [ ] Build signed/checksummed Linux x86_64 toolchain, runtime, and analyzer artifacts.
- [ ] Generate a release SBOM and third-party notices when external Rust or native dependencies first ship.
- [ ] Verify installation, compilation, HTTP example, package locking, analyzer, VS Code, and Zed from clean machines.
- [ ] Publish v0.2.0 only when compiler, async, HTTP/JSON, package tooling, analyzer, performance, and security gates pass.

## Phase 11 — v0.3 Opinionated Application Platform

- [ ] Define official package namespaces and compatibility guarantees for web, data, auth, cache, queue, mail, storage, events, and scheduling.
- [ ] Add `web-api` and `worker` starters with conventional controllers, services, models, middleware, config, migrations, and tests.
- [ ] Implement compile-time constructor injection, explicit registrations, lifecycle hooks, cycle detection, test overrides, and analyzer support.
- [ ] Add `poo make` generators for controllers, services, models, migrations, middleware, policies, tests, and jobs.
- [ ] Implement PostgreSQL-first async pooling, transactions, migrations, seeds, test isolation, and failure recovery.
- [ ] Implement a typed data mapper, explicit repositories, and compile-time checked query builder without hidden lazy queries.
- [ ] Implement official Redis cache, distributed locks, rate limits, queue coordination, and connection health.
- [ ] Implement authentication, password hashing, tokens, authorization policies, and security middleware.
- [ ] Implement jobs/workers with retries, backoff, idempotency hooks, dead-letter handling, metrics, and graceful draining.
- [ ] Implement cache, mail, object storage, events, scheduling, and background-worker contracts.
- [ ] Build a public registry with authenticated publish/search/yank, SemVer resolution, immutable artifacts, checksums, provenance, and abuse response.
- [ ] Implement `poo search`, `publish`, `yank`, registry login, alternate registries, and reproducible registry lockfile resolution.
- [ ] Gate v0.3 on PostgreSQL and Redis integration suites, migration safety, queue failure tests, registry security, and starter end-to-end tests.

## Phase 12 — v0.4 gRPC and Microservices

- [ ] Add official protobuf, gRPC, resilience, and OpenTelemetry packages plus a `grpc-service` starter.
- [ ] Implement `poo generate proto` using standard `.proto` files and generate typed messages, async clients, and server traits.
- [ ] Support unary, client-streaming, server-streaming, and bidirectional RPCs with backpressure.
- [ ] Implement gRPC metadata, status, deadlines, cancellation, compression, TLS/mTLS hooks, authentication interceptors, reflection, and health service.
- [ ] Implement explicit retry policies, exponential backoff with jitter, throttling, idempotency controls, circuit breakers, bulkheads, and client balancing.
- [ ] Support DNS and Kubernetes-native discovery without a proprietary registry or service mesh.
- [ ] Implement OpenTelemetry-compatible traces, metrics, structured logs, baggage/context propagation, and OTLP export across HTTP, queues, and gRPC.
- [ ] Add liveness/readiness, graceful connection draining, container builds, deployment manifests, and local multi-service development commands.
- [ ] Add cross-language Go and Java interoperability tests covering every RPC shape, errors, deadlines, cancellation, health, retries, auth, and telemetry.
- [ ] Gate v0.4 on load, failure-injection, rolling-deployment, observability-correlation, and service-mesh compatibility tests.

## Later Work

- [ ] Add Linux ARM64, macOS, and Windows compiler/runtime/analyzer artifacts after Linux x86_64 is stable.
- [ ] Add SQLite and MySQL through the v0.3 data contracts after PostgreSQL reaches production quality.
- [ ] Evaluate user-defined generics, pattern matching, a precise moving GC, WebAssembly, and FFI through separate language/runtime RFCs.
- [ ] Evaluate server-rendered templates, sessions, forms, localization, and asset tooling without weakening the API/backend-first focus.
- [ ] Add debugger protocol support, profiling, coverage visualization, and framework-specific editor dashboards.

## Reference Architecture Sources

- LLVM frontend, object generation, coroutine, and garbage-collection documentation: <https://llvm.org/docs/>
- Boehm-Demers-Weiser collector: <https://www.hboehm.info/gc/>
- Language Server Protocol: <https://microsoft.github.io/language-server-protocol/>
- VS Code language extensions: <https://code.visualstudio.com/api/language-extensions/overview>
- Zed language extensions: <https://zed.dev/docs/extensions/languages>
- Laravel application patterns: <https://laravel.com/docs/12.x>
- Spring dependency injection and production features: <https://docs.spring.io/spring-framework/reference/core/beans.html>
- gRPC concepts and guides: <https://grpc.io/docs/>
- OpenTelemetry specification: <https://opentelemetry.io/docs/specs/otel/>
