@AGENTS.md

## Claude Code notes

PooLang is becoming a sandboxed scripting language for AI agents: capability-based,
resource-budgeted, deterministic, never panics, LLM-readable diagnostics.

### Workflow

- Branch from a fresh `origin/main` as `<type>/<issue#>-<slug>`. For parallel work use
  one git worktree per issue at `.claude/worktrees/<issue#>-<slug>` (ignored by git).
- Issues labelled `language-change` alter syntax or semantics: use plan mode and get
  the plan approved before editing code.
- Write the failing spec first: `spec_<component>_<behavior>` in
  the matching module of `tests/language_specs/`, covering success and failure paths.
- Run `just check-fast` while iterating and `just check` before pushing.
- PRs are rebase-merged, so every commit lands on `main` as-is: each must be a
  self-contained Conventional Commit (header of 72 characters or fewer) that builds
  and passes tests. Fold review fixes into the commit they belong to.
- Never edit `docs/book/` (mdBook output) or hand-edit `.github/workflows/v-release.yml`;
  after changing `dist-workspace.toml`, regenerate it with `dist generate --mode=ci`.

### Never, unless the user explicitly asks

- Merge PRs, create or push tags, run releases, or publish crates.
- Push to `main` or force-push. Update your own pushed branch with
  `git push --force-with-lease`.

### Permissions and hooks

- `.claude/settings.json` allows routine checks and denies pushes to `main`,
  force-pushes, tags, releases, publishing, and edits to generated files. Every
  `gh pr merge` (or merge through `gh api`) asks first, so a human confirms each merge.
- GitHub branch and tag protection is the real control. These rules are a guardrail
  and can be bypassed (for example `git push -uf` or `git -C <dir> push`).
- A PostToolUse hook runs `cargo fmt` after every Rust edit.

### Project agents and commands

- Subagents in `.claude/agents/`: `implementer`, `spec-writer`, `reviewer`, `docs-sync`.
- Slash commands in `.claude/commands/`: `/start-issue <n>`, `/new-case <behaviour>`,
  `/ship <n>`.
