---
description: Verify checks and commit hygiene, push the branch, and open a PR for one issue
argument-hint: <issue-number>
---

Ship the current branch as a pull request for issue #$ARGUMENTS.

1. Refuse if the current branch is `main` or the working tree has uncommitted changes.
2. Run `git fetch origin` and confirm the branch is based on current `origin/main`;
   rebase onto it if not.
3. Run `just check`. Stop and fix (or report) any failure; do not push red builds.
4. Inspect `git log --format='%h %s%n%b' origin/main..HEAD`. PRs are rebase-merged, so
   every commit must be a Conventional Commit (imperative, header of 72 characters or
   fewer) that is self-contained and builds on its own. No `fixup!`, WIP, or merge
   commits. If history needs reshaping, propose the rebase and wait for confirmation.
5. Confirm the diff (`git diff --stat origin/main...HEAD`) addresses only this issue.
6. Push with `git push -u origin HEAD`. If the branch was already pushed and you
   rewrote its history, use `git push --force-with-lease` instead (never `--force`).
7. Open the PR with `gh pr create --base main`, a Conventional Commit title, and a body
   that follows `.github/pull_request_template.md`: `## Summary`, `## Issue` containing
   exactly one `Closes #$ARGUMENTS`, `## Verification` listing the commands run, and
   `## Documentation and Visual Impact` (or "None").

Report the PR URL. Never merge the PR, tag, or release.
