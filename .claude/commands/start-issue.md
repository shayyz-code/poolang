---
description: Start work on a GitHub issue - branch from fresh main, summarize criteria, plan tests first
argument-hint: <issue-number>
---

Start work on PooLang issue #$ARGUMENTS.

1. Run `gh issue view $ARGUMENTS` and read the title, body, labels, and comments.
   Stop and ask if the issue does not exist, is closed, or is already assigned to an
   open PR (`gh pr list --state all --search "#$ARGUMENTS in:body"`).
2. Pick the Conventional Commit type from the issue (`feat`, `fix`, `docs`, `chore`,
   `ci`, `refactor`, `test`, ...) and a short kebab-case slug. The branch is
   `<type>/$ARGUMENTS-<slug>`.
3. Run `git fetch origin`, then create the branch from fresh `origin/main`. Prefer a
   worktree at `.claude/worktrees/$ARGUMENTS-<slug>`; from the main checkout run
   `git worktree add .claude/worktrees/$ARGUMENTS-<slug> -b <branch> origin/main`. If
   this session is already in an isolated worktree, use
   `git switch -c <branch> origin/main`.
4. Summarize the acceptance criteria as a checklist.
5. If the issue is labelled `language-change`, enter plan mode and get the plan approved
   before editing anything.
6. Plan the tests first: list the `spec_<component>_<behavior>` specs (success and
   failure paths) you will add to `tests/language_specs/`, then the implementation
   steps, the docs to update, and the intended commits.

Do not write code yet; finish by presenting the plan.
