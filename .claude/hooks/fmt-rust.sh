#!/bin/sh
# PostToolUse hook: format Rust code after Claude edits a .rs file.
# Reads the hook payload (JSON) from stdin. Runs `cargo fmt --all` in the edited
# file's own tree (so edits inside a git worktree format that worktree), and falls
# back to `rustfmt` on the single file. Never blocks the agent: always exits 0.

payload=$(cat)

file_path=$(printf '%s' "$payload" \
  | tr -d '\n' \
  | sed -n 's/.*"file_path"[[:space:]]*:[[:space:]]*"\([^"]*\)".*/\1/p')

case "$file_path" in
  *.rs) ;;
  *) exit 0 ;;
esac

case "$file_path" in
  /*) ;;
  *) file_path="${CLAUDE_PROJECT_DIR:-.}/$file_path" ;;
esac

[ -f "$file_path" ] || exit 0

if output=$(cd "$(dirname "$file_path")" && cargo fmt --all 2>&1); then
  exit 0
fi

if fallback=$(rustfmt --edition 2024 "$file_path" 2>&1); then
  exit 0
fi

echo "fmt-rust hook: formatting $file_path failed (ignored):" >&2
printf '%s\n%s\n' "$output" "$fallback" | head -n 20 >&2
exit 0
