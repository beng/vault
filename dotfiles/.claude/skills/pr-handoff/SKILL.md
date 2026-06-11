---
name: pr-handoff
description: Use when work is ready to push or needs a pull request, when the user says "open a PR", "push this", or "hand off", or when you feel the urge to run git push, gh pr create, or gh repo sync (all deny-listed in this environment).
---

# PR Handoff

Push and PR creation are the user's actions, not yours. Your job is to prepare a copy-paste-ready handoff: ground it in the actual branch state, draft the PR text from real commits, print the exact commands, and stop.

## Steps

1. Ground in actual state (never from memory):
   - `git status` - surface uncommitted changes; report them, do not commit unless told.
   - Base branch: `gh repo view --json defaultBranchRef -q .defaultBranchRef.name`; if the work forked from somewhere else, say so and ask.
   - `git log --oneline <base>..HEAD` and `git diff --stat <base>...HEAD`
2. Draft the title (imperative, one line) and body from those commits and diffs, not from what you remember doing. If `.github/PULL_REQUEST_TEMPLATE.md` exists, follow it.
3. Print ONE copy-paste block in the format below. The quoted heredoc for the body is the point: it prevents the shell-quoting bugs that inline `--body` strings cause.
4. Stop. State plainly that nothing was pushed.

## Output format

```bash
git push -u origin <branch>

gh pr create --base <base> --head <branch> --title "<title>" --body "$(cat <<'EOF'
## Summary
<what changed and why, from the actual commits>

## Test evidence
<commands run and their results, or "untested" stated plainly>
EOF
)"
```

## Rules

- Never execute `git push`, `gh pr create`, or `gh repo sync`. They are deny-listed and will fail; attempting them mid-task is the exact failure this skill exists to prevent.
- Never retry a denied command in another form (`sh -c`, `env`, etc.). Denial means hand off.
- Keep the heredoc delimiter quoted (`'EOF'`) so backticks and `$vars` in the body do not expand.
- ASCII only in title and body: no emojis, no em dashes, straight quotes.
- Do not poll for the PR afterward; the user runs the commands on their own schedule.

## Common mistakes

| Mistake | Fix |
|---------|-----|
| Assuming base is `main` | Read the default branch or the fork point; state which you used |
| Inline `--body "long string"` | Always the quoted heredoc block |
| Drafting the body from memory of the session | Draft from `git log` + `git diff --stat` output you just ran |
| Committing leftover changes to "tidy up" | Report uncommitted files and let the user decide |
