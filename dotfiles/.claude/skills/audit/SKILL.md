---
name: audit
description: Read-only critical audit of code, a diff, or a subsystem. Use when the user asks to audit, review, diagnose, or critique without making changes. Never edits files.
---

# Read-Only Audit

Hard rules:
- Read-only. No file edits, no state-changing commands, no worktrees.
- Every finding cites file:line you actually read this session.
- Every behavioral claim is backed by a command you actually ran
  (grep, test, typecheck). If you cannot verify a claim, label it
  UNVERIFIED instead of asserting it.
- Do the analysis directly. No multi-agent workflows unless asked.

Process:
1. Restate scope in one line (what is audited, which files/diff).
2. Read the code. Trace actual call paths; never reason from file names.
3. Rate findings:
   - P0: correctness, data loss, security -- ships broken
   - P1: likely bugs, missing error handling, silent failures
   - P2: maintainability, clarity, dead code
4. Per finding: file:line, one-sentence problem, one-sentence minimal
   fix shown as a diff in chat only.
5. End with a bullet summary and severity counts. No PR-comment prose.
