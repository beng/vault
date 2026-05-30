---
description: Initialize a new focused session, copy docs template, and load context
argument-hint: "[topic_path e.g. rf/sparam-to-layout] [extra paths e.g. atlas-web/apps/foo ...]"
requires-approval: true
allowed-tools: Bash, Read
---

!`set -e

if [ ! -d ".claude/docs" ]; then
echo "ERROR: .claude/docs does not exist in $(pwd)."
exit 1
fi

if [ ! -d ".claude/docs/_tmpl" ]; then
echo "ERROR: .claude/docs/\_tmpl does not exist."
exit 1
fi

topic="$1"
mkdir -p ".claude/docs/$topic"
cp -R ".claude/docs/\_tmpl/." ".claude/docs/$topic/"
`

Before we begin, read these files:

@/Users/ben/.claude/CLAUDE.md
@.claude/docs/
!`topic="$1"; echo "@.claude/docs/$topic/"`

Session topic:
!`topic="$1"; echo "$topic"`

Session description:
!`desc="${2:-(no description provided)}"; echo "$desc"`

Additional context:
!`for p in "${@:3}"; do [ -n "$p" ] && echo "@$p"; done`

Once you've read them, confirm:

1. You understand the TypeScript guidelines (factory functions, no classes, file structure)
2. You understand the Python guidelines (typed, functional, pytest)
3. You understand the design and debugging philosophy
4. You will be responsible for updating the below files in @.claude/docs/$1/
   - PLAN.md # High-level roadmap (phases, tasks, success criteria)
   - IMPLEMENTATION.md # Progress tracking (what's done, what's next)
   - DECISIONS.md # Architectural decisions & rejected alternatives
   - SCRATCHPAD.md # My working notes during sessions
   - ISSUES.md # Blockers, open questions, tech debt
   - REFERENCE.md

Once you confirm, then we'll start implementing.
