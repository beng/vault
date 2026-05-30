---
description: Resume a previous session -- shows active features and lets you pick up where you left off
---

# Resume Command

"Where was I?" -- morning standup for your features.

## Input

$ARGUMENTS: Optional. A feature name to jump straight to.

## Process

### 1. Read All Features

```bash
FEATURES_DIR="$HOME/.claude/projects/-Users-ben-Documents-code-atlas-core/features"
for f in "$FEATURES_DIR"/*/status.json; do
  cat "$f" 2>/dev/null
done
```

Skip anything in `_archived/`.

### 2. Filter and Sort

- Only show features where phase is NOT `archived` and NOT `done`
- Sort by `updated` descending (most recent first)

### 3. If $ARGUMENTS Names a Feature

Jump directly to that feature:
- Read its `status.json`, `spec.md`, `tasks.md`, and `notes.md`
- Show the detailed status (same as `/feature status <name>`)
- Then ask: "What do you want to work on?" with suggestions based on phase:
  - `brainstorming`: "Continue brainstorming, or ready to `/plan`?"
  - `speccing`: "Refine the spec, or `/feature set-phase <name> approved`?"
  - `approved`: "Ready to decompose? `/decompose-spec`"
  - `decomposed`: "Ready to implement? `/spawn-workers`"
  - `implementing`: "Check worker status with `/check-status`, or continue coding"

### 4. If No Arguments

Show the active features table:

```
## Active Features

| Feature              | Phase        | Tasks     | Branch                       | Updated    |
|----------------------|--------------|-----------|------------------------------|------------|
| rf-onboarding-v2     | implementing | 4/6 done  | feature/rf-onboarding-v2     | 2026-02-18 |
| agent-planner-v0     | decomposed   | 0/4 done  | feature/agent-planner        | 2026-02-15 |
| timeline-rebuild     | speccing     | --        | --                           | 2026-02-10 |

Which feature do you want to work on?
```

- Tasks column: `<done>/<total> done` or `--` if total is 0
- Branch column: branch name or `--` if null

If no active features exist:
```
No active features. Start one with `/feature new <name>` or `/brainstorm <idea>`.
```

### 5. After User Picks a Feature

- Load its `status.json`
- Briefly summarize: phase, task progress, branch
- Read the most relevant file based on phase:
  - `brainstorming`/`speccing`: read `notes.md` and `spec.md`
  - `approved`/`decomposed`: read `spec.md` and `tasks.md`
  - `implementing`: read `tasks.md`
- Suggest the logical next action based on phase (same suggestions as step 3)

## Key Behavior

- This is a read-only, orientation command -- don't modify any files
- Keep the output scannable -- tables, not paragraphs
- The goal is to reduce context-switching cost between sessions
