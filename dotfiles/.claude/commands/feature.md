---
description: Manage feature lifecycle (new, list, status, set-phase, brainstorm, plan, decompose, spawn, merge)
---

# Feature Command

Manage features and their lifecycle. Each feature gets a self-contained directory with spec, tasks, notes, and status tracking.

## Input

$ARGUMENTS: `<subcommand> [args]`

Subcommands:
- `help` - Show pipeline overview with inputs/outputs for each stage
- `new <name>` - Create a new feature
- `list` - List all features
- `status [name]` - Show detailed status of a feature
- `set-phase <name> <phase>` - Manually set a feature's phase
- `brainstorm <name> [topic]` - Brainstorm on a feature (writes to notes.md)
- `plan <name> [description]` - Create/refine spec (writes to spec.md)
- `decompose <name>` - Decompose spec into tasks (writes to tasks.md)
- `spawn <name>` - Spawn parallel workers from tasks.md
- `merge <name>` - Merge completed task branches

## Features Directory

All feature data lives in:
```
~/.claude/projects/-Users-ben-Documents-code-atlas-core/features/
```

Each feature gets:
```
<feature-name>/
  spec.md       # The specification
  tasks.md      # Decomposed tasks
  notes.md      # Brainstorm capture, scratchpad, decisions
  status.json   # Lifecycle metadata
```

## Lifecycle Helper

Many subcommands need to load and update a feature's status. Common pattern:

1. Resolve FEATURES_DIR: `$HOME/.claude/projects/-Users-ben-Documents-code-atlas-core/features`
2. Verify `$FEATURES_DIR/<name>/status.json` exists; if not, tell user and suggest `/feature new <name>`
3. After any write operation, update `status.json`: set `updated` to today's date, and update `phase` if the subcommand triggers a phase transition

---

## Subcommand: `help`

Print the pipeline overview. Output this exactly:

```
## Feature Lifecycle Pipeline

Everything is local -- nothing touches GitHub until you explicitly push or create a PR.

### Pipeline

  /feature new <name>          Create feature directory + scaffold files
                               -> brainstorming
       |
  /feature brainstorm <name>   Conversational exploration (no code, no files)
                               Reads: notes.md    Writes: notes.md
       |
  /feature plan <name>         Draft implementation spec from brainstorm
                               Reads: notes.md    Writes: spec.md
                               -> speccing -> approved (on user approval)
       |
  /feature decompose <name>    Break spec into parallel, file-disjoint tasks
                               Reads: spec.md     Writes: tasks.md
                               -> decomposed
       |
  /feature spawn <name>        Launch worker agents in local worktrees
                               Reads: tasks.md    Writes: tasks.md (assigns owners)
                               -> implementing
       |
  /feature merge <name>        Interactively merge completed branches (local only)
                               Reads: tasks.md    Writes: tasks.md (marks merged)
                               -> done (when all tasks merged)

### Management

  /feature list                Table of all features (name, phase, tasks, branch)
  /feature status [name]       Detailed view of one feature (defaults to most recent)
  /feature set-phase <n> <p>   Manually override phase
  /resume                      "Where was I?" -- show active features, pick one up

### Phases

  brainstorming -> speccing -> approved -> decomposed -> implementing -> done -> archived

### Files (per feature)

  ~/.claude/projects/.../features/<name>/
    spec.md       The specification
    tasks.md      Decomposed task list
    notes.md      Brainstorm capture, decisions, scratchpad
    status.json   Phase, branch, PR, task counts, timestamps
```

---

## Subcommand: `new <name>`

1. Parse the feature name from $ARGUMENTS (everything after `new`). Slugify it: lowercase, hyphens for spaces, strip non-alphanumeric except hyphens.

2. Check for duplicates:
```bash
ls ~/.claude/projects/-Users-ben-Documents-code-atlas-core/features/ 2>/dev/null
```
If a directory with that name already exists, tell the user and stop.

3. Create the feature directory and files:
```bash
FEATURES_DIR="$HOME/.claude/projects/-Users-ben-Documents-code-atlas-core/features"
NAME="<slugified-name>"
mkdir -p "$FEATURES_DIR/$NAME"
```

4. Create `status.json` with this schema:
```json
{
  "name": "<name>",
  "phase": "brainstorming",
  "created": "<today YYYY-MM-DD>",
  "updated": "<today YYYY-MM-DD>",
  "branch": null,
  "pr_url": null,
  "tasks": {
    "total": 0,
    "done": 0,
    "in_progress": 0,
    "blocked": 0
  },
  "tags": []
}
```

5. Create empty `spec.md`:
```markdown
# <Feature Name>

_Created: <today>_

## Overview

<!-- What is this feature? Why are we building it? -->

## Design

<!-- Technical approach, architecture decisions -->

## Acceptance Criteria

<!-- Testable conditions for "done" -->
```

6. Create empty `notes.md`:
```markdown
# <Feature Name> - Notes

## Brainstorm

<!-- Ideas, questions, rough thinking -->
```

7. Create empty `tasks.md`:
```markdown
# <Feature Name> - Tasks

<!-- Task breakdown goes here after decomposition -->
```

8. Confirm to the user:
```
Feature "<name>" created (phase: brainstorming)

  spec.md    - Write your specification here
  tasks.md   - Task breakdown (after decompose)
  notes.md   - Scratchpad for ideas and decisions
  status.json - Lifecycle tracking

Next steps:
  /feature brainstorm <name> <idea>  - Explore the idea
  /feature plan <name> <description> - Draft a spec
  /feature status <name>             - Check status anytime
```

---

## Subcommand: `list`

1. Read all `status.json` files:
```bash
FEATURES_DIR="$HOME/.claude/projects/-Users-ben-Documents-code-atlas-core/features"
for f in "$FEATURES_DIR"/*/status.json; do
  cat "$f" 2>/dev/null
done
```

2. Sort by `updated` (most recent first).

3. Render a table:
```
## Features

| Feature            | Phase        | Tasks    | Branch                   | Updated    |
|--------------------|--------------|----------|--------------------------|------------|
| rf-onboarding-v2   | implementing | 4/6 done | feature/rf-onboarding-v2 | 2026-02-18 |
| agent-planner-v0   | decomposed   | 0/4 done | --                       | 2026-02-15 |
```

- Tasks column: `<done>/<total> done` or `--` if total is 0
- Branch column: branch name or `--` if null
- If no features exist, say "No features yet. Create one with `/feature new <name>`"

Valid phases for reference: `brainstorming` -> `speccing` -> `approved` -> `decomposed` -> `implementing` -> `done` -> `archived`

---

## Subcommand: `status [name]`

1. If no name given, find the most recently updated feature (highest `updated` in status.json).

2. Read the feature's `status.json`.

3. Render detailed view:
```
## <Feature Name>

Phase: <phase>
Branch: <branch or "not set">
PR: <pr_url or "none">
Created: <created>
Updated: <updated>
Tags: <tags joined with ", " or "none">

### Tasks (<done>/<total>)
- Done: <done>
- In Progress: <in_progress>
- Blocked: <blocked>
- Remaining: <total - done - in_progress - blocked>

### Files
- spec.md: <exists? size? first non-empty line after title?>
- tasks.md: <exists? size?>
- notes.md: <exists? size?>
```

4. If the feature directory doesn't exist, say so and suggest `/feature list`.

---

## Subcommand: `set-phase <name> <phase>`

1. Parse name and phase from $ARGUMENTS.

2. Validate phase is one of: `brainstorming`, `speccing`, `approved`, `decomposed`, `implementing`, `done`, `archived`.

3. Read the feature's `status.json`, update `phase` and `updated` (to today), write it back.

4. If phase is `archived`, move the feature directory to `features/_archived/<name>/`.

5. Confirm: `Feature "<name>" phase set to <phase>`.

---

## Subcommand: `brainstorm <name> [topic]`

Open-ended exploration of a feature idea. Same spirit as the standalone `/brainstorm` command, but scoped to a feature and captured in its notes.

**Phase transition**: If phase is before `brainstorming`, set to `brainstorming`. Don't regress phase if it's already past brainstorming.

**Before starting**:
1. Load `features/<name>/status.json` (verify feature exists)
2. Read `features/<name>/notes.md` to see what's already been captured
3. If a topic was provided in $ARGUMENTS (after the name), use it as the starting point

**Behavior** (same rules as standalone /brainstorm):
1. No code, no files, no specs -- just conversation
2. Ask questions -- poke holes, surface edge cases, suggest alternatives
3. Be opinionated -- push back on bad ideas
4. Think about: simplest version, future pain points, tradeoffs, prior art, user experience
5. Use ASCII diagrams when useful
6. Keep responses short, ask one question at a time

**When the session wraps up**:
1. Append a dated summary of key decisions/ideas to `features/<name>/notes.md`
2. Update `status.json`: set `updated` to today
3. Suggest next step: `Ready to formalize? Run: /feature plan <name>`

---

## Subcommand: `plan <name> [description]`

Transform the brainstorm into an implementation-ready spec. Same spirit as standalone `/plan`, but writes to the feature's spec.md.

**Phase transition**: Set phase to `speccing`. Don't regress if already past speccing.

**Before starting**:
1. Load `features/<name>/status.json` (verify feature exists)
2. Read `features/<name>/notes.md` for brainstorm context
3. Read `features/<name>/spec.md` to see if there's an existing draft to refine
4. If a description was provided in $ARGUMENTS (after the name), use it as the prompt

**Step 1: Gather Context**

Understand the codebase:
- Project type (check package.json, pyproject.toml, etc.)
- Directory conventions
- Existing patterns to follow

Use Explore subagent if needed for codebase research.

**Step 2: Clarify Requirements**

If the brainstorm notes + description leave ambiguity, ask 3-5 specific questions. Propose a direction and ask for confirmation -- don't ask open-ended questions.

**Step 3: Research (if needed)**

If the feature involves external APIs, unfamiliar libraries, or complex domain -- look them up.

**Step 4: Generate Spec**

Write the spec to `features/<name>/spec.md`. Requirements:
1. Every component must have explicit file paths
2. No file should appear in multiple components
3. All types/interfaces must be defined inline
4. Acceptance criteria must be testable
5. Test cases must be specified
6. Edge cases must be listed with handling approach

**Step 5: Review Checkpoint**

Present the spec and ask:
```
## Spec Review

I've created a detailed spec for <name>. Key points:

- Components: [N] parallel workstreams
- Files to create: [N]
- Files to modify: [N]

### Please Review:
1. Scope: Is anything missing? Anything that should be cut?
2. Assumptions: [List 2-3 key assumptions]
3. Risk areas: [Component X] touches [shared thing] - confirm approach

### Options:
- "approved" - Mark spec as approved, ready for decomposition
- "add [feature]" - Expand scope
- "remove [feature]" - Reduce scope
- "change [X] to [Y]" - Modify approach
- "more detail on [component]" - Expand a section
```

**Step 6: On Approval**

1. Write final spec to `features/<name>/spec.md`
2. Update `status.json`: phase -> `approved`, updated -> today
3. Suggest: `/feature decompose <name>`

---

## Subcommand: `decompose <name>`

Break the spec into independent, parallelizable tasks. Same spirit as standalone `/decompose-spec`, but reads/writes within the feature directory.

**Phase transition**: Set phase to `decomposed`.

**Before starting**:
1. Load `features/<name>/status.json` (verify feature exists, phase should be `approved` or later)
2. Read `features/<name>/spec.md` -- this is the input

**Process** (same as standalone /decompose-spec):

1. Identify components from the spec
2. Map each component to specific files it will create/modify
3. Check for file overlap -- if two tasks touch the same file, combine them or make one depend on the other
4. Generate task list with spec-specific IDs: `{NAME}-001`, `{NAME}-002`, etc.

**Task format** (written to `features/<name>/tasks.md`):
```markdown
# <Feature Name> - Tasks

> Generated from: features/<name>/spec.md
> Created: <today>

## Backlog

- [ ] NAME-001: [Title] | Owner: - | Status: backlog
  - Files: src/file1.ts, src/file2.ts
  - Depends on: None

## In Progress

## Done

## Merged

---

## Task Details

### NAME-001: [Title]

Scope: [description]

Files:
- `src/path/to/file1.ts` (create)
- `src/path/to/file2.ts` (modify)

Acceptance criteria:
- [ ] Criterion 1
- [ ] Criterion 2

Dependencies: None
```

**Validation before saving**:
- No two parallel tasks touch the same files
- Each task is completable in isolation
- Shared infrastructure assigned to earliest task

**After saving**:
1. Write tasks to `features/<name>/tasks.md`
2. Update `status.json`: phase -> `decomposed`, tasks.total -> count, updated -> today
3. Show summary: "[N] tasks created for <name>"
4. Show execution order diagram
5. Suggest: `/feature spawn <name>`

---

## Subcommand: `spawn <name>`

Launch parallel worker agents from the feature's task list. Same spirit as standalone `/spawn-workers`, but reads from the feature directory.

**Phase transition**: Set phase to `implementing`.

**Before starting**:
1. Load `features/<name>/status.json` (verify feature exists, phase should be `decomposed` or later)
2. Read `features/<name>/tasks.md`
3. If no backlog tasks exist, tell user and stop

**Process** (same as standalone /spawn-workers):

1. Detect project type and guidelines
2. Read backlog tasks from `features/<name>/tasks.md`
3. Check available worktrees (`../worktrees/agent-*`)
4. For each backlog task, assign to next available agent
5. Update `features/<name>/tasks.md` with owner and status
6. Spawn each worker via Task tool (run_in_background: true)

Worker prompt should include:
- Which guidelines to read
- The task details from tasks.md
- Working directory (worktree path)
- Instruction to commit locally, not push

**After spawning**:
1. Update `status.json`: phase -> `implementing`, tasks.in_progress -> count, updated -> today
2. Log spawn summary table (agent, task ID, description)
3. Suggest: `/feature status <name>` or `/check-status`

---

## Subcommand: `merge <name>`

Interactively merge completed worker branches. Same spirit as standalone `/merge-complete`, but updates the feature's status.

**Phase transition**: If all tasks are merged, set phase to `done`.

**Before starting**:
1. Load `features/<name>/status.json`
2. Read `features/<name>/tasks.md`

**Process** (same as standalone /merge-complete):

1. Scan worktrees for branches matching `task/{NAME}-*`
2. For each branch with commits, show diff stats and ask user to approve
3. Merge approved branches (no-ff)
4. Run validation after each merge
5. Handle conflicts interactively
6. Update `features/<name>/tasks.md` status to `merged` for each merged task

**After merging**:
1. Recount tasks and update `status.json`: tasks.done, tasks.in_progress, etc.
2. If all tasks are done/merged, update phase -> `done`
3. Update `updated` -> today
4. Show summary: merged count, remaining count, validation status
5. Suggest next steps based on state

---

## Error Handling

- Unknown subcommand: list available subcommands
- Missing required arguments: show usage for that subcommand
- Feature not found: suggest `/feature list` to see available features
- Wrong phase for subcommand (e.g., `spawn` before `decomposed`): warn but allow with confirmation
