---
description: Spawn worker agents for a specific task file
---

# Spawn Workers

Launch parallel worker agents, each in its own worktree.

## Input

$ARGUMENTS: The spec/feature name (e.g., `roguelike`, `liveblocks`)

This will read from `tasks/{name}_tasks.md`

## Prerequisites

1. Worktrees exist (run `./scripts/setup-worktrees.sh N` first)
2. Task file exists at `tasks/{name}_tasks.md`
3. Each task has clear file scope

## Process

### 1. Find Task File

```bash
FEATURE_NAME="$ARGUMENTS"
TASKS_FILE="tasks/${FEATURE_NAME}_tasks.md"

if [ ! -f "$TASKS_FILE" ]; then
  echo "❌ Task file not found: $TASKS_FILE"
  echo ""
  echo "Available task files:"
  ls tasks/*.md 2>/dev/null || echo "  (none)"
  echo ""
  echo "Usage: /spawn-workers <feature-name>"
  exit 1
fi

echo "📋 Using task file: $TASKS_FILE"
```

### 2. Detect Project Type & Guidelines

```bash
GUIDELINES=""

if [ -f "package.json" ] || [ -f "tsconfig.json" ]; then
  GUIDELINES="TypeScript project. Read .claude/guidelines/typescript.md first."
fi

if [ -f "pyproject.toml" ] || [ -f "requirements.txt" ]; then
  GUIDELINES="Python project. Read .claude/guidelines/python.md first."
fi

GUIDELINES="$GUIDELINES Always read .claude/guidelines/unit-testing.md for test patterns."

echo "Guidelines: $GUIDELINES"
```

### 3. Read Backlog Tasks

```bash
echo ""
echo "=== BACKLOG TASKS ==="
grep -E "Status: backlog" "$TASKS_FILE"
```

### 4. Check Available Worktrees

```bash
echo ""
echo "=== AVAILABLE WORKTREES ==="
ls -d ../worktrees/agent-* 2>/dev/null | wc -l
```

### 5. Assign Tasks to Agents

For each backlog task, find the task details section and assign to next available agent.

Update the task file:
```
- [ ] ROGUELIKE-001: Title | Owner: agent-01 | Status: in-progress | Started: [timestamp]
```

### 6. Spawn Each Worker

For each (worktree, task) pair:

```
Task({
  subagent_type: "general-purpose",
  prompt: `
## GUIDELINES (READ FIRST)
${GUIDELINES}

You MUST read and follow these guidelines before writing any code:
- .claude/guidelines/[python|typescript].md (based on project)
- .claude/guidelines/unit-testing.md
- .claude/guidelines/design-system.md (if UI work)

Run: /load-guidelines

---

## YOUR TASK: ${TASK_ID}

**Feature**: ${FEATURE_NAME}
**Task File**: ${TASKS_FILE}
**Working Directory**: ../worktrees/${AGENT_ID}

**Description**: ${TASK_DESCRIPTION}

**Files to create/modify**:
${FILE_LIST}

**Acceptance Criteria**:
${CRITERIA}

**Dependencies**: ${DEPENDENCIES}

---

## INSTRUCTIONS

1. FIRST: cd ../worktrees/${AGENT_ID} (CRITICAL - all work must happen here)
2. Verify you're in the worktree: pwd should show "worktrees/${AGENT_ID}"
3. git checkout -b task/${TASK_ID}-short-name
4. Run /load-guidelines (MANDATORY)
5. Implement following the guidelines
6. Validate before each commit: npm run validate
7. Commit when complete (DO NOT push - coordinator will review and merge)
8. Update task status in ${TASKS_FILE} to "done"

CRITICAL RESTRICTIONS:
- DO NOT modify files outside your scope
- DO NOT merge to main
- DO NOT run commands in the main checkout - stay in your worktree
- DO NOT use git push, git stash, or any commands that affect shared git state
`,
  run_in_background: true
})
```

### 7. Log Spawn

```
✅ Spawned N workers for "${FEATURE_NAME}":

| Agent    | Task          | Description              |
|----------|---------------|--------------------------|
| agent-01 | ROGUELIKE-001 | Pixi setup + renderer    |
| agent-02 | ROGUELIKE-002 | Dungeon generation       |
| agent-03 | ROGUELIKE-003 | Player + input           |
...

Task file: tasks/${FEATURE_NAME}_tasks.md

Monitor: /check-status ${FEATURE_NAME}
```

## Usage Examples

```bash
# Spawn workers for roguelike tasks
/spawn-workers roguelike

# Spawn workers for liveblocks tasks
/spawn-workers liveblocks
```

## Key Point

Every spawned worker gets told **explicitly**:
- Which guidelines to read
- Which task file to update
- Which feature they're working on

This ensures consistent tracking across parallel agents.
