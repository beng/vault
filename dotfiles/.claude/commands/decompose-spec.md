---
description: Analyze a spec and decompose into parallel, file-disjoint tasks
---

# Task Decomposition

Analyze the provided spec and break it into independent, parallelizable tasks.

## Input
$ARGUMENTS should be a spec file path (e.g., @specs/roguelike.md)

## Process

### 1. Parse Spec Name

Extract the feature name from the spec file:

```bash
SPEC_PATH="$ARGUMENTS"
SPEC_NAME=$(basename "$SPEC_PATH" .md)
TASKS_FILE="tasks/${SPEC_NAME}_tasks.md"

echo "Spec: $SPEC_NAME"
echo "Tasks will be saved to: $TASKS_FILE"

mkdir -p tasks
```

### 2. Check for Existing Tasks File

```bash
if [ -f "$TASKS_FILE" ]; then
  echo "⚠️  Task file already exists: $TASKS_FILE"
  echo "Options:"
  echo "  1. Append new tasks"
  echo "  2. Overwrite (start fresh)"
  echo "  3. Cancel"
  # Ask user which option
fi
```

### 3. Identify Components
List all distinct features/components in the spec.

### 4. Map to Files
For each component, identify which files will be created or modified.

### 5. Check for Overlap
If two tasks would touch the same file:
- Combine them into one task, OR
- Split the file into separate concerns, OR  
- Make one task depend on the other (sequential, not parallel)

### 6. Generate Task List

Use spec-specific task numbering: `{SPEC_NAME}-001`, `{SPEC_NAME}-002`, etc.

```markdown
# [Feature Name] Tasks

> Generated from: specs/[name].md
> Created: [date]

## Backlog

- [ ] ROGUELIKE-001: [Title] | Owner: - | Status: backlog
  - **Files**: src/file1.ts, src/file2.ts
  - **Depends on**: None

- [ ] ROGUELIKE-002: [Title] | Owner: - | Status: backlog
  - **Files**: src/file3.ts
  - **Depends on**: ROGUELIKE-001

## In Progress

## Done

## Merged

---

## Task Details

### ROGUELIKE-001: [Title]

**Scope**: [description]

**Files**:
- `src/path/to/file1.ts` (create)
- `src/path/to/file2.ts` (create)

**Acceptance criteria**:
- [ ] Criterion 1
- [ ] Criterion 2

**Dependencies**: None

---

### ROGUELIKE-002: [Title]

...
```

### 7. Save Tasks File

```bash
mkdir -p tasks
cat > "tasks/${SPEC_NAME}_tasks.md" << 'TASKFILE'
[generated content]
TASKFILE

echo "✅ Tasks saved to: tasks/${SPEC_NAME}_tasks.md"
```

### 8. Validate Decomposition

Before finalizing, verify:
- [ ] No two parallel tasks touch the same files
- [ ] Each task is completable in isolation
- [ ] Shared infrastructure assigned to earliest task
- [ ] Task IDs use spec prefix (ROGUELIKE-XXX, not TASK-XXX)

## Output

1. Summary: "[N] tasks created for [spec-name]"
2. Task list for review
3. Execution order diagram
4. Confirmation: "Save to tasks/[spec-name]_tasks.md? (yes/no)"

## File Structure

```
project/
├── specs/
│   ├── counter.md
│   ├── liveblocks.md
│   └── roguelike.md
├── tasks/
│   ├── counter_tasks.md      ← Tasks for counter spec
│   ├── liveblocks_tasks.md   ← Tasks for liveblocks spec
│   └── roguelike_tasks.md    ← Tasks for roguelike spec
└── ...
```

Each feature gets its own task file. No conflicts.
