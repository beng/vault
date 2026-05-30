---
description: Interactively merge completed task branches from local worktrees
---

# Merge Completed Tasks

Interactively merge completed worker branches from local worktrees.
Works WITH the user to review and approve each merge. No remote operations.

## Input

$ARGUMENTS: Optional feature name
- (empty) - Merge all completed branches
- `<feature-name>` - Only merge branches for that feature (e.g., `roguelike`)

## Prerequisites

- Worktrees exist at `../worktrees/agent-*`
- Workers should have committed their changes locally (no push required)

## IMPORTANT: Interactive Mode

This command works WITH the user interactively. DO NOT auto-merge without user approval.
For each branch, show the changes and ASK the user before merging.
The user has final say on what gets merged.

## Process

### 1. Determine Scope

```bash
FEATURE="$ARGUMENTS"

# Get the base branch (what worktrees were created from)
BASE_BRANCH=$(git branch --show-current)
echo "Base branch: $BASE_BRANCH"

if [ -n "$FEATURE" ]; then
  TASKS_FILE="tasks/${FEATURE}_tasks.md"
  BRANCH_PATTERN="task/${FEATURE^^}-"  # e.g., task/ROGUELIKE-
  echo "Merging branches for: $FEATURE"
  echo "Task file: $TASKS_FILE"
else
  TASKS_FILE=""
  BRANCH_PATTERN="task/"
  echo "Merging all completed task branches"
fi
```

### 2. Scan Local Worktrees for Branches

```bash
echo ""
echo "=== BRANCHES READY FOR MERGE (from local worktrees) ==="
for dir in ../worktrees/agent-*; do
  if [ -d "$dir" ]; then
    agent=$(basename "$dir")
    branch=$(git -C "$dir" branch --show-current 2>/dev/null)

    if [ -z "$branch" ]; then
      echo "- $agent: no branch (skipping)"
      continue
    fi

    # Check if branch matches our pattern (if filtering by feature)
    if [ -n "$BRANCH_PATTERN" ] && [[ ! "$branch" == *"$BRANCH_PATTERN"* ]]; then
      continue
    fi

    commits=$(git -C "$dir" log --oneline HEAD ^"$BASE_BRANCH" 2>/dev/null | wc -l | tr -d ' ')
    last_msg=$(git -C "$dir" log -1 --format="%s" 2>/dev/null || echo "no commits")

    echo ""
    echo "[$agent] $branch"
    echo "  Commits: $commits"
    echo "  Latest: $last_msg"
  fi
done
```

### 3. Merge Each Branch (Interactive with User)

For each worktree with commits, work WITH the user:

**IMPORTANT**: Always use AskUserQuestion to get user approval before each merge.

```bash
AGENT_DIR="../worktrees/agent-01"
BRANCH=$(git -C "$AGENT_DIR" branch --show-current)

echo ""
echo "=== Review: $BRANCH (in $(basename $AGENT_DIR)) ==="

# Show what we're about to merge
git -C "$AGENT_DIR" log --oneline HEAD ^"$BASE_BRANCH"
echo ""
git -C "$AGENT_DIR" diff --stat HEAD ^"$BASE_BRANCH"
```

Then ASK the user:
- "Merge this branch?" with options: Yes / Skip / Show full diff first

If user approves:

```bash
# Get the commit SHA from the worktree (git merge takes a ref, not a path)
COMMIT_SHA=$(git -C "$AGENT_DIR" rev-parse HEAD)

# Merge using the commit SHA
git merge "$COMMIT_SHA" --no-ff -m "Merge $BRANCH from $(basename $AGENT_DIR)"

# Run validation if available
if [ -f "package.json" ]; then
  npm run validate 2>/dev/null || npm test 2>/dev/null || true
elif [ -f "pyproject.toml" ]; then
  make run-tests-quick 2>/dev/null || true
fi
```

If validation fails, ASK the user:
- "Validation failed. Keep merge or rollback?" with options: Keep anyway / Rollback

### 4. Handle Conflicts (Interactive)

If merge conflicts occur, ASK the user how to proceed:

```
CONFLICT DETECTED in: [file list]
```

Use AskUserQuestion with options:
- "Help me resolve manually" - Show conflicting files, work with user to resolve
- "Take ours (current branch version)"
- "Take theirs (worktree version)"
- "Skip this branch for now"

After user resolves:
```bash
git add -A
git commit -m "Merge $BRANCH (resolved conflicts)"
```

### 5. Update Task File

After successful merge, update the task file:

```bash
# Extract task ID from branch name (e.g., ROGUELIKE-001 from task/ROGUELIKE-001-foo)
TASK_ID=$(echo "$BRANCH" | grep -oE '[A-Z]+-[0-9]+')

# Find which task file contains this task
if [ -n "$TASKS_FILE" ]; then
  # Update specified task file
  sed -i '' "s/\(${TASK_ID}.*Status:\) [a-z-]*/\1 merged/" "$TASKS_FILE"
else
  # Search all task files
  for file in tasks/*_tasks.md; do
    if grep -q "$TASK_ID" "$file"; then
      sed -i '' "s/\(${TASK_ID}.*Status:\) [a-z-]*/\1 merged/" "$file"
      echo "Updated $file"
      break
    fi
  done
fi
```

### 7. Cleanup (Interactive)

After merging, ASK the user if they want to clean up worktrees:

```bash
echo ""
echo "=== CLEANUP ==="
echo "Merged worktrees can be removed to free up space."
```

For each merged worktree, ask user: "Remove worktree $AGENT_DIR?" with options: Yes / Keep for now

If user approves:

```bash
# Remove the worktree (local only - no remote operations)
git worktree remove "$AGENT_DIR" --force
echo "Removed worktree: $AGENT_DIR"
```

Note: This does NOT push or delete remote branches (push is blocked by policy).

## Post-Merge Summary

```
=== MERGE COMPLETE ===

Feature: roguelike
Merged: 5 branches
Remaining: 3 in-progress

Updated: tasks/roguelike_tasks.md
  - ROGUELIKE-001: merged ✓
  - ROGUELIKE-002: merged ✓
  - ROGUELIKE-003: merged ✓
  - ROGUELIKE-004: merged ✓
  - ROGUELIKE-005: merged ✓

Validation: ✅ All tests passing

Next:
- /check-status roguelike (see remaining tasks)
- /verify-design (check UI consistency)
```

## Usage

```bash
/merge-complete              # Merge all done branches
/merge-complete roguelike    # Only roguelike branches
```
