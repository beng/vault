---
description: Check status of all parallel worker agents
---

# Check Worker Status

Get a quick overview of all worker progress.

## Arguments

$ARGUMENTS: Optional
- (empty) - Check all worktrees and all task files
- `<feature-name>` - Check specific feature (e.g., `roguelike`)
- `--screenshots` or `-s` - Also capture UI screenshots
- `<feature-name> --screenshots` - Both

## Process

### 1. Determine Scope

```bash
FEATURE=""
SCREENSHOTS=false

for arg in $ARGUMENTS; do
  if [ "$arg" = "--screenshots" ] || [ "$arg" = "-s" ]; then
    SCREENSHOTS=true
  else
    FEATURE="$arg"
  fi
done

if [ -n "$FEATURE" ]; then
  TASKS_FILE="tasks/${FEATURE}_tasks.md"
  echo "📋 Checking: $TASKS_FILE"
else
  echo "📋 Checking all task files"
fi
```

### 2. Scan Worktrees

```bash
echo ""
echo "=== WORKTREE STATUS ==="
for dir in ../worktrees/agent-*; do
  if [ -d "$dir" ]; then
    agent=$(basename $dir)
    branch=$(git -C "$dir" branch --show-current 2>/dev/null || echo "detached")
    commits=$(git -C "$dir" rev-list --count main..HEAD 2>/dev/null || echo "0")
    last_commit=$(git -C "$dir" log --oneline -1 2>/dev/null || echo "no commits")
    status=$(git -C "$dir" status -s | wc -l | tr -d ' ')
    
    echo ""
    echo "[$agent] branch: $branch"
    echo "  commits ahead: $commits"
    echo "  uncommitted: $status files"
    echo "  last: $last_commit"
  fi
done
```

### 3. Check Task Files

```bash
echo ""
echo "=== TASK PROGRESS ==="

if [ -n "$FEATURE" ]; then
  FILES="tasks/${FEATURE}_tasks.md"
else
  FILES=$(ls tasks/*_tasks.md 2>/dev/null)
fi

for file in $FILES; do
  if [ -f "$file" ]; then
    name=$(basename "$file" _tasks.md)
    backlog=$(grep -c "Status: backlog" "$file" 2>/dev/null || echo 0)
    progress=$(grep -c "Status: in-progress" "$file" 2>/dev/null || echo 0)
    done=$(grep -c "Status: done" "$file" 2>/dev/null || echo 0)
    merged=$(grep -c "Status: merged" "$file" 2>/dev/null || echo 0)
    
    echo ""
    echo "[$name]"
    echo "  ⏳ Backlog:     $backlog"
    echo "  🔄 In Progress: $progress"
    echo "  ✅ Done:        $done"
    echo "  🎉 Merged:      $merged"
  fi
done
```

### 4. Check for Problems

```bash
echo ""
echo "=== POTENTIAL ISSUES ==="
for dir in ../worktrees/agent-*; do
  if [ -d "$dir" ]; then
    agent=$(basename $dir)
    
    # Check for merge conflicts
    if git -C "$dir" diff --check 2>/dev/null | grep -q "conflict"; then
      echo "⚠️  $agent: has merge conflicts"
    fi
    
    # Check for stale work (no commits in 30+ min)
    last_commit_time=$(git -C "$dir" log -1 --format=%ct 2>/dev/null || echo "0")
    now=$(date +%s)
    age=$(( (now - last_commit_time) / 60 ))
    if [ "$age" -gt 30 ] && [ "$age" -lt 9999 ]; then
      echo "⏰ $agent: no commits in ${age} minutes"
    fi
  fi
done
```

### 5. Screenshots (if requested)

If `$SCREENSHOTS` is true:

```bash
echo ""
echo "=== CAPTURING SCREENSHOTS ==="
SCREENSHOT_DIR="screenshots/status-$(date +%Y%m%d-%H%M%S)"
mkdir -p $SCREENSHOT_DIR

# Start dev server if not running
if ! curl -s http://localhost:5173 > /dev/null 2>&1; then
  echo "Starting dev server..."
  npm run dev &
  DEV_PID=$!
  npx wait-on http://localhost:5173 --timeout 30000
fi
```

Use Playwright MCP to capture:

```typescript
const viewports = [
  { width: 1280, height: 800, name: 'desktop' },
  { width: 375, height: 667, name: 'mobile' },
];

await page.goto('http://localhost:5173');
await page.waitForLoadState('networkidle');

for (const vp of viewports) {
  await page.setViewportSize(vp);
  await page.screenshot({ 
    path: `${SCREENSHOT_DIR}/home-${vp.name}.png`,
    fullPage: true 
  });
  console.log(`📸 home-${vp.name}.png`);
}
```

```bash
if [ ! -z "$DEV_PID" ]; then
  kill $DEV_PID 2>/dev/null
fi
echo "📁 Screenshots: $SCREENSHOT_DIR"
```

### 6. Summary Table

```
=== SUMMARY ===

Feature    | Backlog | In Progress | Done | Merged
-----------|---------|-------------|------|-------
roguelike  | 3       | 4           | 1    | 0
liveblocks | 0       | 0           | 5    | 5

Worktrees: 8 active, 2 idle
Screenshots: screenshots/status-20240115-143022/
```

## Usage

```bash
/check-status                      # All features
/check-status roguelike            # Just roguelike tasks
/check-status --screenshots        # All + screenshots
/check-status roguelike -s         # Roguelike + screenshots
```

## Quick Actions

- Stuck agent: `cd ../worktrees/agent-XX && claude --resume`
- View agent work: `cd ../worktrees/agent-XX && git log --oneline`
- View screenshots: `open screenshots/status-*/`
