---
description: Verify UI matches design system standards
---

# Verify Design

Run visual and interaction verification against the design system.

## Input

$ARGUMENTS: Optional scope
- (empty) - Check all pages/components
- `page:/dashboard` - Check specific page
- `component:Button` - Check specific component
- `pr` - Check only files changed in current PR/branch

## Process

### 1. Setup

```bash
# Ensure screenshots directory exists
mkdir -p screenshots

# Get current commit for report
COMMIT=$(git rev-parse --short HEAD)
BRANCH=$(git branch --show-current)

echo "=== Design Verification ==="
echo "Commit: $COMMIT"
echo "Branch: $BRANCH"
echo ""
```

### 2. Start Application

```bash
# Start dev server
echo "Starting dev server..."
npm run dev &
DEV_PID=$!

# Wait for ready
npx wait-on http://localhost:3000 --timeout 60000 || {
  echo "❌ Server failed to start"
  exit 1
}
echo "✓ Server ready"
```

### 3. Run Design Reviewer Agent

```
Task({
  subagent_type: "design-reviewer",
  prompt: `
Verify the UI at http://localhost:3000 matches our design system.

Scope: $ARGUMENTS (or all if empty)

Read the design guidelines first:
cat .claude/guidelines/design-system.md

Then use Playwright to:
1. Screenshot all component states (default, hover, focus, active, disabled)
2. Verify CSS properties match design tokens
3. Test all interactions (clicks, hovers, keyboard navigation)
4. Check responsive layouts (mobile, tablet, desktop)
5. Run accessibility checks

Generate a report in design-report.md with:
- Summary (pass/fail counts)
- Detailed findings per component
- Screenshots of any issues
- Recommended fixes

If critical issues found, list them clearly.
`,
  run_in_background: false
})
```

### 4. Cleanup

```bash
# Stop dev server
kill $DEV_PID 2>/dev/null || true
echo ""
echo "Dev server stopped."
```

### 5. Report Results

```bash
# Show summary
if [ -f "design-report.md" ]; then
  echo ""
  echo "=== Design Report Summary ==="
  head -30 design-report.md
  echo ""
  echo "Full report: design-report.md"
  echo "Screenshots: screenshots/"
fi
```

## Quick Checks (Without Full Agent)

For fast verification during development:

```bash
# Just check if pages load without errors
npx playwright test tests/e2e/smoke.spec.ts

# Visual regression against baselines
npx playwright test tests/e2e/visual.spec.ts --update-snapshots

# Accessibility only
npx playwright test tests/e2e/a11y.spec.ts
```

## Integration with Workflow

### After Every UI Commit

Add to your worker agent's completion step:

```bash
# In worker.md, after pushing:
if git diff --name-only HEAD~1 | grep -qE '\.(tsx|css|scss)$'; then
  echo "UI files changed - running design verification"
  /project:verify-design
fi
```

### As PR Check

```yaml
# .github/workflows/design-check.yml
name: Design Verification
on: [pull_request]
jobs:
  verify:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - uses: actions/setup-node@v4
      - run: npm ci
      - run: npx playwright install --with-deps
      - run: npm run dev &
      - run: npx wait-on http://localhost:3000
      - run: npx playwright test tests/e2e/design-verification.spec.ts
      - uses: actions/upload-artifact@v4
        if: failure()
        with:
          name: design-screenshots
          path: screenshots/
```

## Output

**Success:**
```
=== Design Verification ===
Commit: abc1234
Branch: feat/new-dashboard

✓ Server ready
✓ Components verified: 24/24
✓ Pages verified: 8/8
✓ Interactions verified: 15/15
✓ Accessibility: No violations

Design verification PASSED ✓
```

**Failure:**
```
=== Design Verification ===
Commit: abc1234
Branch: feat/new-dashboard

✓ Server ready
✓ Components verified: 22/24
✗ Pages verified: 7/8
✓ Interactions verified: 15/15
⚠ Accessibility: 2 violations

ISSUES FOUND:
1. Button (ghost variant): Missing hover state
2. Dashboard page: Layout breaks at 375px
3. Color contrast: Text on warning badge fails WCAG AA

See design-report.md for details.
```
