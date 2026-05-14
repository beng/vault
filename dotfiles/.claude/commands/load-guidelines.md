---
name: load-guidelines
description: Load the appropriate coding guidelines for this task
---

# Guidelines Loader

Run this FIRST before any implementation work to load the correct coding standards.

## Auto-Detection

```bash
echo "=== Detecting Project Type ==="

# Check for TypeScript/JavaScript
if [ -f "package.json" ] || [ -f "tsconfig.json" ]; then
  echo "📘 TypeScript/JavaScript project detected"
  echo ""
  echo "=== TYPESCRIPT GUIDELINES ==="
  cat .claude/guidelines/typescript.md
  echo ""
fi

# Check for Python
if [ -f "pyproject.toml" ] || [ -f "setup.py" ] || [ -f "requirements.txt" ]; then
  echo "🐍 Python project detected"
  echo ""
  echo "=== PYTHON GUIDELINES ==="
  cat .claude/guidelines/python.md
  echo ""
fi

# Always load unit testing guidelines
echo "=== UNIT TESTING GUIDELINES ==="
cat .claude/guidelines/unit-testing.md
echo ""

# Check if task involves UI
if [ -d "app" ] || [ -d "src/components" ] || [ -d "pages" ] || [ -f "playwright.config.ts" ]; then
  echo "🎭 UI components detected"
  echo ""
  echo "=== E2E TESTING GUIDELINES ==="
  cat .claude/guidelines/e2e-testing.md
  echo ""
  echo "=== DESIGN SYSTEM GUIDELINES ==="
  cat .claude/guidelines/design-system.md
  echo ""
fi

echo "=== GUIDELINES LOADED ==="
```

## Manual Override

If auto-detection doesn't work, specify explicitly:

```bash
# For Python
cat .claude/guidelines/python.md
cat .claude/guidelines/unit-testing.md

# For TypeScript
cat .claude/guidelines/typescript.md
cat .claude/guidelines/unit-testing.md

# For UI work
cat .claude/guidelines/e2e-testing.md
```

## Summary

After loading, you MUST follow these guidelines:
- All code style rules
- All naming conventions
- All testing patterns
- All error handling patterns

Code that violates guidelines will fail validation.
