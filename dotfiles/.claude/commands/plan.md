---
description: Create a detailed spec from a high-level idea
---

# Plan Command

Transform a high-level idea into an implementation-ready spec.

## Input

$ARGUMENTS: Either:
- A brief description of what to build
- A path to a rough requirements doc (@requirements.md)
- "interactive" to start from scratch

## Workflow

### Step 1: Gather Context

First, understand the codebase:

```bash
# What kind of project is this?
echo "=== Project Type ==="
[ -f "package.json" ] && echo "Node.js project" && cat package.json | grep -E '"(name|dependencies|devDependencies)"' | head -20
[ -f "pyproject.toml" ] && echo "Python project" && head -30 pyproject.toml
[ -f "go.mod" ] && echo "Go project" && cat go.mod
[ -f "Cargo.toml" ] && echo "Rust project" && head -20 Cargo.toml

echo ""
echo "=== Directory Structure ==="
find . -type d -not -path '*/node_modules/*' -not -path '*/.git/*' -not -path '*/venv/*' -not -path '*/__pycache__/*' | head -30

echo ""
echo "=== Key Files ==="
find . -type f \( -name "*.ts" -o -name "*.tsx" -o -name "*.py" -o -name "*.go" \) -not -path '*/node_modules/*' | head -40
```

Note:
- Framework in use (React, Express, FastAPI, etc.)
- Directory conventions (src/, app/, lib/, etc.)
- Existing patterns to follow

### Step 2: Understand the Request

If $ARGUMENTS is brief, ask clarifying questions.

**Ask 3-5 specific questions max**, like:
- "This will need [X]. Should that be included or is it separate?"
- "For [edge case], should we [option A] or [option B]?"
- "Is this for [user type A] or [user type B] or both?"

Don't ask open-ended questions. Propose a direction and ask for confirmation.

### Step 3: Research (If Needed)

If the feature involves:
- External APIs → Search for their docs
- Unfamiliar libraries → Look up patterns
- Complex domain → Research best practices

```
# Use Explore subagent for codebase research
Task({
  subagent_type: "Explore", 
  prompt: "Find examples of [pattern] in this codebase",
  run_in_background: false
})
```

### Step 4: Generate Spec

Using the planner agent's template, create a complete spec.

**Critical requirements:**
1. Every component must have explicit file paths
2. No file should appear in multiple components
3. All types/interfaces must be defined inline
4. Acceptance criteria must be testable
5. Test cases must be specified
6. Edge cases must be listed with handling approach

### Step 5: Review Checkpoint

Present the spec and ask:

```
## Spec Review

I've created a detailed spec for [feature]. Key points:

- **Components**: [N] parallel workstreams
- **Estimated agents**: [N]
- **Files to create**: [N]
- **Files to modify**: [N]

### Please Review:

1. **Scope**: Is anything missing? Anything that should be cut?
2. **Assumptions**: [List 2-3 key assumptions you made]
3. **Risk areas**: [Component X] touches [shared thing] - confirm approach

### Options:
- "approved" - Save spec and proceed to decomposition
- "add [feature]" - Expand scope
- "remove [feature]" - Reduce scope  
- "change [X] to [Y]" - Modify approach
- "more detail on [component]" - Expand a section
```

### Step 6: Save Spec

Once approved:

```bash
# Save to specs directory
mkdir -p specs
cat > specs/[feature-name].md << 'EOF'
[Full spec content]
EOF

echo "Spec saved to specs/[feature-name].md"
```

Then prompt:

```
Spec saved! Next steps:

1. Run: /decompose-spec @specs/[feature-name].md
2. Review task breakdown  
3. Run: /spawn-workers

Or if you want to refine further, just tell me what to change.
```

## Quality Gates

Before saving, verify the spec passes these checks:

- [ ] File paths are explicit (not "somewhere in src/")
- [ ] No file appears in multiple components
- [ ] Types are defined (not "define appropriate types")
- [ ] Acceptance criteria use "should" + measurable outcome
- [ ] At least 2 test cases per component
- [ ] Error handling is specified
- [ ] Follows existing codebase patterns
- [ ] Out of scope section exists

## Example Interaction

**User**: `/project:plan Build a notification system`

**Claude**: 
```
I'll create a spec for a notification system. First, let me understand the codebase...

[runs context gathering]

I see you're using Express + PostgreSQL + React. I found existing patterns for:
- API routes in `src/routes/`
- Services in `src/services/`
- React components in `app/components/`

Quick questions before I draft the spec:

1. **Notification types**: Email only, or also in-app + push?
2. **Triggers**: User actions (e.g., comments) or also system events (e.g., daily digests)?
3. **User preferences**: Should users be able to configure which notifications they receive?

[User answers]

[Claude generates full spec]

[Review checkpoint]

[User approves]

Spec saved to specs/notification-system.md

Next: /decompose-spec @specs/notification-system.md
```
