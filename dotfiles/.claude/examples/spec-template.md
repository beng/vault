# [Feature Name] Spec

> Generated: [DATE]
> Status: Draft
> Estimated agents: [N]

## Overview

[2-3 sentences: What are we building and why?]

## User Stories

- As a [user type], I want to [action] so that [benefit]
- As a [user type], I want to [action] so that [benefit]
- As a [user type], I want to [action] so that [benefit]

## Technical Context

### Code Guidelines
<!-- Reference the guidelines agents must follow -->
- **Language**: See `.claude/guidelines/[python|typescript].md`
- **Unit Testing**: See `.claude/guidelines/unit-testing.md`
- **E2E Testing**: See `.claude/guidelines/e2e-testing.md` (if UI components)

### Existing Patterns to Follow
<!-- Look at your codebase and reference similar implementations -->
- [Pattern]: See `path/to/example`

### Dependencies
<!-- List libraries/services this feature needs -->
- [Library/Service]: [version/details] - used for [purpose]

### Constraints
<!-- Anything that limits implementation choices -->
- [Constraint]

---

## Components

<!-- 
IMPORTANT: Each component should touch DIFFERENT files.
If two components need to modify the same file, merge them or redesign.
-->

### Component 1: [Name]

**Purpose**: [One sentence]

**Files**:
- `path/to/file1.ts` (create)
- `path/to/file2.ts` (modify)

**Interfaces**:
```typescript
// Define ALL types/interfaces this component uses or exposes
// Don't write "TBD" - agents can't ask questions
```

**Behavior**:
1. When [trigger], the system should [action]
2. When [error condition], the system should [error handling]

**Acceptance Criteria**:
- [ ] [Specific, testable criterion]
- [ ] [Specific, testable criterion]

**Test Cases**:
- `should [behavior] when [condition]`
- `should [behavior] when [condition]`
- `should throw/return error when [invalid condition]`

**Edge Cases**:
- [Edge case]: Handle by [approach]

---

### Component 2: [Name]

**Purpose**: 

**Files**:

**Interfaces**:

**Behavior**:

**Acceptance Criteria**:

**Test Cases**:

**Edge Cases**:

---

### Component 3: [Name]

<!-- Add more components as needed -->

---

## Integration Points

### Between Components
<!-- How do components connect? -->
- Component 1 exports [X], imported by Component 2
- Component 2 calls [API endpoint] from Component 3

### External Services
<!-- Any APIs, databases, third-party services -->
- [Service]: [How to call, error handling]

## Data Flow

```
[Entry point]
    ↓
[Step 1]
    ↓
[Step 2]
    ↓
[Output]
```

## Non-Functional Requirements

- **Performance**: [Expectations]
- **Security**: [Requirements]
- **Error Handling**: [Strategy]

## Out of Scope

<!-- Be explicit about what we're NOT building -->
- [Feature X] - will be separate task
- [Feature Y] - future enhancement

## Open Questions

<!-- If there are unresolved questions, list them here for human review -->
- [Question needing decision]

## Decomposition Hint

<!-- Help the decompose command understand dependencies -->

| Agent | Component | Files | Dependencies |
|-------|-----------|-------|--------------|
| 1 | [Name] | [files] | None |
| 2 | [Name] | [files] | After 1 |
| 3 | [Name] | [files] | 1, 2 parallel |

---

## Checklist Before Approval

- [ ] Every component has explicit file paths
- [ ] No file appears in multiple components
- [ ] All interfaces are fully defined (no "TBD")
- [ ] Acceptance criteria are testable
- [ ] Test cases exist for happy path + errors
- [ ] Edge cases are identified with handling
- [ ] Out of scope is explicit
- [ ] An agent could implement this without questions
