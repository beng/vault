# Guidelines Quick Reference

## Which Guideline When?

| Task Type | Guidelines to Load |
|-----------|-------------------|
| Python backend code | `python.md` + `unit-testing.md` |
| Python with API | `python.md` + `unit-testing.md` |
| TypeScript/JavaScript | `typescript.md` + `unit-testing.md` |
| React components | `typescript.md` + `react.md` + `unit-testing.md` + `design-system.md` |
| React + E2E tests | `typescript.md` + `react.md` + `unit-testing.md` + `e2e-testing.md` + `design-system.md` |
| UI styling/CSS | `design-system.md` |
| API endpoints | `[python\|typescript].md` + `unit-testing.md` |
| Database/models | `[python\|typescript].md` + `unit-testing.md` |
| CLI tools | `[python\|typescript].md` + `unit-testing.md` |
| Full-stack feature | ALL guidelines |

## Auto-Detection

The `/project:load-guidelines` command auto-detects based on:

| File Present | Guidelines Loaded |
|--------------|-------------------|
| `package.json` or `tsconfig.json` | `typescript.md` |
| `pyproject.toml` or `requirements.txt` | `python.md` |
| `app/` or `src/components/` | `e2e-testing.md` + `design-system.md` |
| Always | `unit-testing.md` |

## Manual Loading

```bash
# TypeScript project
cat .claude/guidelines/typescript.md
cat .claude/guidelines/unit-testing.md

# Python project  
cat .claude/guidelines/python.md
cat .claude/guidelines/unit-testing.md

# UI components (add to above)
cat .claude/guidelines/e2e-testing.md
cat .claude/guidelines/design-system.md
```

## What Each Guideline Covers

### python.md
- Project structure
- Type hints (mandatory)
- Pydantic models
- Async patterns
- Error handling
- Logging
- Ruff linting

### typescript.md
- Functional-first patterns (no classes for core logic)
- NO stutter imports (forbidden)
- Result<T, E> pattern for HTTP errors
- Domain boundary rules
- Type architecture
- Strict typing (no `any`)
- Zod validation
- Error handling

### react.md
- Component composition patterns
- Single responsibility principle
- Hooks (useCallback, useMemo, useRef, useEffect)
- State management (colocate, controlled vs uncontrolled)
- Async patterns in effects and handlers
- Forbidden React patterns

### unit-testing.md
- AAA pattern (Arrange/Act/Assert)
- Test factories
- Mocking strategies
- Assertion patterns
- Edge cases
- Test isolation

### e2e-testing.md
- Playwright setup
- Selector priority (roles > test IDs)
- Page Object Model
- Auth fixtures
- Form testing
- API mocking
- CI configuration

### design-system.md
- Color tokens
- Typography scale
- Spacing system
- Component standards (buttons, inputs, cards)
- Interaction states (hover, focus, active, disabled)
- Animation/transitions
- Accessibility requirements
- Responsive breakpoints

## Design Verification

For UI changes, also run:
```bash
/project:verify-design
```

This uses Playwright to:
- Screenshot all component states
- Verify CSS matches design tokens
- Test interactions
- Check accessibility
- Validate responsive layouts
