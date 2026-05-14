---
description: Brainstorm and explore ideas before creating a spec
---

# Brainstorm

Open-ended exploration of an idea. No spec, no tasks, no files created.

## Input

$ARGUMENTS: A rough idea, question, or direction to explore.

## Rules

1. **No code, no files, no specs** - just conversation
2. **Ask questions** - poke holes, surface edge cases, suggest alternatives
3. **Be opinionated** - don't just agree, push back on bad ideas
4. **Think about**:
   - What's the simplest version of this?
   - What will be painful later if we don't think about it now?
   - What are the tradeoffs between approaches?
   - What already exists that we could learn from?
   - What's the user experience actually like?
5. **Draw diagrams** when useful (ASCII)
6. **Research** if needed - look up how others solved similar problems

## Output Format

Keep it conversational. Use short responses. Ask one question at a time.

When the user is ready to move on, suggest:

```
Ready to formalize this? Run:
/plan [summary of what we decided]
```

## Anti-Patterns

- DON'T write code
- DON'T create files
- DON'T generate a spec
- DON'T be a yes-man
- DON'T dump walls of text
