---
name: skill-evaluator
description: "Evaluate the quality of a Claude agent skill file (SKILL.md) against best practices. Use this skill whenever someone asks to review, audit, evaluate, critique, grade, or check a skill file, or when they ask if a skill is 'good', 'well-written', 'following best practices', or 'ready for production'. Also trigger when someone says 'review my skill', 'is this skill file OK', 'what's wrong with my skill', 'improve this skill', or shares a SKILL.md and asks for feedback. Do NOT use for creating skills from scratch (use skill-creator instead) or for general code review unrelated to skill files."
---

# Skill Evaluator

Evaluate a SKILL.md file against established best practices for Claude agent skills. Produce a structured quality report with actionable feedback.

## Workflow

### Step 1: Load the skill file

Read the target skill file. If the user provided a path, use `view` to read it. If they pasted content inline, work from that directly. If the skill directory has bundled resources (scripts/, references/, assets/), scan the directory structure with `view` to understand the full skill anatomy.

```bash
# Read the skill file
view <path-to-SKILL.md>

# Check for bundled resources
view <skill-directory>
```

### Step 2: Run the evaluation

Evaluate the skill across every dimension in the rubric below. For each dimension, assign a rating and write specific, evidence-based feedback referencing actual lines or sections from the skill. Don't just say "this is good" or "this is bad" — quote the problem and explain why it matters and how to fix it.

### Step 3: Produce the report

Generate a structured evaluation report as a Markdown artifact. The report format is defined in the **Report Template** section below.

### Step 4: Present the report

Save the report to `/mnt/user-data/outputs/skill-evaluation.md` and present it to the user with `present_files`.

---

## Evaluation Rubric

Evaluate the skill across these 10 dimensions. Each dimension gets a rating: **Strong**, **Adequate**, or **Needs Work**.

---

### 1. Frontmatter & Triggering

The YAML frontmatter is the primary mechanism that determines whether Claude invokes the skill. A weak description means the skill never fires.

**Check for:**

- `name` field exists and is a clear kebab-case identifier
- `description` field exists, is detailed, and is appropriately "pushy" — it should lean toward over-triggering rather than under-triggering
- Description includes BOTH what the skill does AND specific contexts/phrases/keywords that should trigger it
- Description includes negative triggers (when NOT to use the skill) to avoid false positives against adjacent skills
- Description is a single string, not a list

**Red flags:**

- Description is one sentence or under 30 words — almost certainly too vague
- Description only says what the skill does without mentioning trigger contexts
- No negative triggers when competing skills exist
- Missing frontmatter entirely

**Example of a weak description:**

```
description: Create Word documents
```

**Example of a strong description:**

```
description: "Use this skill whenever the user wants to create, read, edit, or manipulate Word documents (.docx files). Triggers include: any mention of 'Word doc', 'word document', '.docx', or requests to produce professional documents... Do NOT use for PDFs, spreadsheets, or Google Docs."
```

---

### 2. Behavioral Focus

A skill should define agent _behaviors_ — what to do and how to do it — not just be a reference document of facts. The agent reading this skill should come away knowing exactly what actions to take, in what order, with what tools.

**Check for:**

- Instructions are written in imperative form ("Run the validation script", "Read the template file", not "The validation script can be run")
- The skill defines a workflow (a sequence of actions), not just a knowledge dump
- It reads as a playbook, not an encyclopedia article

**Red flags:**

- Majority of content is background information or theory with no action steps
- Passive voice throughout ("the file is then processed")
- Reads like API documentation rather than agent instructions
- No verbs that tell the agent what to _do_

---

### 3. Workflow Clarity & Unambiguity

The execution path must be crystal clear. An agent reading this skill at inference time should never be confused about what to do next. Ambiguity causes the agent to guess, hallucinate steps, or stall.

**Check for:**

- Clear step-by-step workflows with numbered or labeled steps
- Decision points are explicit: "If X, do A. If Y, do B." — not "you might want to consider..."
- Each step has a single, concrete action
- The happy path is obvious; edge cases are handled separately
- Workflow order matches actual execution order (no "but first, you should have already...")

**Red flags:**

- Vague phrases like "consider doing", "you may want to", "it's a good idea to"
- Steps that contain multiple unrelated actions
- Decision points without clear criteria for choosing
- Circular references ("see section X" which says "see section Y" which says "see section X")
- Missing steps -- logical gaps where the agent must infer what happens between step N and step N+1
- **Floating sections**: workflow steps reference logic that lives in separate sections below the workflow (e.g., "see the Validation section"). The agent treats these as optional supplementary reading and skips them. Decision logic belongs inside the step where it applies.
- **Redundant descriptions**: the same requirement described in multiple places (e.g., validation explained in step 3, step 5, and a standalone section). Each instance feels like "I already know this" and the agent skips all of them. One authoritative location per requirement.

---

### 4. Tool Specification

Skills should explicitly name which tools the agent needs to use and how. The agent has access to bash, view, str_replace, create_file, present_files, and web tools — the skill should be specific about which ones are needed and when.

**Check for:**

- Explicit tool names (bash, view, create_file, etc.) appear in the skill
- Command-line tools and their exact invocations are shown (not just "use pandoc" but the actual command)
- External dependencies are listed (npm packages, Python libraries, system tools)
- Installation commands are provided where needed
- Script paths are explicit and correct relative to the skill directory

**Red flags:**

- Vague references like "process the file" without specifying the tool or command
- Assumes tools are installed without verifying or installing them
- References scripts that don't exist in the skill's bundled resources
- No concrete commands anywhere in the skill

---

### 5. Output Specification & Verification

The skill should define what the agent produces, in what format, and how to verify it was done correctly before handing off to the user. Without a verification step, the agent has no way to catch its own mistakes — it just produces output and hopes for the best. The best skills treat verification as a first-class workflow step, not an afterthought.

**Check for:**

- Expected output format is defined (file type, structure, naming)
- Output quality criteria are described (what makes a good output vs. a bad one)
- Output location is specified (where to save files)
- **A concrete verification step exists** — this could be a validation script (`python scripts/validate.py output.docx`), a programmatic check (file opens without error, JSON parses, tests pass), a visual inspection step (screenshot + review), or a structured checklist the agent runs through
- The skill defines what "done correctly" means — not just "save the file" but explicit success criteria the agent can evaluate
- **Recovery path when verification fails** — the skill tells the agent what to do if the output doesn't pass (retry, fix specific issues, fall back to an alternative approach)
- Examples of good output are provided where helpful

**Red flags:**

- No mention of what the output should look like
- No verification or validation step of any kind — the workflow ends at "create the file" with no confirmation it's correct
- Verification is mentioned vaguely ("check the output") without a concrete command, script, or checklist
- Output format is mentioned once in passing but not reinforced
- No distinction between intermediate work products and final deliverables
- No guidance on what to do when verification fails — the agent is left stranded on error

---

### 6. Examples & Anti-patterns

Examples are one of the most powerful teaching tools for an agent. They ground abstract instructions in concrete reality. Anti-patterns (what NOT to do) are equally important.

**Check for:**

- At least one concrete example of correct usage or output
- Input → Output examples where applicable
- Anti-patterns with ❌ / ✅ comparisons showing wrong vs. right approaches
- Examples cover the common case, not just edge cases
- Examples are realistic, not contrived

**Red flags:**

- Zero examples anywhere in the skill
- Examples are too abstract to be actionable
- Only positive examples with no anti-patterns (the agent has no idea what to avoid)
- Examples contradict the instructions
- **Anti-pattern accumulation**: the anti-patterns section grows by adding a new "don't do X" line for each production failure. This is a sign the workflow structure isn't preventing the failures -- patching with more rules doesn't fix a structural problem. If the anti-patterns list is longer than the workflow, something is wrong.

---

### 7. Progressive Disclosure & Size

Skills use a three-level loading system: metadata (always loaded), SKILL.md body (loaded on trigger), and bundled resources (loaded on demand). The SKILL.md body should stay lean and defer heavy reference material to separate files.

**Check for:**

- SKILL.md body is under ~500 lines (ideal for context window management)
- Heavy reference material (API docs, schemas, long code blocks) is in separate files under references/
- Clear pointers from SKILL.md to reference files with guidance on WHEN to read them
- Large reference files (>300 lines) have a table of contents

**Red flags:**

- SKILL.md is over 500 lines with no delegation to reference files
- Enormous inline code blocks that could be bundled scripts
- Reference files exist but SKILL.md never tells the agent when to read them
- Everything crammed into one file when it could be modularized

---

### 8. Error Handling & Edge Cases

Robust skills anticipate what can go wrong and tell the agent how to recover. Without this, the agent hits an error and either halts or guesses.

**Check for:**

- Common failure modes are documented with recovery steps
- Input validation is specified (what happens if the user provides bad input?)
- Fallback behaviors exist when the primary approach fails
- "Common pitfalls" or similar section addressing known gotchas

**Red flags:**

- No mention of what to do when things go wrong
- Assumes happy path always succeeds
- Complex workflows with no error handling at any step
- Known sharp edges in tools/libraries that aren't called out

---

### 9. Writing Quality & Tone

The skill's writing style affects how well the agent follows it. Heavy-handed "MUST/NEVER/ALWAYS" rules work less well than explaining _why_ something matters. The agent is smart — treat it that way.

**Check for:**

- Explains the reasoning behind rules, not just the rules themselves
- Uses imperative form but isn't robotic
- Concise — every sentence earns its place
- Good use of formatting (headers, tables, code blocks) for scannability
- Avoids jargon that isn't defined

**Red flags:**

- Walls of text with no structure
- Excessive MUST/NEVER/ALWAYS in all-caps without explaining why
- Redundant instructions (same thing said multiple ways)
- Condescending tone
- Overly long or rambling sections that dilute the actionable content
- **Hardcoded example lists instead of principles**: listing specific phrases or values (e.g., enumerating "ok", "sure", "sounds good" as soft agreement) instead of defining the category and letting the agent classify. Principles generalize; phrase lists break on the first unlisted phrase.
- **Overly literal guardrails**: "If the user changes direction, start from step 1. If they tweak one parameter, update in place" is brittle. "Re-validate when the plan changes" captures the same intent and generalizes.

---

### 10. Completeness & Gaps

The skill should cover the full lifecycle of the task it enables, from reading input to delivering output. Missing pieces create holes where the agent improvises.

**Check for:**

- Full workflow coverage from trigger to output delivery
- All task variants mentioned in the description are actually addressed in the body
- Dependencies, setup, and cleanup are all covered
- The skill handles the 80% case thoroughly (don't over-index on rare edge cases at the expense of the core flow)

**Red flags:**

- Description promises capabilities the body doesn't deliver
- Core workflow steps are missing (e.g., no output delivery step)
- Setup/installation is assumed but never specified
- Major task variants are mentioned but have no instructions
- **Scope creep into downstream consumers**: the skill references internals of other skills or systems it hands off to (e.g., a planning skill knowing about task launch payload formats). Each skill should produce its output and stop. Downstream details belong in the downstream skill.

---

## Report Template

Use this structure for the evaluation report:

```markdown
# Skill Evaluation: [skill-name]

**Overall Grade: [A / B / C / D]**

> [2-3 sentence executive summary: the skill's biggest strength, biggest weakness, and overall readiness.]

---

## Dimension Scores

| #   | Dimension                           | Rating                       | Key Issue          |
| --- | ----------------------------------- | ---------------------------- | ------------------ |
| 1   | Frontmatter & Triggering            | [Strong/Adequate/Needs Work] | [one-line summary] |
| 2   | Behavioral Focus                    | ...                          | ...                |
| 3   | Workflow Clarity                    | ...                          | ...                |
| 4   | Tool Specification                  | ...                          | ...                |
| 5   | Output Specification & Verification | ...                          | ...                |
| 6   | Examples & Anti-patterns            | ...                          | ...                |
| 7   | Progressive Disclosure & Size       | ...                          | ...                |
| 8   | Error Handling & Edge Cases         | ...                          | ...                |
| 9   | Writing Quality & Tone              | ...                          | ...                |
| 10  | Completeness & Gaps                 | ...                          | ...                |

---

## Detailed Findings

### 1. Frontmatter & Triggering — [Rating]

[Specific findings with evidence from the skill. Quote actual lines. Explain what's good, what's weak, and exactly how to fix it.]

[Repeat for each dimension...]

---

## Priority Fixes

Ranked list of the most impactful improvements, starting with the change that would improve the skill the most:

1. **[Fix title]** — [What to change and why. Be specific enough that the user could implement this without further clarification.]
2. ...
3. ...

---

## Optional Enhancements

Lower-priority suggestions that would elevate the skill further:

- [Enhancement 1]
- [Enhancement 2]
```

## Grading Scale

- **A**: Strong across all dimensions. Minor polish items only. Ready for production use.
- **B**: Solid foundation with a few gaps. An agent can follow it successfully most of the time, but will struggle on certain inputs or edge cases.
- **C**: Functional but has significant structural issues. Works for the happy path but breaks down outside it. Needs a revision pass.
- **D**: Fundamental problems — missing workflows, unclear triggers, or no actionable instructions. Needs a rewrite.

## Evaluation Principles

When evaluating, keep these north stars in mind:

- **An agent reads this at inference time with no prior context.** Every instruction must stand on its own. The skill can't assume the agent "knows what you mean."
- **Specificity beats generality.** "Run `python scripts/validate.py output.docx`" beats "validate the output." Every time.
- **Skills are used millions of times across diverse prompts.** Over-fitted skills that work for one example but fail on variations are worse than slightly loose skills that generalize well.
- **The agent is smart but literal.** It follows what you write, not what you meant. Ambiguity is the #1 cause of skill failures.
- **Good skills look like playbooks, not encyclopedias.** If someone could follow it step-by-step with no domain knowledge and produce a good result, it's a good skill.
- **Good fixes simplify; bad fixes add layers.** If a skill is failing because the agent skips steps, the fix is restructuring the workflow so steps can't be skipped -- not adding more verification steps, checklists, or "GATE" annotations on top. When recommending priority fixes, prefer fixes that reduce complexity and consolidate over fixes that add machinery.
