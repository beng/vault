---
name: skill-eval-loop
description: "Adversarial evaluation of a skill file using an iterative evaluate-then-critique loop. A subagent evaluates the skill against the skill-eval rubric, then the main agent critiques the evaluation using grill-me style questioning. Loops until the evaluation is solid. Use when the user says 'skill-eval-loop', 'deep skill review', 'adversarial skill eval', or wants a thorough, stress-tested evaluation of a SKILL.md file. Do NOT use for creating or fixing skills (use create-skill) or for a quick single-pass evaluation (use skill-eval)."
---

# Skill Eval Loop

Iterative evaluate-then-critique loop for producing a rigorous, stress-tested
skill evaluation. A subagent generates the evaluation; you critique it until
it holds up under scrutiny.

## Input

The user provides a path to a SKILL.md file (or pastes its content).

## Step 1: Dispatch the evaluator subagent

Use the Agent tool to spawn a subagent with this prompt structure:

```
You are a skill file evaluator. Your job is to produce a thorough,
evidence-based evaluation of a Claude agent skill file.

Read the evaluation rubric at:
  ~/.claude/skills/skill-eval/SKILL.md

Read the skill file to evaluate at:
  [USER-PROVIDED PATH]

Follow the rubric exactly. For each of the 10 dimensions, quote specific
lines from the skill as evidence. Do not hand-wave -- every rating must
be justified by what's actually in the file.

Produce the full evaluation report as defined in the rubric's Report
Template section. Include the dimension scores table, detailed findings
for all 10 dimensions, priority fixes, and optional enhancements.

[If this is iteration 2+, append:]

Previous evaluation feedback to address:
[PASTE YOUR CRITIQUE POINTS FROM THE PREVIOUS ROUND]

Revise your evaluation to address each critique point. If you disagree
with a critique, explain why with evidence from the skill file.
```

## Step 2: Critique the evaluation

When the subagent returns the evaluation report, apply grill-me style
scrutiny. Walk through the report and challenge it on these axes:

**Evidence quality**:
- Does every rating cite specific lines/sections from the skill?
- Are "Strong" ratings actually earned, or just "nothing obviously wrong"?
- Are "Needs Work" ratings identifying real problems or nitpicking?

**Consistency**:
- Do the dimension ratings align with the overall grade?
- Do the priority fixes match the dimensions rated "Needs Work"?
- Are there contradictions (e.g., praising clarity in one section,
  flagging ambiguity in another for the same content)?

**Completeness**:
- Did the evaluator actually cover all 10 dimensions with substance?
- Are any dimensions given one-sentence treatment while others get paragraphs?
- Did the evaluator miss obvious issues you can see in the skill file?

**Calibration**:
- Would a different skill with similar qualities get the same grade?
- Is the evaluator grading on a curve or against the rubric's actual criteria?
- Are the "optional enhancements" actually critical fixes in disguise?

**Actionability**:
- Could someone implement each priority fix without asking follow-up questions?
- Are fixes specific ("add negative triggers for X and Y") or vague
  ("improve the description")?

**Fix quality**:
- Do priority fixes simplify or add complexity? A fix that adds more
  verification steps, checklists, or gate annotations on top of a broken
  workflow is itself overfitting. Good fixes restructure and consolidate.
- Do fixes address root causes or symptoms? Adding "don't skip validation"
  as an anti-pattern treats the symptom. Inlining validation into the
  workflow step treats the root cause.
- Would the fixes generalize, or do they patch one specific failure case?

Write down every issue you find. Be specific -- quote the evaluation report.

## Step 3: Decide whether to loop

**Exit criteria -- all must be true:**
1. Every dimension rating is backed by quoted evidence from the skill
2. No contradictions between sections
3. Priority fixes are specific and actionable
4. The overall grade is justified by the dimension scores
5. No dimensions were treated superficially (one-liner analysis)

If all criteria are met, go to Step 4.

If any criterion fails, go back to Step 1 with your critique points
appended. Maximum 3 iterations -- if it hasn't converged by round 3,
present the best version with a note on what's still shaky.

## Step 4: Present the final evaluation

Present the final evaluation report to the user. Include:
- The evaluation report (clean, no critique trail)
- Number of iterations it took
- If applicable, any residual concerns that didn't fully resolve

## Anti-patterns

- Don't rubber-stamp the first evaluation. The whole point is adversarial
  pressure. Round 1 almost always has gaps.
- Don't critique style/formatting of the report -- focus on substance.
  The question is "is this evaluation accurate and useful?" not "is this
  report pretty?"
- Don't let the subagent get away with "Adequate" as a default rating.
  Adequate means "works but has gaps" -- that should be backed by what
  the gaps are.
- Don't loop more than 3 times. Diminishing returns. If it's not converging,
  the skill file itself may be too ambiguous to evaluate cleanly -- say so.
- Don't recommend fixes that add complexity to fix complexity. If a skill
  has floating sections and redundant descriptions, the fix is consolidation,
  not more annotations. Watch for this in the subagent's priority fixes.
