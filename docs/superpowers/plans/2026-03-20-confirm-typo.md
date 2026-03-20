# Confirm Typo Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add a reusable `confirm-typo` skill that interrupts only for high-confidence typo corrections, asks a short confirmation question, and continues with the corrected interpretation only after user approval.

**Architecture:** Keep the skill under `.codex/skills/confirm-typo/` with a concise `SKILL.md` and UI metadata in `agents/openai.yaml`. Reuse the existing shared `.codex/skills` directory so Claude, Copilot, and Gemini see the same skill automatically through the repo-wide symlinks that already exist.

**Tech Stack:** Markdown, YAML, existing local skill-authoring scripts

---

### Task 1: Scaffold the Skill Folder

**Files:**
- Create: `.codex/skills/confirm-typo/SKILL.md`
- Create: `.codex/skills/confirm-typo/agents/openai.yaml`

- [ ] **Step 1: Create the skill scaffold**

Run: `python3 .codex/skills/.system/skill-creator/scripts/init_skill.py confirm-typo --path .codex/skills --interface display_name='Confirm Typo' --interface short_description='Confirm likely typos before acting' --interface default_prompt='Use $confirm-typo when a user request appears to contain a high-confidence typo and you should confirm the intended wording before acting.'`

Expected: New `.codex/skills/confirm-typo/` directory exists with `SKILL.md` and `agents/openai.yaml`.

- [ ] **Step 2: Review generated files before editing**

Run: `find .codex/skills/confirm-typo -maxdepth 3 -type f | sort`

Expected: `SKILL.md` and `agents/openai.yaml` are present and no extra resource directories were created.

### Task 2: Write the Skill Guidance

**Files:**
- Modify: `.codex/skills/confirm-typo/SKILL.md`

- [ ] **Step 1: Replace the generated frontmatter**

Set the frontmatter to exactly:

```yaml
---
name: confirm-typo
description: Use when Codex is about to act on user input that appears to contain a typo and one correction candidate is strongly favored, so Codex should confirm the intended wording before proceeding.
---
```

- [ ] **Step 2: Write the overview and workflow**

Replace the template body with a concise structure that includes:

```md
# Confirm Typo

## Overview

Pause only for high-confidence typo corrections. Treat the full user message as one utterance, ask a short confirmation question with one correction candidate, and continue only after approval.

## Workflow

1. Read the full user utterance in context.
2. Identify wording that appears unnatural or misspelled in the current context.
3. Generate correction candidates, prioritizing existing names in the current working directory or repository, then nearby known identifiers, then common commands, terms, and skill names.
4. Ask for confirmation only when one candidate is clearly stronger than the alternatives and makes the full request read naturally.
5. Phrase the question with one concrete correction only.
6. If the user approves, continue the original request using the corrected interpretation.
7. If the user rejects the correction, keep the original wording and do not push the alternative again unless the user provides new information.
```

- [ ] **Step 3: Add guardrails and examples**

Append sections that include these exact ideas:

```md
## Confidence Rules

Ask for confirmation only when:

- the suggested correction is materially more natural than the original wording
- the candidate set effectively collapses to one strong option
- the corrected wording fits the full request cleanly
- competing candidates are weak enough that asking about one correction is not misleading

Avoid confirmation when:

- multiple candidates remain similarly plausible
- the text could reasonably be an intentional name, project term, or arbitrary identifier
- the issue looks like style, abbreviation, or phrasing preference rather than a likely typo
- the proposed correction would substantially change the likely meaning of the request

## Confirmation Style

Use concise prompts such as:

- `touch README.md` でいいですか?
- `README.md を作成` の意味でいいですか?
- `brainstorming` skill のことですか?

## Approval Handling

- If the user approves, continue with the corrected interpretation.
- If the user rejects the suggestion, respect the original wording.
- If the user provides a different correction, use the user-provided wording instead.
- Until approval is received, do not execute the corrected version.
```

Expected: `SKILL.md` is concise, trigger-oriented, and matches the approved spec without adding scripts or references.

### Task 3: Verify Metadata and Packaging

**Files:**
- Modify: `.codex/skills/confirm-typo/agents/openai.yaml`
- Review: `.claude/skills`
- Review: `.copilot/skills`
- Review: `.gemini/skills`

- [ ] **Step 1: Check generated UI metadata**

Open `.codex/skills/confirm-typo/agents/openai.yaml` and ensure it matches this shape:

```yaml
interface:
  display_name: "Confirm Typo"
  short_description: "Confirm likely typos before acting"
  default_prompt: "Use $confirm-typo when a user request appears to contain a high-confidence typo and you should confirm the intended wording before acting."
```

Expected: `default_prompt` explicitly mentions `$confirm-typo` and the copy matches the skill's trigger behavior.

- [ ] **Step 2: Regenerate metadata only if the scaffold output differs**

Run only if `agents/openai.yaml` does not match the expected content:

`python3 .codex/skills/.system/skill-creator/scripts/generate_openai_yaml.py .codex/skills/confirm-typo --interface display_name='Confirm Typo' --interface short_description='Confirm likely typos before acting' --interface default_prompt='Use $confirm-typo when a user request appears to contain a high-confidence typo and you should confirm the intended wording before acting.'`

Expected: `agents/openai.yaml` matches the expected values exactly.

- [ ] **Step 3: Confirm cross-agent exposure already works**

Run: `ls -ld .claude/skills .copilot/skills .gemini/skills`

Expected: Each path is a symlink to `../.codex/skills`, so no extra integration work is needed for the new skill.

### Task 4: Validate and Review

**Files:**
- Review: `.codex/skills/confirm-typo/SKILL.md`
- Review: `.codex/skills/confirm-typo/agents/openai.yaml`

- [ ] **Step 1: Run the local validator**

Run: `python3 .codex/skills/.system/skill-creator/scripts/quick_validate.py .codex/skills/confirm-typo`

Expected: Validation passes. If it fails because a Python dependency such as `yaml` is unavailable, record that blocker explicitly before continuing.

- [ ] **Step 2: Perform manual structural review**

Check all of the following:

- frontmatter contains only `name` and `description`
- the description is trigger-oriented, not a workflow summary
- `SKILL.md` treats the user input as one utterance
- the candidate-priority order matches the spec
- the skill asks only when one candidate is strongly favored
- approval and rejection handling are both explicit
- no `references/`, `scripts/`, or unnecessary docs were added

Expected: The skill matches the spec and remains minimal.

- [ ] **Step 3: Run a lightweight behavior review against the text**

Manually compare the written skill against these examples:

- `touch READNE.md` should lead to a confirmation for `README.md` if that file exists nearby
- `brainstorning skill を使って` should lead to a confirmation for `brainstorming`
- `touch qzrmx.md` should not trigger confirmation if the name could be intentional

Expected: The wording of the skill supports these outcomes without requiring extra scripts or heuristics files.

- [ ] **Step 4: Summarize validation status**

Report:

- whether scaffolding succeeded
- whether `openai.yaml` needed regeneration
- whether the validator ran successfully
- what manual checks were performed
