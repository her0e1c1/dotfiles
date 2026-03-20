# Plan New Tests Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add a reusable `plan-new-tests` skill that teaches Codex to read implementation code and current tests, then list likely missing test cases and a short follow-up plan without changing or running tests.

**Architecture:** Keep the canonical skill under `.codex/skills/plan-new-tests/` with a concise `SKILL.md`, a reusable checklist reference, and UI metadata in `agents/openai.yaml`. Expose the same skill to Claude, Copilot, and Gemini via symlinks so the instructions stay in one place.

**Tech Stack:** Markdown, YAML, existing local skill-authoring scripts

---

### Task 1: Scaffold the Skill Folder

**Files:**
- Create: `.codex/skills/plan-new-tests/SKILL.md`
- Create: `.codex/skills/plan-new-tests/agents/openai.yaml`
- Create: `.codex/skills/plan-new-tests/references/checklist.md`
- Create: `.claude/skills/plan-new-tests` (symlink)
- Create: `.copilot/skills/plan-new-tests` (symlink)
- Create: `.gemini/skills/plan-new-tests` (symlink)

- [ ] **Step 1: Create the skill scaffold**

Run: `python3 .codex/skills/.system/skill-creator/scripts/init_skill.py plan-new-tests --path .codex/skills --resources references --interface display_name='Plan New Tests' --interface short_description='Find missing test cases from code' --interface default_prompt='Use $plan-new-tests to read the implementation and current tests, then list likely missing test cases and a short follow-up plan.'`

Expected: New `.codex/skills/plan-new-tests/` directory exists with starter files.

- [ ] **Step 2: Review generated files before editing**

Run: `find .codex/skills/plan-new-tests -maxdepth 3 -type f | sort`

Expected: `SKILL.md`, `agents/openai.yaml`, and `references/` directory are present.

### Task 2: Write the Skill Guidance

**Files:**
- Modify: `.codex/skills/plan-new-tests/SKILL.md`
- Modify: `.codex/skills/plan-new-tests/references/checklist.md`

- [ ] **Step 1: Replace template frontmatter and body in `SKILL.md`**

Include:
- trigger-focused description starting with `Use when`
- concise overview
- workflow for reading implementation code, comparing it against current tests, and producing missing-test candidates
- explicit instruction not to add tests or run tests
- reference to `references/checklist.md`

- [ ] **Step 2: Write `references/checklist.md`**

Include:
- how to read implementation behavior first
- how to find nearby tests and compare coverage
- how to consolidate likely missing cases
- how to write a concise follow-up plan without execution

### Task 3: Validate and Review

**Files:**
- Review: `.codex/skills/plan-new-tests/SKILL.md`
- Review: `.codex/skills/plan-new-tests/agents/openai.yaml`
- Review: `.codex/skills/plan-new-tests/references/checklist.md`

- [ ] **Step 1: Run the local validator if dependencies allow it**

Run: `python3 .codex/skills/.system/skill-creator/scripts/quick_validate.py .codex/skills/plan-new-tests`

Expected: Validation passes. If it fails because `yaml` is unavailable, record that blocker explicitly.

- [ ] **Step 2: Perform manual structural review**

Check:
- frontmatter has only `name` and `description`
- description is trigger-oriented, not workflow-oriented
- `default_prompt` explicitly mentions `$plan-new-tests`
- no unnecessary files or scripts were added
- reference file is linked from `SKILL.md`

- [ ] **Step 3: Summarize validation status**

Report:
- whether scaffolding succeeded
- whether validation script ran
- what manual checks were performed
