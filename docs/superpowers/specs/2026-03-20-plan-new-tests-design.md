# Plan New Tests Design

**Goal:** Define a generic Codex skill that reads implementation code and existing tests, identifies likely missing test cases, and produces a concise follow-up plan without editing or running tests.

## Problem

Codex often needs to inspect unfamiliar repositories and determine which tests are still missing. Generic advice is too abstract, while framework-specific guidance is too narrow. The useful middle ground is a reusable skill that teaches Codex to read the implementation, compare it against the current tests, and produce a concrete test-planning output.

## Scope

The skill named `plan-new-tests` should:

- Trigger when Codex needs to read implementation code and current tests to identify likely missing test cases.
- Work across languages and test frameworks.
- Require implementation-code review as an input, not just test review.
- Stop short of editing tests, implementation code, or fixtures.
- Stop short of running tests.
- End with a concise candidate list and a short follow-up plan for later test-writing work.

The skill should not:

- Add or modify tests directly.
- Propose code or fixture edits as part of its output.
- Claim test results or imply verification was performed.
- Expand into debugging, refactoring, or execution work.

## Triggering Conditions

The skill description should target situations such as:

- Reading a feature implementation and deciding what tests are still missing.
- Comparing existing tests against current code paths.
- Producing a candidate list of uncovered branches, edge cases, or failure paths.
- Preparing a short plan for a later test-writing task.

The description should focus on when to use the skill, not summarize the workflow.

## Core Workflow

The skill should instruct Codex to follow this order:

1. Locate the relevant feature area and read the implementation code first.
2. Extract major behaviors, branches, boundary conditions, and failure paths from the implementation.
3. Read nearby existing tests and identify the behavior already covered.
4. Compare implementation behavior against test coverage and list likely missing test cases.
5. Remove duplicates and keep the candidate list concise.
6. Produce a short follow-up plan for later test additions.
7. State clearly that no tests were added or run.

## Guardrails

The skill should emphasize these rules:

- Reading implementation code is mandatory.
- Existing tests must be checked before a case is labeled missing.
- Nearby tests outrank distant examples when judging local coverage.
- Uncertain gaps should be phrased as candidates, not facts.
- Never fabricate execution or passing status.

## Skill Packaging

The skill should use a two-layer structure:

- `SKILL.md` for triggers, workflow, and guardrails.
- `references/checklist.md` for the operational checklist used while identifying missing test cases.

The skill folder should be:

- `.codex/skills/plan-new-tests/`

Expected files:

- `.codex/skills/plan-new-tests/SKILL.md`
- `.codex/skills/plan-new-tests/references/checklist.md`
- `.codex/skills/plan-new-tests/agents/openai.yaml`

Cross-agent exposure should reuse the same canonical skill via symlinks rather than duplicating content:

- `.claude/skills/plan-new-tests` -> `.codex/skills/plan-new-tests`
- `.copilot/skills/plan-new-tests` -> `.codex/skills/plan-new-tests`
- `.gemini/skills/plan-new-tests` -> `.codex/skills/plan-new-tests`

No scripts are required for the first version because the problem is primarily judgment-driven and language-agnostic.

## SKILL.md Content Direction

`SKILL.md` should stay concise and include:

- frontmatter with `name` and `description` only
- a short overview
- the main workflow for comparing implementation behavior with existing tests
- explicit instruction not to add tests or run them
- guidance to read `references/checklist.md` when performing the work

## Checklist Reference Direction

`references/checklist.md` should provide a compact reusable checklist for:

- reading the implementation first
- finding the nearest relevant tests
- comparing code paths against current test coverage
- consolidating likely missing cases
- recording a concise follow-up test plan without running anything

## Agent Metadata

`agents/openai.yaml` should include:

- a user-facing display name
- a short UI description
- a default prompt that explicitly mentions `$plan-new-tests`

Icons and brand colors are unnecessary unless provided separately.

## Validation Strategy

Implementation should validate the skill folder structure and frontmatter with the local validation script if dependencies allow it. If the validation script is unavailable in the environment, record that blocker explicitly and perform a manual structural review instead.

Forward-testing with subagents is optional for later iterations, but not required for the first version of this skill.
