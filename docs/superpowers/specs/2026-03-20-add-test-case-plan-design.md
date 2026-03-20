# Add Test Case Plan Design

**Goal:** Define a generic Codex skill that adds new test cases by imitating repository-local test patterns, with optional minimal implementation or fixture changes, while leaving test execution to a separate verification-oriented skill.

## Problem

Codex often needs to add tests in unfamiliar repositories. Generic advice is usually too abstract, while framework-specific guidance is too narrow. The useful middle ground is a reusable skill that teaches Codex to discover and follow the repository's existing testing style before adding a new case.

## Scope

The skill named `add-test-case-plan` should:

- Trigger when Codex needs to add a new test case by studying existing tests in the current repository.
- Work across languages and test frameworks.
- Prefer the nearest existing test conventions over generic best practices.
- Permit the smallest necessary implementation-code or fixture changes when the new test cannot be expressed otherwise.
- Stop short of running tests.
- End with a concrete verification plan describing which commands should be run later and what they should confirm.

The skill should not:

- Introduce a new testing style when the repository already has one.
- Claim test results or imply verification was performed.
- Expand into broad refactors unrelated to the new test case.
- Act as a test execution or debugging skill.

## Triggering Conditions

The skill description should target situations such as:

- Adding a missing regression or edge-case test.
- Extending an existing test file with one more scenario.
- Following nearby repository conventions instead of inventing a fresh pattern.
- Making minimal supporting changes to fixtures or implementation code so the new test case is representable.

The description should focus on when to use the skill, not summarize the workflow.

## Core Workflow

The skill should instruct Codex to follow this order:

1. Locate the relevant feature area and the closest existing tests.
2. Read multiple nearby tests and extract local conventions:
   - file placement
   - test naming
   - setup and teardown style
   - fixture use
   - assertion style
   - helper and factory patterns
3. State the candidate test scenarios briefly and choose the one to add now.
4. Add the new test by reusing the observed local pattern.
5. If required, make the smallest possible implementation or fixture adjustment needed for that test case.
6. Leave a verification plan that names the commands to run later and the behaviors those commands should confirm.
7. State clearly that tests were not run.

## Guardrails

The skill should emphasize these rules:

- Repository-local patterns outrank generic preferences.
- When nearby tests disagree, prefer the closest tests in the same area over distant global examples.
- Keep the change set narrow and directly tied to the new test case.
- Do not clean up unrelated tests, helpers, or production code opportunistically.
- If the intended behavior is ambiguous, articulate the scenario before editing.
- Never fabricate execution or passing status.

## Skill Packaging

The skill should use a two-layer structure:

- `SKILL.md` for triggers, workflow, and guardrails.
- `references/checklist.md` for the operational checklist used while adding a test case.

The skill folder should be:

- `.codex/skills/add-test-case-plan/`

Expected files:

- `.codex/skills/add-test-case-plan/SKILL.md`
- `.codex/skills/add-test-case-plan/references/checklist.md`
- `.codex/skills/add-test-case-plan/agents/openai.yaml`

No scripts are required for the first version because the problem is primarily judgment-driven and language-agnostic.

## SKILL.md Content Direction

`SKILL.md` should stay concise and include:

- frontmatter with `name` and `description` only
- a short overview
- the main workflow
- decision rules for minimal implementation or fixture edits
- explicit instruction to leave test execution to another skill
- guidance to read `references/checklist.md` when performing the work

## Checklist Reference Direction

`references/checklist.md` should provide a compact reusable checklist for:

- finding the nearest relevant tests
- comparing multiple examples before copying a pattern
- deciding where the new test belongs
- choosing names and assertions that match local style
- deciding whether a fixture or implementation change is truly necessary
- recording an honest verification plan without running anything

## Agent Metadata

`agents/openai.yaml` should include:

- a user-facing display name
- a short UI description
- a default prompt that explicitly mentions `$add-test-case-plan`

Icons and brand colors are unnecessary unless provided separately.

## Validation Strategy

Implementation should validate the skill folder structure and frontmatter with the local validation script if dependencies allow it. If the validation script is unavailable in the environment, record that blocker explicitly and perform a manual structural review instead.

Forward-testing with subagents is optional for later iterations, but not required for the first version of this skill.
