---
name: plan-new-tests
description: Use when Codex needs to read the current implementation code and existing tests, identify likely missing test cases, and produce a concise test-addition plan without editing or running tests.
---

# Plan New Tests

## Overview

Read the implementation first, then compare it with existing tests to find likely gaps. Output only a concise list of missing test-case candidates and a follow-up plan for adding them later.

Read `references/checklist.md` before starting if the codebase has multiple testing styles or the feature area is broad.

## Workflow

1. Read the relevant implementation code and extract major behaviors, branches, boundary conditions, and failure paths.
2. Read the nearest existing tests and identify what behavior is already covered.
3. Compare implementation behavior against test coverage and list likely missing test cases.
4. Remove duplicates and keep the candidate list concise.
5. Produce a short plan for follow-up test additions.
6. State clearly that no tests were added or run.

## Output

Return:

- a concise list of likely missing test cases
- a short follow-up plan for adding those tests later

Do not include implementation edits, fixture edits, or test execution results.

## Guardrails

- Reading implementation code is mandatory.
- Do not mark a case as missing until existing tests have been checked.
- Prefer concise candidate lists over long explanations.
- Treat uncertain cases as candidates, not facts.
- Do not add tests, edit code, or run commands that execute tests.
