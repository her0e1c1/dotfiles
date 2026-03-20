# Plan New Tests Checklist

## Read the implementation

- Identify the target feature, module, or command path first.
- Read the implementation code before reviewing tests.
- Note major success paths, branching behavior, edge conditions, invalid inputs, and failure handling.

## Read existing tests

- Find the nearest existing test files for the same feature or module.
- Check what behaviors are already covered before proposing anything new.
- Prefer nearby tests over distant shared examples when judging local coverage.

## Identify missing cases

- Compare implementation behavior against current coverage.
- Look for uncovered branches, boundary values, error paths, and important input variants.
- Merge duplicates and keep only the cases that still look plausibly uncovered after reading tests.
- Keep the list short enough to be actionable.

## Produce the plan

- Return a concise candidate list of missing test cases.
- Add a short follow-up plan describing how to approach the later test-writing work.
- State clearly that no tests were added or run.

## Avoid

- Do not propose implementation or fixture changes in this skill.
- Do not claim a case is definitely missing if coverage is ambiguous.
- Do not execute tests or leave fabricated verification status.
