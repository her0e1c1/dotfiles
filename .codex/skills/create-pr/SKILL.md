---
name: create-pr
description: Use when Codex should create a GitHub pull request for the current branch and needs to infer the right base branch or decide whether local uncommitted changes belong in that PR.
---

# Create PR

## Overview

Turn the current branch into a GitHub pull request end to end. Inspect the Git state, infer the base branch when one candidate is clearly favored, include only clearly related local changes, then generate the PR title and body from the branch diff before running `gh pr create`.

## Workflow

1. Read the current Git context:
   - branch name and tracking branch
   - staged, unstaged, and untracked changes
   - remote availability and `gh` availability
2. Determine the base branch:
   - check `git config branch.<current-branch>.base` first and prefer it when set
   - compare likely candidates such as `main`, `master`, and `develop`
   - prefer merge-base and fork-point evidence
   - use upstream and recent branch context only as secondary signals
   - ask the user only when one strong candidate does not emerge
3. Review uncommitted changes:
   - include only work that clearly belongs in this PR
   - keep unrelated local changes untouched
   - stop and ask the user if the boundary is ambiguous
4. Create any needed commits:
   - preserve existing history
   - append only the new commits needed
   - split into multiple commits when the change groups are distinct
5. Summarize `base...HEAD` and identify the branch's main purpose.
6. Generate PR metadata:
   - adapt to a repository PR template if one exists
   - otherwise use a compact body with `## Summary` and `## Testing`
7. Push the current branch, setting upstream if needed.
8. Run `gh pr create`.
9. Return a concise result summary with the chosen base branch, any new commits, the PR title, and the PR URL.

## Decision Rules

### Base Branch

- Prefer `git config branch.<current-branch>.base` when it exists.
- Prefer merge-base and fork-point relationships with common integration branches.
- Use the configured upstream or remote default branch only when that evidence matches the commit graph.
- Treat recent branch history or naming as a tie-breaker, not primary proof.
- Ask the user when multiple candidates remain similarly plausible.

### Uncommitted Changes

- Inspect staged, unstaged, and untracked changes before committing anything.
- Decide by branch purpose, not by file state alone.
- Include changes that clearly belong in the PR.
- Leave unrelated changes in the working tree.
- Ask the user when a change could plausibly belong either in this PR or in separate follow-up work.

### Commit Creation

- Preserve the current branch history.
- Add only the new commits needed for PR-ready history.
- Generate commit messages from the grouped changes and split commits only when the work naturally separates.
- Do not rewrite prior commits unless the user separately asks for history cleanup.

## Stop Conditions

Stop and explain the blocker when any of the following hold:

- detached `HEAD`
- no suitable remote for pushing
- `gh` is unavailable
- push fails due to permissions, authentication, or remote state
- base branch inference remains ambiguous
- local changes contain ambiguous scope that cannot be safely inferred

If PR creation fails after the title and body have already been generated, return the chosen base branch, the generated title, the generated body, and the next manual command to run.
