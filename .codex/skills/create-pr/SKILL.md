---
name: create-pr
description: Use when the agent should create a GitHub pull request for the current branch and the work should stay limited to the minimum `git` and `gh` actions needed to publish it.
---

# Create PR

## Overview

Turn the current branch into a GitHub pull request with a tight scope. Use only `git` and `gh`, do only the work needed to get the branch published, and treat `git add`, `git commit`, `git push`, and `gh pr create` as the allowed path to completion.

## Guardrails

- Use `git` and `gh` only.
- Keep the task scoped to PR creation. Do not run tests, refactors, formatting passes, dependency changes, or unrelated cleanup.
- `git add`, `git commit`, and `git push` are allowed when they are needed to publish the PR.
- Do not rewrite existing history, restack branches, or reorganize unrelated local work unless the user explicitly asks.
- If local changes are ambiguous, stop and ask instead of trying to be clever.

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
   - use `git add` only for changes that clearly belong in this PR
   - preserve existing history
   - append only the new commits needed to publish the branch
   - prefer the minimum commit work needed over commit cleanup
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

- `git add` only the changes that clearly belong in the PR.
- Preserve the current branch history.
- Add only the new commits needed for PR-ready history.
- Generate a practical commit message and avoid unnecessary commit splitting.
- Do not rewrite prior commits unless the user separately asks for history cleanup.

### Scope Discipline

- Stay on the narrow path: inspect Git state, add related changes, commit if needed, push, and create the PR.
- Do not do extra repository maintenance just because it might be helpful.
- Do not expand the task into validation, cleanup, or follow-up implementation work.

## Stop Conditions

Stop and explain the blocker when any of the following hold:

- detached `HEAD`
- no suitable remote for pushing
- `gh` is unavailable
- push fails due to permissions, authentication, or remote state
- base branch inference remains ambiguous
- local changes contain ambiguous scope that cannot be safely inferred

If PR creation fails after the title and body have already been generated, return the chosen base branch, the generated title, the generated body, and the next manual command to run.
