---
name: commit
description: Review the current staged git diff, generate a concise commit message from staged changes only, and run git commit. Use when the user asks to commit staged changes or write a commit message for the staged diff.
---

# Commit

Use this skill when the task is to create a commit message from the current staged diff, and usually to run `git commit`.

## Workflow

1. Inspect staged changes only.
   - Run `git status --short` to confirm what is staged.
   - Run `git diff --cached --stat` for a compact summary.
   - Run `git diff --cached` for the actual staged patch.
2. Ignore unstaged and untracked changes completely unless the user explicitly asks about them.
3. Write a commit message that reflects only the staged diff.
   - Prefer an imperative subject.
   - Keep the subject within 72 characters.
   - Add a body only when notable user-facing or maintenance-impacting details need clarification.
4. Run `git commit` with the generated message unless the user asked for message drafting only.

## Guardrails

- Do not invent changes that are not present in the staged diff.
- Do not mention unstaged changes in the commit message.
- If nothing is staged, report that clearly and do not run `git commit`.
- If `git commit` is blocked by sandbox or permissions, retry with the required escalation rather than stopping at the draft message.
- If hooks or Git errors block the commit, report the exact blocker and avoid rewriting the message unless the failure requires it.
