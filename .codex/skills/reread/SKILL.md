---
name: reread
description: Use before editing files. Re-read the latest contents and diff, understand user-made manual changes, and avoid overwriting them unless explicitly requested.
---

# Reread

## Purpose
Refresh your understanding from the current files on disk and preserve intentional user edits.

## When to use
- Before modifying files

## Behavior
- Re-read the relevant files
- Check the current diff before editing
- Treat the latest state as the source of truth
- Understand and preserve user-made manual changes
- Do not overwrite manual edits unless the user explicitly asks
- Continue from the refreshed state
