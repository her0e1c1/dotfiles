---
name: reread
description: Re-read the latest file contents or current state, reconcile them with prior assumptions, and continue the task from the refreshed source of truth.
---

# Reread

## Purpose
Use this skill when the agent needs to stop relying on stale context and read the latest file contents or current state again before continuing.

Typical cases:

- The user says a file was changed and the agent should read it again
- The agent suspects local files changed during the task
- The current file contents may no longer match the agent's earlier understanding
- The user wants the next action based on what is on disk now

## When to use

Use this skill when there is an explicit or implicit signal such as:

- "reread this file"
- "read it again"
- "I changed it, check again"
- "use the latest version"
- visible drift between prior assumptions and the current workspace state

## Core behavior

When this skill is used, treat the latest readable state as the source of truth.

Always do these things:

1. Re-open the relevant files or inspect the current state again
2. Compare the refreshed state with the agent's previous assumptions
3. Separate confirmed changes from inference
4. Update the working assumptions
5. Continue the task from the refreshed understanding

## Operating rules

### 1. Prefer the current state over memory
Do not continue based only on earlier reads if the latest contents may differ.

### 2. Re-read before reacting
If the user says something changed, inspect the actual file or state before proposing the next step.

### 3. Distinguish facts from interpretation
State what was directly observed and what is only inferred from those observations.

### 4. Do not silently keep stale assumptions
If refreshed contents invalidate earlier assumptions, replace them explicitly.

### 5. Resume from the refreshed baseline
Any follow-up explanation, edit, review, or plan should be based on the newly read contents.

## Output format

When helpful, respond in this format:

### Reread Summary
- Refreshed source:
  - ...
- Confirmed changes:
  - ...
- Updated assumptions:
  - ...
- Next action:
  - ...

## Recommended workflow

1. Identify what must be read again
2. Open the smallest relevant set of files or inspect the relevant state
3. Note what changed, what stayed the same, and what is still unclear
4. Update assumptions and constraints
5. Continue the task using the refreshed context

## Heuristics

Interpret the need to reread through these patterns:

- direct edit signal:
  - the user says they changed a file
  - the agent sees a newer diff or different contents
- stale-context risk:
  - the task has evolved across several turns
  - earlier assumptions were tentative
- conflicting evidence:
  - current contents do not match prior summaries
  - current behavior does not match the expected implementation

## Example prompts

- Reread the file and continue
- I changed it by hand, check the latest version
- Use the current contents on disk, not the earlier draft
- Refresh your understanding and proceed

## Example response

### Reread Summary
- Refreshed source:
  - Re-read `config.yaml` and the related loader function
- Confirmed changes:
  - The timeout is now `30s`
  - The fallback path was removed
- Updated assumptions:
  - Timeout handling is now explicit
  - The loader no longer supports the old fallback behavior
- Next action:
  - Update the validation logic and related explanation to match the current file contents

## Anti-patterns

Avoid these mistakes when using this skill:

- continuing from memory without reopening the relevant files
- describing changes without checking the current contents
- mixing observed facts with speculation
- keeping old assumptions after the reread contradicts them

## Success criteria

This skill is successful when:

- the latest relevant contents have been read again
- the agent's assumptions match the current state
- stale context is explicitly corrected
- the next step is based on the refreshed source of truth
