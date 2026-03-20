---
name: confirm-typo
description: Use when the agent is about to act on user input that appears to contain a typo and one correction candidate is strongly favored, so the agent should confirm the intended wording before proceeding.
---

# Confirm Typo

## Overview

Pause only for high-confidence typo corrections. Treat the full user message as one utterance, ask a short confirmation question with one correction candidate, and continue only after approval.

## Workflow

1. Read the full user utterance in context.
2. Identify wording that appears unnatural or misspelled in the current context.
3. Generate correction candidates, prioritizing existing names in the current working directory or repository, then nearby known identifiers, then common commands, terms, and skill names.
4. Ask for confirmation only when one candidate is clearly stronger than the alternatives and makes the full request read naturally.
5. Phrase the question with one concrete correction only.
6. If the user approves, continue the original request using the corrected interpretation.
7. If the user rejects the correction, keep the original wording and do not push the alternative again unless the user provides new information.

## Confidence Rules

Ask for confirmation only when:

- the suggested correction is materially more natural than the original wording
- the candidate set effectively collapses to one strong option
- the corrected wording fits the full request cleanly
- competing candidates are weak enough that asking about one correction is not misleading

Avoid confirmation when:

- multiple candidates remain similarly plausible
- the text could reasonably be an intentional name, project term, or arbitrary identifier
- the issue looks like style, abbreviation, or phrasing preference rather than a likely typo
- the proposed correction would substantially change the likely meaning of the request

## Confirmation Style

Use concise prompts such as:

- `touch README.md` でいいですか?
- `README.md を作成` の意味でいいですか?
- `brainstorming` skill のことですか?

Ask before acting. Present one correction only. Avoid long explanations and confidence percentages.

## Approval Handling

- If the user approves, continue with the corrected interpretation.
- If the user rejects the suggestion, respect the original wording.
- If the user provides a different correction, use the user-provided wording instead.
- Until approval is received, do not execute the corrected version.
