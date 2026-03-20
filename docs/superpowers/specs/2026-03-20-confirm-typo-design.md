# Confirm Typo Design

**Goal:** Define a reusable Codex skill named `confirm-typo` that pauses only for high-confidence typo corrections, asks the user to confirm the intended wording, and then continues with the corrected interpretation after approval.

## Problem

Codex can misinterpret user intent when a request contains a typo in natural language, file names, command arguments, or skill names. Blindly executing the literal input creates the wrong artifact, while over-eager typo handling causes unnecessary interruptions. The useful middle ground is a skill that asks for confirmation only when a correction is both plausible and specific.

## Scope

The skill named `confirm-typo` should:

- Trigger when Codex is about to act on user input that appears to contain a typo and a single correction candidate is strongly favored.
- Treat the user input as one utterance instead of splitting it into separate natural-language and command-token passes.
- Pause before execution or interpretation changes when a typo correction would alter what Codex does.
- Ask a short confirmation question that presents exactly one correction candidate.
- Resume the original task with the corrected interpretation only after the user approves.

The skill should not:

- Auto-correct without confirmation.
- Stop on every unusual string or invented name.
- Ask confirmation when multiple plausible corrections compete.
- Replace the user's wording after the user rejects the correction.

## Triggering Conditions

The description should target situations such as:

- A command refers to a likely misspelled existing file, such as `READNE.md` when `README.md` exists nearby.
- A natural-language request contains a likely misspelling whose intended correction is obvious in context.
- A skill, command, or common term is misspelled in a way that strongly points to one known candidate.

The description should emphasize that the skill is for high-confidence confirmation before acting, not for generic spell-checking.

## Core Workflow

The skill should instruct Codex to follow this order:

1. Read the full user utterance in context.
2. Identify wording that appears unnatural or misspelled in the current context.
3. Generate correction candidates, prioritizing names that exist in the current working directory or repository, then nearby known identifiers, then common commands, terms, and skill names.
4. Ask for confirmation only when one candidate is clearly stronger than the alternatives and makes the full request read naturally.
5. Phrase the question with one concrete correction only.
6. If the user approves, continue the original request using the corrected interpretation.
7. If the user rejects the correction, keep the original wording and do not push the alternative again unless the user provides new information.

## Confidence Rules

The skill should ask for confirmation only when all of the following are true:

- The suggested correction is materially more natural than the original wording.
- The candidate set effectively collapses to one strong option.
- The corrected wording fits the full request cleanly.
- Competing candidates are weak enough that asking about one correction is not misleading.

The skill should avoid confirmation when any of the following are true:

- Multiple candidates remain similarly plausible.
- The text could reasonably be an intentional name, project term, or arbitrary identifier.
- The issue looks like style, abbreviation, or phrasing preference rather than a likely typo.
- The proposed correction would substantially change the likely meaning of the request.

## Confirmation Style

The skill should require concise confirmation prompts such as:

- `` `touch README.md` でいいですか? ``
- `` `README.md を作成` の意味でいいですか? ``
- `` `brainstorming` skill のことですか? ``

The prompt should:

- present one correction only
- avoid long explanations
- avoid confidence percentages
- ask before acting

## Approval and Rejection Handling

After a confirmation prompt:

- If the user approves with a clear affirmative response, continue with the corrected interpretation.
- If the user rejects the suggestion, respect the original wording.
- If the user supplies a different correction, use the user-provided wording instead.
- Until approval is received, do not execute the corrected version.

## Output and Behavior Guardrails

The skill should emphasize these rules:

- Confirmation is a gate before action, not a side comment after action.
- High precision matters more than high recall.
- Default to not interrupting when certainty is weak.
- Never claim that the user meant the correction unless the user confirms it.
- Keep the interaction short so the original task can resume immediately after confirmation.

## Skill Packaging

The first version should use a minimal structure:

- `.codex/skills/confirm-typo/SKILL.md`
- `.codex/skills/confirm-typo/agents/openai.yaml`

No scripts, assets, or reference files are required initially because the behavior is prompt- and judgment-driven rather than tool-driven.

## SKILL.md Content Direction

`SKILL.md` should stay concise and include:

- frontmatter with `name` and `description` only
- a short overview of the confirmation-first behavior
- the candidate selection order
- the confidence rules for when to interrupt
- the approval and rejection handling rules
- short example confirmation prompts

## Agent Metadata

`agents/openai.yaml` should include:

- a user-facing display name
- a short UI description
- a default prompt that explicitly mentions `$confirm-typo`

Icons and brand colors are unnecessary unless provided separately.

## Validation Strategy

Implementation should validate the skill folder structure and frontmatter with the local validation script if available. If the validation script cannot run in the environment, record that limitation and perform a manual structural review instead.

Forward-testing is recommended after the first implementation by giving fresh agents realistic typo-containing requests and checking whether they ask for confirmation only in high-confidence cases.
