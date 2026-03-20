---
name: plan-new-feature
description: Use when Codex needs to inspect a repository broadly, synthesize all available local information, and propose recommended new feature ideas in narrowed and broad sections without implementing anything.
---

# Plan New Feature

## Overview

Inspect the repository broadly, understand what it already does, and propose feature ideas that fit its current shape. Return two sections: one narrowed by feasibility and repo fit, and one broader set of possibilities.

Read `references/checklist.md` before starting when the repository has multiple subsystems or sparse documentation.

## Workflow

1. Read all useful local signals you can find:
   - README files
   - docs
   - config files
   - scripts
   - key entry points
   - recent commits
2. Infer the repository's current purpose, workflows, strengths, and gaps.
3. Generate feature ideas that fit the local context.
4. Organize the output into:
   - `Narrowed Recommendations`
   - `Broader Possibilities`
5. Give each feature a sufficient description for later decision-making.
6. State clearly that this skill proposes ideas only and does not implement them.

## Output

Return:

- `Narrowed Recommendations`: feature ideas filtered by feasibility and repository fit
- `Broader Possibilities`: wider ideas that may be less immediate or less certain

For each feature idea, provide a concise but sufficient description. Do not include detailed implementation plans.

## Guardrails

- Use repository-local evidence, not generic product advice.
- Read broadly before proposing features.
- Prefer concrete feature suggestions over vague strategy statements.
- Separate higher-confidence ideas from wider exploration ideas.
- Do not edit files, implement code, or run tests.
