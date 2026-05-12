---
name: summarize-repository
description: Use when the user asks to inspect, summarize, diagram, or generate documentation for a repository's structure, workflows, APIs, data model, runtime, configuration, features, or tests.
---

# Summarize Repository

## Overview

Inspect the repository broadly, infer its current structure from local evidence, and regenerate `docs/summary/` as a clean snapshot of the codebase.

Read [references/outputs.md](references/outputs.md) before writing the summary files.

## Workflow

1. Detect the repository's documentation language from existing docs such as `README`, `docs/`, or contribution guides.
2. If the dominant documentation language is unclear, ask the user before generating output.
3. Read the repository broadly enough to understand:
   - purpose and major workflows
   - entry points and runtime boundaries
   - data stores and schema sources
   - API surfaces
   - major features and representative use cases
4. Recreate `docs/summary/` from the current repository state instead of incrementally preserving old summary files.
5. Always produce a repository overview document.
6. Use `references/outputs.md` as the source of truth for supported output artifacts and produce additional documents only when the repository contains enough evidence to support them.
7. Mark uncertainty explicitly. Do not invent endpoints, entities, components, or flows that are not grounded in local evidence.

## Output Rules

- Write into `docs/summary/`.
- Treat the generated files as a fresh snapshot of the repository at the time of analysis.
- Use the stable filenames and generation conditions in `references/outputs.md`.
- Prefer Mermaid for diagrams so the output stays text-native and reviewable.
- Keep prose concise and traceable to repository evidence.
- When an output is skipped, say why in the overview or in an assumptions file rather than fabricating content.

## Guardrails

- Base the summary on repository-local evidence, not generic expectations for the stack.
- Prefer a smaller set of correct artifacts over a complete-looking but speculative package.
- If an existing `docs/summary/` contains manual edits, replace it anyway unless the user explicitly requests a different policy.
- If generated artifacts depend on ambiguous architecture or domain behavior, stop and ask focused follow-up questions.
