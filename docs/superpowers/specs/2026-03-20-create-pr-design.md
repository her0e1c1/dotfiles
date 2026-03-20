# Create PR Design

**Goal:** Define a reusable Codex skill named `create-pr` that determines the correct base branch, incorporates relevant uncommitted work when appropriate, summarizes the branch diff, generates a PR title and body, and creates the GitHub pull request end to end.

## Problem

Creating a pull request often requires several connected decisions that generic Git or GitHub commands do not solve well on their own. Codex needs to determine which branch the work should target, decide whether local staged or unstaged changes belong in the PR, turn the resulting branch delta into coherent commits, and then generate a usable PR title and body before calling `gh pr create`.

The failure mode is not just inconvenience. Choosing the wrong base branch, sweeping unrelated local changes into the PR, or generating a title and body from the wrong diff produces misleading history and noisy review requests. The useful middle ground is a single execution skill that automates the full PR-creation path, while stopping only when the branch state is genuinely ambiguous or unsafe to continue.

## Scope

The skill named `create-pr` should:

- Trigger when the user wants Codex to create a pull request for the current branch.
- Inspect the current Git state before taking action.
- Infer the likely base branch automatically when one candidate is clearly favored.
- Ask the user only when the base branch remains genuinely ambiguous.
- Review staged, unstaged, and untracked changes and decide which uncommitted work naturally belongs in the PR.
- Create one or more commits for relevant uncommitted work when needed.
- Leave unrelated local changes untouched in the working tree.
- Summarize the `base...HEAD` diff and derive a PR title and body from that summary.
- Push the current branch and create the PR with `gh pr create`.
- Return a concise execution summary that includes the chosen base branch, any commits added, and the created PR URL.

The skill should not:

- Blindly commit every local change just because the working tree is dirty.
- Discard, stash, or rewrite unrelated local work without an explicit user request.
- Guess a base branch when multiple candidates remain similarly plausible.
- Claim success if push or PR creation fails.
- Require a confirmation step before push or PR creation in normal high-confidence cases.

## Triggering Conditions

The description should target situations such as:

- "create pr"
- "push this branch and open a PR"
- "make a pull request for the current work"
- "open a GitHub PR and fill in the title/body"

The description should emphasize that this skill performs the PR-creation workflow, not just title drafting.

## Core Workflow

The skill should instruct Codex to follow this order:

1. Read the current Git context:
   - current branch name
   - tracking branch, if present
   - working tree state across staged, unstaged, and untracked files
   - remote availability
   - `gh` availability
2. Determine the base branch:
   - check `git config branch.<current-branch>.base` first and prefer it when set
   - compare likely candidates such as `main`, `master`, and `develop`
   - use merge-base and fork-point style evidence first
   - use upstream and recent branch context as secondary signals
   - ask the user only if the candidate set does not collapse to one strong option
3. Review uncommitted changes:
   - identify which changes clearly belong in the PR
   - keep unrelated changes out of the PR and preserve them locally
   - if some changes are plausibly related but still ambiguous, stop and ask the user
4. Create commits for relevant uncommitted work:
   - split into multiple commits when the work naturally separates into distinct changes
   - otherwise group related work into one commit
   - preserve existing commits as they are and append only the new commits needed
5. Summarize the branch delta using the chosen base branch:
   - derive a concise summary from `base...HEAD`
   - identify the branch's main user-visible or maintenance-visible purpose
6. Generate PR metadata:
   - title should capture the branch's main purpose
   - body should summarize the meaningful changes
   - if a repository PR template exists, adapt to it
   - otherwise use a compact default structure
7. Push the current branch:
   - set upstream if needed
8. Create the PR with `gh pr create`
9. Return a concise result summary

## Base Branch Inference Rules

The skill should prefer evidence in this order:

1. `git config branch.<current-branch>.base` when it is set
2. merge-base and fork-point relationships with common integration branches
3. the branch's configured upstream or remote default branch when that evidence aligns with the commit graph
4. recent branch history or naming context as a tie-breaker only

The skill should treat base-branch inference as successful only when one candidate is materially stronger than the others. If `main` and `master`, or `main` and `develop`, remain similarly plausible after graph inspection, the skill should ask the user directly instead of guessing.

The user requirement is: infer the base branch when possible, but ask if it is not knowable with sufficient confidence.

## Uncommitted Change Selection Rules

The skill should inspect all local changes before committing anything:

- staged changes
- unstaged tracked changes
- untracked files

Selection should be behavior-based, not file-state-based. The key question is whether a change naturally belongs in the PR branch's purpose.

The skill should:

- include changes that are clearly part of the same work
- split changes into multiple commits when they represent different logical units
- keep unrelated changes uncommitted and unpushed
- never destroy unrelated changes

The skill should stop and ask the user when:

- a change could reasonably belong either in this PR or in later work
- the working tree mixes two plausible but distinct tasks and the boundary is not inferable
- partial staging or commit splitting would require guessing at intent beyond what the diff supports

## Commit Creation Rules

When relevant uncommitted changes exist, the skill should:

- preserve the current commit history
- add only the new commits needed for PR-ready history
- generate commit messages from the actual grouped changes
- create multiple commits when the change groups are distinct enough to justify it

The skill should not flatten existing history or rewrite prior commits unless the user separately asks for history cleanup.

## PR Metadata Rules

If the repository does not provide a PR template, the default body should remain compact and predictable. A suitable default structure is:

- `## Summary`
- `## Testing`

`Summary` should usually contain 2-4 bullets. `Testing` should include only checks actually performed or a clear note when no verification was run as part of the PR creation flow.

The PR title should describe the branch's main change at the branch level, not mirror the most recent commit title unless that commit already captures the whole branch.

## Failure and Stop Conditions

The skill should stop and explain the blocker when any of the following hold:

- detached `HEAD`
- no suitable remote for pushing
- `gh` is unavailable
- push fails due to permissions, auth, or remote state
- base branch inference remains ambiguous
- uncommitted changes contain ambiguous scope that cannot be safely inferred

If PR creation fails after the title and body have already been generated, the skill should return:

- the chosen base branch
- the generated title
- the generated body
- the next command the user could run manually

This preserves useful output even when automation fails late.

## Output Contract

On success, the skill should return a concise execution summary including:

- chosen base branch
- any newly created commits
- final PR title
- created PR URL

On blocked or failed execution, the skill should report the exact blocker and avoid claiming that the PR exists.

## Skill Packaging

The first version should use a minimal structure:

- `.codex/skills/create-pr/SKILL.md`
- `.codex/skills/create-pr/agents/openai.yaml`

No helper scripts are required initially because the behavior is primarily workflow- and judgment-driven rather than template-heavy.

## SKILL.md Content Direction

`SKILL.md` should stay concise and include:

- frontmatter with `name` and `description` only
- a short overview of the end-to-end PR creation behavior
- the main workflow from Git inspection through `gh pr create`
- the base-branch inference rules
- the uncommitted-change selection and commit-splitting rules
- the stop conditions and late-failure behavior

## Agent Metadata

`agents/openai.yaml` should include:

- a user-facing display name
- a short UI description
- a default prompt that explicitly mentions `$create-pr`

Icons and brand colors are unnecessary unless provided separately.

## Validation Strategy

Implementation should validate the resulting skill directory structure and frontmatter if a local validation path exists. If no validation script is available, perform a manual structural review and record that limitation.

Because this skill interacts with Git and GitHub state, behavioral checks should cover at least these scenarios:

- clean branch with existing commits only
- branch with clearly related uncommitted changes
- branch with mixed related and unrelated uncommitted changes
- ambiguous base branch requiring a user question
- late `gh pr create` failure after metadata generation
