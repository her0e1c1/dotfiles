# tig Cheatsheet

Use this when you remember the workflow but not the custom `tig` bindings in this repo's `.tigrc`.

## Frequently Used

| Key | Context | Purpose |
| --- | --- | --- |
| `c` | `refs` | Checkout the selected branch |
| `c` | `main` | Checkout the selected commit |
| `b` | `main` / `refs` | Create and checkout a branch from the selected commit |
| `C` | `generic`, `status`, `main`, `refs` | Run `git commit -v` |
| `p` | `refs` | Push the selected branch with upstream |
| `r` | `refs` | Pull with `--ff-only --rebase` |
| `F` | `refs` | Fetch and prune the selected remote |
| `H` | `generic` | Open `tig` history for the selected file |
| `W` | `generic` | Toggle whitespace ignoring in diffs (`ignore-space`) |

## Config-Specific

| Key | Context | Purpose |
| --- | --- | --- |
| `<Esc>P` | `generic` | Push current HEAD with upstream |
| `<Esc>P` | `refs` | Force-push selected branch with lease |
| `M` | `main` / `refs` | Merge the selected commit |
| `;mm` | `refs` | Merge the selected commit |
| `;mo` | `refs` | Merge with `-X ours` |
| `;mt` | `refs` | Merge with `-X theirs` |
| `I` | `main` | Start autosquash interactive rebase from the selected commit |
| `I` | `refs` | Confirm and rebase onto the selected branch |
| `O` | `main` | Rebase current HEAD onto a prompted base |
| `K` | `main` | Cherry-pick the selected commit |
| `R` | `main` | Revert the selected commit without committing |
| `R` | `refs` | Rebase onto the selected branch |
| `;ma` / `;mc` | `generic` | Abort or continue merge |
| `;ra` / `;rc` / `;rs` | `generic` | Abort, continue, or skip rebase |

## gh / Diff Helpers

| Key | Context | Purpose |
| --- | --- | --- |
| `;gg` | `generic` | Open GitHub in the browser |
| `;gc` | `generic` | Create a PR with `gh pr create --fill --web --assignee @me` |
| `;gv` | `generic` | View the current PR in the browser |
| `;gl` | `generic` | List PRs |
| `v` | `blame`, `main`, `tree` | Browse the selected file or commit on GitHub |
| `;gb` | multiple | Browse the selected file, commit, or branch on GitHub |
| `;dd` | `generic` | Diff `commit..HEAD` in tig |
| `;dr` | `generic` | Diff `HEAD..commit` in tig |
| `;d3` | `generic` | Diff `commit...HEAD` in tig |
| `;df` | `generic` | Diff a prompted hash against the selected file |

## Discovery

| Command | Purpose |
| --- | --- |
| `rg '^bind' .tigrc` | Inspect custom bindings in this repo |
| `tig` then move across views | Confirm which view a binding applies to |
| `less .tigrc` | Inspect view-specific commands and shell integrations |

## Dangerous / Destructive

| Key | Context | Purpose |
| --- | --- | --- |
| `dd` | `refs` | Delete the selected local branch |
| `dr` | `refs` | Delete the selected remote branch |
| `dw` | `refs` | Remove the selected worktree and delete its branch |
| `dt` | `refs` | Delete the selected tag |
| `dd` | `stash` | Drop the selected stash |
| `dc` | `status` | Run `git clean -d -f` |
| `R` | `status` | Run `git reset --hard HEAD` |
| `S` | `main` | Soft reset to the selected commit |
| `R` / `H` / `S` | `reflog` | Reset, hard reset, or soft reset to the selected commit |
