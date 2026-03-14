# Shell / CLI Cheatsheet

Use this when you forget the aliases, fzf helpers, or readline bindings defined in `.profile`.

## Frequently Used

| Alias / Key | Purpose |
| --- | --- |
| `n` | Start containerized Neovim via `nvim_start` |
| `g` | `git` |
| `d` | `docker` |
| `dc` | `docker compose` |
| `k` | `kubectl` |
| `vs` | Open VS Code rooted at the current working directory |
| `f` | Search for a word under the current directory and open the match |
| `cd` | Fuzzy-select a recent directory via `fzf_select_dir` |
| `vim` | Open the recent-files picker instead of launching Vim directly |

## fzf Helpers

| Command / Key | Purpose |
| --- | --- |
| `Ctrl-r` | Fuzzy-search shell history and execute the selected command |
| `Alt-o` | Fuzzy-pick a recent file and open it in `$EDITOR` |
| `Alt-d` | Fuzzy-pick a recent directory and `cd` into it |
| `Alt-w` | Fuzzy-pick a running Docker container and open a shell inside it |
| `Alt-g` | Jump to the parent directory with `cdls ..` |
| `f <word>` | Grep under the current directory, then open the selected result |
| `cd <path>` | Still works, but routes through `cdls` and prints directory info |

## Config-Specific

| Alias / Function | Purpose |
| --- | --- |
| `dev` | Run `devcontainer --workspace-folder . ...` via `devcontainer_wrapper` |
| `dcr` | `docker compose run --remove-orphans --rm` |
| `dcd` | `docker compose down --remove-orphans --volumes` |
| `dcu` | `docker compose up` |
| `dcw` | `docker compose up --remove-orphans --force-recreate --watch watch` |
| `ka` | `kubectl apply -f` |
| `kk` | `kubectl kustomize --enable-helm ... | kubectl apply -f -` |
| `me` | `docker compose -f docker-compose.me.yml` |
| `mesh` | Run a shell in `docker-compose.me.yml` |
| `ti` | `tmuxinator` |
| `tmux_new [name]` | Start a new tmux session, optionally with a session name |
| `Alt-i` | Edit and execute the current shell command |
| `Ctrl-x r` | Use Bash reverse history search |

## Neovim Container Flow

| Command | Purpose |
| --- | --- |
| `n [args...]` | Start `nvim:stable` in Docker, mounting the current working directory |
| `nvim_end` | Optionally remove `nvim-cache` and `nvim-local` volumes |

## Discovery

| Command | Purpose |
| --- | --- |
| `rg '^(alias |bind )' .profile` | Inspect aliases and readline bindings |
| `rg '^.*\\(\\) \\{' .profile` | Inspect custom shell functions |
| `bind -P | less` | Inspect the active readline key bindings |
| `type <alias-or-function>` | Confirm what a shortcut resolves to |

## Dangerous / Destructive

| Command | Purpose |
| --- | --- |
| `dcd` | Drops compose services and removes volumes |
| `nvim_end` | Removes Neovim Docker volumes if confirmed |
