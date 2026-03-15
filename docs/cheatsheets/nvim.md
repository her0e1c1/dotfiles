# Neovim Cheatsheet

Use this when you know the edit flow but forget the command name or this config's custom keymaps.

## Frequently Used

| Command | Purpose |
| --- | --- |
| `:e` | Reload the current buffer |
| `<leader>e` | toggle explorer |
| `<leader>uw` | toggle line wrap |
| `<leader>z` | zen mode |
| `:checkhealth` | Run health checks |

## Find Files

| `<leader><space>` | Find files from the project root |
| `<leader>fr` | Open recent files |
| `<leader>fd` | Open files in current directoires |

## Search

| `<leader>:` | Command History |
| `<leader>fc` | Lua Config Files |
| `<leader>:fb` | Buffer Search |

## Buffer

| `H` | go to prev buffer |
| `L` | go to next buffer |
| `<leader>bd` | delete buffer |

## Config-Specific

| Key | Purpose |
| --- | --- |
| `/` | Search lines in the current buffer via `Snacks` |
| `g/` | Use the original Vim `/` search |
| `<leader>d` | Pick files and directories in the current buffer's directory only |
| `<leader>D` | Pick any subdirectory under the current buffer's directory |
| `<leader>e` | Open Snacks explorer in the current buffer's directory |
| `<leader>f?` | Open Snacks keymap search |
| `<leader>fc` | Open Telescope command search |
| `<leader>fh` | Open Telescope help search |
| `<leader>z` | Toggle Snacks zen mode |

## Discovery

| Command | Purpose |
| --- | --- |
| `<leader>f?` | Open Snacks keymap search |
| `<leader>n` | Show notification history |
| `:lua Snacks.picker.keymaps()` | Search available keymaps |
| `:Telescope commands` | Search available commands |
| `:Telescope help_tags` | Search help topics |
| `:Lazy` | Inspect installed plugins |
| `:Mason` | Inspect installed LSP/tools |
| `:LspInfo` | Inspect attached LSP clients |

## Vim Binds

| `:w` | Save the current buffer |
| `:q` | Quit the current window |
| `:qa` | Quit all windows |

## Dangerous / Destructive

None beyond normal editor commands. This page intentionally excludes broad Vim builtins and focuses on the shortcuts that differ in this config.
