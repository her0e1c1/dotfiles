# Neovim Cheatsheet

Use this when you know the edit flow but forget the command name or this config's custom keymaps.

## Frequently Used

| Command | Purpose |
| --- | --- |
| `:e` | Reload the current buffer |
| `<leader>/` | grep across the project |
| `<leader>uw` | toggle line wrap |
| `<leader>z` | zen mode |
| `:checkhealth` | Run health checks |

## Find Files

| `<leader><space>` | Find files from the project root |
| `<leader>fr` | Open recent files |
| `<leader>fd` | Open files in current directoires |
| `<leader>l` | Show `ls -1`-style entries for the current buffer's directory |
| `<leader>L` | Open Snacks buffer picker |

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
| `<leader>l` | Show `ls -1`-style directory entries; open files or enter directories with `netrw` |
| `<leader>L` | Open Snacks buffer picker |
| `?` | In `netrw`, open `which-key` for buffer-local keymaps |
| `g?` | In `netrw`, open the built-in keymap help |
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

| `<C-q>` | Enter Visual Block mode; useful on Windows where `<C-v>` is often paste |
| `:w` | Save the current buffer |
| `:q` | Quit the current window |
| `:qa` | Quit all windows |

## Dangerous / Destructive

None beyond normal editor commands. This page intentionally excludes broad Vim builtins and focuses on the shortcuts that differ in this config.
