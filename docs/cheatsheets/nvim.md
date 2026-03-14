# Neovim Cheatsheet

Use this when you know the edit flow but forget the command name or this config's custom keymaps.

## Frequently Used

| Command | Purpose |
| --- | --- |
| `:e {path}` | Open a file by path |
| `:w` | Save the current buffer |
| `:q` | Quit the current window |
| `:qa` | Quit all windows |
| `:checkhealth` | Run health checks |

## Config-Specific

| Key | Purpose |
| --- | --- |
| `/` | Fuzzy-search lines in the current buffer via `fzf-lua` |
| `g/` | Use the original Vim `/` search |
| `<leader>d` | Pick files and directories in the current buffer's directory only |
| `<leader>D` | Pick any subdirectory under the current buffer's directory |
| `<leader>fc` | Open Telescope command search |
| `<leader>fk` | Open Telescope keymap search |
| `<leader>fh` | Open Telescope help search |

## Discovery

| Command | Purpose |
| --- | --- |
| `:Telescope keymaps` | Search available keymaps |
| `:Telescope commands` | Search available commands |
| `:Telescope help_tags` | Search help topics |
| `:Lazy` | Inspect installed plugins |
| `:Mason` | Inspect installed LSP/tools |
| `:LspInfo` | Inspect attached LSP clients |

## Dangerous / Destructive

None beyond normal editor commands. This page intentionally excludes broad Vim builtins and focuses on the shortcuts that differ in this config.
