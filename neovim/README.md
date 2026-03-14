# Neovim Cheatsheet

## Frequently used

| Command | Purpose |
| --- | --- |
| `:e {path}` | Open a file by path |
| `:w` | Save the current buffer |
| `:q` | Quit the current window |
| `:qa` | Quit all windows |

## Common Neovim commands

| Command | Purpose |
| --- | --- |
| `:Lazy` | Open the plugin manager UI |
| `:Mason` | Open the LSP/tool installer UI |
| `:LspInfo` | Inspect attached LSP clients |
| `:checkhealth` | Run Neovim health checks |
| `:Telescope keymaps` | Search available keymaps |
| `:Telescope commands` | Search available commands |
| `:Telescope help_tags` | Search help topics |

## Custom keymaps in this config

| Key | Purpose |
| --- | --- |
| `/` | Fuzzy-search lines in the current buffer via `fzf-lua` |
| `g/` | Use the original Vim `/` search |
| `<leader>fc` | Open Telescope command search |
| `<leader>fk` | Open Telescope keymap search |
| `<leader>fh` | Open Telescope help search |
