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

|j`<leader><space>` | Find files from the current working directory |
| `<leader>d` | Pick subdirectories (max depth 1) under the current directory |
| `<leader>D` | Pick recursive subdirectories under the project root |
| `<leader>l` | Show `ls -1`-style entries for the current buffer's directory |
| `<leader>L` | Open Snacks buffer picker |
| `<leader>fF` | Open files from the project root (`<leader>ff` remap) |
| `<leader>fr` | Open recent files |
| `<leader>fc` | Lua Config Files |

## Netrw

Open netrw with `o` from the dashboard or `:e .` from anywhere.

| Command | Purpose |
| --- | --- |
| `<Enter>` | Open file or enter directory |
| `-` | Go to parent directory |
| `%` | Create a new file |
| `d` | Create a new directory |
| `R` | Rename file or directory |
| `D` | Delete file or directory |
| `a` | Create a new file (`%` remap) |
| `b` | Open buffer picker (`<leader>fb` remap) |
| `f` | Open project root files (`<leader>fF` remap) |
| `r` | Open recent files (`<leader>fr` remap) |
| `o` | Open netrw directory history (`<leader>O` remap) |
| `l` | Show directory entries picker |
| `?` | Show local netrw keymaps |
| `g?` | Open `:help netrw-browse-maps` |

## Search

| `<leader>:` | Command History |

## Buffer

| `<leader>fb` | Buffer Search |
| `<leader>L` | Buffer Search (`<leader>fb` remap) |
| `H` | go to prev buffer |
| `L` | go to next buffer |
| `<leader>bd` | delete buffer |

## Config-Specific

| Key | Purpose |
| --- | --- |
| `<leader>f?` | Open Snacks keymap search |
| `<leader>fc` | Open Telescope command search |
| `<leader>fh` | Open Telescope help search |

## Discovery

| Command | Purpose |
| --- | --- |
| `<leader>f?` | Open Snacks keymap search |
| `<leader>n` | Show notification history |
| `:lua Snacks.picker.keymaps()` | Search available keymaps |
| `:Lazy` | Inspect installed plugins |
| `:Mason` | Inspect installed LSP/tools |
| `:LspInfo` | Inspect attached LSP clients |

## Vim Binds

| `<C-q>` | Enter Visual Block mode; useful on Windows where `<C-v>` is often paste |
| `:w` | Save the current buffer |
| `:q` | Quit the current window |
| `:qa` | Quit all windows |

## Lines Binds

| `<C-w>` | Clear line |
| `<C-q>` | Selection mode |

## Ohters

```lua
-- run code
:lua print(vim.fn.getcwd())
-- run global function
:lua print(vim.inspect(list_netrw_directory_history()))
```
