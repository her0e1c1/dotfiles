# tmux Cheatsheet

Use this when you forget the custom prefix or the window/pane bindings from this repo's `.tmux.conf`.

## Frequently Used

| Key | Purpose |
| --- | --- |
| `M-u` | tmux prefix in this config |
| `M-u M-r` | Reload `~/.tmux.conf` |
| `M-u h` | Split pane horizontally |
| `M-u v` | Split pane vertically |
| `M-.` | Select next pane |
| `M-,` | Select previous pane |
| `M-j` | Toggle zoom for the current pane |
| `M-L` | Jump to the previous window |

## Config-Specific

| Key | Purpose |
| --- | --- |
| `M-u u` | Send the prefix key to the application |
| `M-u r` | Refresh the current client |
| `M-u Up/Down/Left/Right` | Resize the current pane by 5 cells |
| `M-u M-w` | Choose a window |
| `M-u C-w` | Rename the current window |
| `M-u M-t` | Open the tree chooser |
| `M-u M-s` | Choose a session |
| `M-u C-s` | Rename the current session |
| `M-s` | Open `tmux_new` in a popup for fuzzy session switching or creating `main` |
| `M-Down` | Switch to the previous session |
| `M-Up` | Switch to the next session |
| `M-u M-p` | Choose a paste buffer |
| `M-p` | Paste the latest buffer |
| `M-k` | Enter copy-mode |
| `M-d` | Detach the client |

## Function Keys

| Key | Purpose |
| --- | --- |
| `F1` | New window in the current pane path |
| `F2` | Split vertically in the current pane path |
| `F3` | Split horizontally in the current pane path |
| `F4` | Switch to the previous client |
| `F5` | Rotate panes in the current window |
| `F6` | Cycle to the next layout |
| `F9` | Kill the current pane |
| `F10` | Kill the current window |
| `F12` | Kill the current session |

## Discovery

| Command | Purpose |
| --- | --- |
| `tmux list-keys | less` | Inspect all active key bindings |
| `tmux show -g | less` | Inspect global options |
| `tmux source-file ~/.tmux.conf` | Reload config manually if needed |

## Dangerous / Destructive

| Key | Purpose |
| --- | --- |
| `F9` | Kill the current pane |
| `F10` | Kill the current window |
| `F12` | Kill the current session |
| `M-u M-K` | Kill the current session |
