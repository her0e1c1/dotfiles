# macOS Setup

If you see `No developer tools were found` when running the `curl` command from the [installation instructions](../../../README.md#run-install-script), follow the on-screen prompts in the dialog that opens to install the Command Line Tools. Once installation finishes, run the `curl` command again.

Run from the dotfiles repository as the Mac user whose settings you want to change.

```bash
make macos
```

The full installer calls [`scripts/macos.sh`](../../../scripts/macos.sh) on macOS. You can also run `./scripts/macos.sh` directly. If the changes do not take effect immediately, log out and back in.

If Terminal was open during setup, quit it with Command-Q and reopen it to load the updated profiles. If Terminal restores a previous session, open a new window to check Option as Meta.

| Setting | Value |
| --- | --- |
| Appearance | Dark |
| Dock auto-hide | Enabled |
| Dock icon size | 128 |
| Stage Manager | Enabled |
| Finder view style | List |
| App Exposé trackpad gesture | Swipe down with four fingers |
| Press-and-hold accent menu | Disabled |
| Terminal profile for new windows | Basic |
| Terminal startup profile | Basic |
| Terminal Option as Meta key | Both Option keys in all profiles present during setup |
| Trackpad tap to click | Enabled |
| Trackpad dragging style | Three-finger drag |
| Trackpad tap to drag | Disabled |
| Trackpad three-finger horizontal swipe | Disabled |
| Trackpad three-finger vertical swipe | Disabled |
| Mouse tracking speed | 2.5 |
| Trackpad tracking speed | 2.5 |
| Dock pinned apps | Finder → Safari → Terminal → ChatGPT (if installed in `/Applications`) → System Settings |

Dock setup replaces the current list of pinned apps each time it runs. Finder is displayed automatically by macOS.
