# macOS Setup

Run from the dotfiles repository as the Mac user whose settings you want to change.

```bash
make macos
```

The full installer calls [`scripts/macos.sh`](../../../scripts/macos.sh) on macOS. You can also run `./scripts/macos.sh` directly. If the changes do not take effect immediately, log out and back in.

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
| Terminal Option as Meta key in Basic | Enabled |
| Trackpad tap to click | Enabled |
| Trackpad dragging style | Three-finger drag |
| Trackpad tap to drag | Disabled |
| Trackpad three-finger horizontal swipe | Disabled |
| Trackpad three-finger vertical swipe | Disabled |
| Mouse tracking speed | Fastest |
| Trackpad tracking speed | Fastest |
| Dock pinned apps | Finder → Safari → Terminal → ChatGPT (if installed in `/Applications`) → System Settings |

Dock setup replaces the current list of pinned apps each time it runs. Finder is displayed automatically by macOS.
