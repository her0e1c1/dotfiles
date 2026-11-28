# macOS Setup

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

## Server iCloud settings

Run `make macos-server` to apply the [home server settings](server.md). The command prints the iCloud steps and opens System Settings when run from a graphical login. If you ran it over SSH, use Screen Sharing to complete the steps on the server.

Use the macOS account whose sync settings you want to change. If System Settings shows **Sign In** at the top of the sidebar, iCloud sync is already off for that user. Otherwise, open **System Settings → Apple Account → iCloud**.

1. Open **Drive**, turn off **Sync this Mac**, and confirm it is off. This also stops syncing Desktop and Documents. If prompted, choose to keep a copy of files you still need on the server. [Apple’s guide to turning off iCloud Drive](https://support.apple.com/en-us/126628)
2. Under **iCloud → See All**, turn off and verify any other sync settings you want disabled, such as Photos, Passwords, or Notes. Save local copies of any iCloud data you need before turning off sync. [Apple’s iCloud settings guide](https://support.apple.com/guide/mac-help/mh36834/mac)

If the previously installed **macOS Server iCloud Restrictions** profile prevents you from changing the switches, remove that profile in **System Settings → General → Device Management** and check the switches again. [Apple’s profile removal guide](https://support.apple.com/guide/mac-help/mh35561/mac)
