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

From a graphical session on the server Mac, run the following command to apply the [home server settings](server.md) and open the configuration profile.

```bash
make macos-server
```

In **System Settings → General → Device Management**, select **macOS Server iCloud Restrictions** under **Downloaded** and click **Install**. Use a screen sharing session if the server has no display. [Apple’s profile installation guide](https://support.apple.com/guide/mac-help/mh35561/mac)

The profile disables iCloud Drive (including Desktop and Documents), Photos, Keychain, Notes, Contacts, Calendar, Safari bookmark sync, and iCloud Mail for all users on the Mac. If the Mac already syncs files or photos, [copy anything you need locally](https://support.apple.com/en-us/108306) before installing the profile.

You can run `make macos-server` again to open an updated profile for installation. To remove the restrictions, remove the profile in **Device Management**.
