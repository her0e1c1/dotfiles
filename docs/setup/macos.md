# macOS Setup

Use this checklist after initializing a Mac.

## Setup Mac

### Change PS1 `\H`

Set the local host name used by shell prompts that include `\H`.

```bash
sudo scutil --set LocalHostName $NAME
```

### Enable Continuous Key Repeat

Disable press-and-hold accent selection so holding a key repeats the same character.

```bash
defaults write -g ApplePressAndHoldEnabled -bool false
```

### Trackpad and Pointer

Run from the dotfiles repository as the Mac user whose settings you want to change.

```bash
make macos
```

You can also run `./scripts/macos.sh` directly. The full installer calls the same script on macOS. If the changes do not take effect immediately, log out and back in.

| Setting | Value |
| --- | --- |
| Trackpad tap to click | Enabled |
| Trackpad dragging style | Three-finger drag |
| Mouse and trackpad tracking speed | Fastest |

For two-finger secondary click, open `System Settings` → `Trackpad` → `Point & Click` and set `Secondary click` to `Click or Tap with Two Fingers`.

### Terminal

Open `Terminal` -> `Settings` -> `Profiles` -> `Keyboard`.

| Setting | Value |
| --- | --- |
| Use Option as Meta key | Enabled |

### Apple Watch Unlock

Open `System Settings` -> `Touch ID & Password`.

| Setting | Value |
| --- | --- |
| Use Apple Watch to unlock your applications and your Mac | Enabled |
