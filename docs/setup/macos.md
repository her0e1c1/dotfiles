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

### Trackpad

Open `System Settings` -> `Trackpad`.

| Setting | Value |
| --- | --- |
| Click | Enable one-finger click |
| Secondary click | Click or tap with two fingers |
| Tracking speed | Set the cursor speed to a comfortable level |

### Three-Finger Drag

Open `System Settings` -> `Accessibility` -> `Pointer Control` -> `Trackpad Options`.

| Setting | Value |
| --- | --- |
| Use trackpad for dragging | Enabled |
| Dragging style | Three-Finger Drag |

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
