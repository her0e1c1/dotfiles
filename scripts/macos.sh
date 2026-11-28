#!/bin/bash

set -euo pipefail

if [[ "$OSTYPE" != darwin* ]]; then
  echo "Skipping macOS settings on $OSTYPE"
  exit 0
fi

echo "Configuring macOS settings..."

defaults write -g AppleInterfaceStyle -string Dark
defaults write -g AppleInterfaceStyleSwitchesAutomatically -bool false
defaults write com.apple.dock autohide -bool true
defaults write com.apple.dock tilesize -float 128
defaults write com.apple.WindowManager GloballyEnabled -bool true
# Finder uses Nlsv as the identifier for List view.
defaults write com.apple.finder FXPreferredViewStyle -string Nlsv
defaults write -g ApplePressAndHoldEnabled -bool false

# macOS stores gestures separately for built-in and Bluetooth trackpads.
for domain in com.apple.AppleMultitouchTrackpad com.apple.driver.AppleBluetoothMultitouch.trackpad; do
  defaults write "$domain" Clicking -bool true
  defaults write "$domain" TrackpadThreeFingerDrag -bool true
  # Clear tap-to-drag and swipes that compete with three-finger dragging.
  defaults write "$domain" Dragging -bool false
  defaults write "$domain" TrackpadThreeFingerHorizSwipeGesture -int 0
  defaults write "$domain" TrackpadThreeFingerVertSwipeGesture -int 0
  defaults write "$domain" TrackpadFourFingerVertSwipeGesture -int 2
done

# Keep the per-Mac gesture and tap preferences in sync with the trackpad domains.
defaults -currentHost write -g com.apple.mouse.tapBehavior -int 1
defaults -currentHost write -g com.apple.trackpad.threeFingerDragGesture -bool true
defaults -currentHost write -g com.apple.trackpad.threeFingerHorizSwipeGesture -int 0
defaults -currentHost write -g com.apple.trackpad.threeFingerVertSwipeGesture -int 0
# Use four fingers for App Exposé so three fingers remain available for dragging.
defaults -currentHost write -g com.apple.trackpad.fourFingerVertSwipeGesture -int 2
defaults write com.apple.dock showAppExposeGestureEnabled -bool true

# 3 is the maximum tracking speed in System Settings.
defaults write -g com.apple.mouse.scaling -float 3
defaults write -g com.apple.trackpad.scaling -float 3

# Update the active mouse speed as well. HID uses 16.16 fixed-point values.
if command -v hidutil >/dev/null 2>&1; then
  hidutil property --set '{"HIDMouseAcceleration":196608}'
fi

# Use Foundation to update the nested Terminal profile while preserving its other settings.
osascript -l JavaScript <<'JXA'
ObjC.import("Foundation");

const prefs = $.NSUserDefaults.alloc.initWithSuiteName("com.apple.Terminal");
const terminal = $.NSBundle.bundleWithPath("/System/Applications/Utilities/Terminal.app");
const profilesKey = "Window Settings";
const builtInDirectory = "Initial Settings";
let profiles = prefs.dictionaryForKey(profilesKey).mutableCopy;

// Keep the full set of bundled profiles when Terminal has no saved profiles yet.
if (!profiles) {
  profiles = $.NSMutableDictionary.alloc.init;
  const paths = terminal.pathsForResourcesOfTypeInDirectory("terminal", builtInDirectory);
  for (const path of ObjC.deepUnwrap(paths)) {
    const name = ObjC.unwrap($(path).lastPathComponent.stringByDeletingPathExtension);
    profiles.setObjectForKey($.NSDictionary.dictionaryWithContentsOfFile(path), name);
  }
}

let basic = profiles.objectForKey("Basic").mutableCopy;
if (!basic) {
  const path = terminal.pathForResourceOfTypeInDirectory("Basic", "terminal", builtInDirectory);
  basic = $.NSDictionary.dictionaryWithContentsOfFile(path).mutableCopy;
}
basic.setObjectForKey($.NSNumber.numberWithBool(true), "useOptionAsMetaKey");
profiles.setObjectForKey(basic, "Basic");
prefs.setObjectForKey(profiles, profilesKey);
if (!prefs.synchronize) {
  throw new Error("Could not save Terminal preferences");
}
JXA

defaults write com.apple.Terminal 'Default Window Settings' -string Basic
defaults write com.apple.Terminal 'Startup Window Settings' -string Basic

echo "Configuring macOS Dock..."

dock_tile() {
  # Dock uses URL type 15 for file URLs to application bundles.
  printf '<dict>
    <key>tile-type</key><string>file-tile</string>
    <key>tile-data</key><dict>
      <key>file-data</key><dict>
        <key>_CFURLString</key><string>file://%s/</string>
        <key>_CFURLStringType</key><integer>15</integer>
      </dict>
    </dict>
  </dict>' "${1// /%20}"
}

dock_apps=(
  /Applications/Safari.app
  /System/Applications/Utilities/Terminal.app
)

if [[ -d /Applications/ChatGPT.app ]]; then
  dock_apps+=(/Applications/ChatGPT.app)
fi

dock_apps+=("/System/Applications/System Settings.app")

dock_tiles=()
for app in "${dock_apps[@]}"; do
  if [[ ! -d "$app" ]]; then
    printf 'Cannot configure Dock: application not found at %s\n' "$app" >&2
    exit 1
  fi
  dock_tiles+=("$(dock_tile "$app")")
done

# Finder is supplied by macOS and stays ahead of the pinned apps.
defaults write com.apple.dock persistent-apps -array "${dock_tiles[@]}"

# Without a running Dock, the saved preferences apply at the next login.
if pgrep -u "$UID" -x Dock >/dev/null; then
  killall Dock
fi

echo "macOS system, input, Dock, and Terminal settings saved. If needed, log out and back in to apply them."
