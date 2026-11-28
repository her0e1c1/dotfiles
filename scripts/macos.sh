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

function configureTerminal(prefs, factoryProfilesPath) {
  const profilesKey = "Window Settings";
  const storedProfiles = prefs.objectForKey(profilesKey);
  const profiles = storedProfiles.isNil()
    ? $.NSMutableDictionary.alloc.init
    : storedProfiles.mutableCopy;

  // On a fresh setup, retain all of Terminal's built-in profiles.
  if (storedProfiles.isNil()) {
    const files = $.NSFileManager.defaultManager.contentsOfDirectoryAtPathError(factoryProfilesPath, null);
    for (const file of ObjC.deepUnwrap(files)) {
      if (file.endsWith(".terminal")) {
        const profile = $.NSDictionary.dictionaryWithContentsOfFile(factoryProfilesPath + "/" + file);
        profiles.setObjectForKey(profile, file.slice(0, -".terminal".length));
      }
    }
  }

  const storedBasic = profiles.objectForKey("Basic");
  const basic = storedBasic.isNil()
    ? $.NSDictionary.dictionaryWithContentsOfFile(factoryProfilesPath + "/Basic.terminal").mutableCopy
    : storedBasic.mutableCopy;
  basic.setObjectForKey($.NSNumber.numberWithBool(true), "useOptionAsMetaKey");
  profiles.setObjectForKey(basic, "Basic");

  prefs.setObjectForKey(profiles, profilesKey);
  prefs.setObjectForKey("Basic", "Default Window Settings");
  prefs.setObjectForKey("Basic", "Startup Window Settings");

  if (!prefs.synchronize) {
    throw new Error("Could not save Terminal preferences");
  }
}

const factoryProfilesPath = "/System/Applications/Utilities/Terminal.app/Contents/Resources/Initial Settings";
configureTerminal($.NSUserDefaults.alloc.initWithSuiteName("com.apple.Terminal"), factoryProfilesPath);
JXA

echo "macOS settings saved. If needed, log out and back in to apply them."
