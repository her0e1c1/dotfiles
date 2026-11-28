#!/bin/bash

set -euo pipefail

if [[ "$OSTYPE" != darwin* ]]; then
  echo "Skipping macOS input settings on $OSTYPE"
  exit 0
fi

echo "Configuring macOS trackpad and pointer settings..."

# macOS stores gestures separately for built-in and Bluetooth trackpads.
for domain in com.apple.AppleMultitouchTrackpad com.apple.driver.AppleBluetoothMultitouch.trackpad; do
  defaults write "$domain" Clicking -bool true
  defaults write "$domain" TrackpadThreeFingerDrag -bool true
  # Clear tap-to-drag and swipes that compete with three-finger dragging.
  defaults write "$domain" Dragging -bool false
  defaults write "$domain" TrackpadThreeFingerHorizSwipeGesture -int 0
  defaults write "$domain" TrackpadThreeFingerVertSwipeGesture -int 0
done

# Keep the per-Mac gesture and tap preferences in sync with the trackpad domains.
defaults -currentHost write -g com.apple.mouse.tapBehavior -int 1
defaults -currentHost write -g com.apple.trackpad.threeFingerDragGesture -bool true
defaults -currentHost write -g com.apple.trackpad.threeFingerHorizSwipeGesture -int 0
defaults -currentHost write -g com.apple.trackpad.threeFingerVertSwipeGesture -int 0

# 3 is the maximum tracking speed in System Settings.
defaults write -g com.apple.mouse.scaling -float 3
defaults write -g com.apple.trackpad.scaling -float 3

# Update the active mouse speed as well. HID uses 16.16 fixed-point values.
if command -v hidutil >/dev/null 2>&1; then
  hidutil property --set '{"HIDMouseAcceleration":196608}'
fi

echo "macOS input settings saved. If needed, log out and back in to apply them."
