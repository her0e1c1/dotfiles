#!/bin/bash

set -euo pipefail

if [[ "$OSTYPE" != darwin* ]]; then
  echo "Skipping macOS server settings on $OSTYPE"
  exit 0
fi

if (( EUID == 0 )); then
  echo "Run make macos-server as the Mac user whose screen should lock." >&2
  exit 1
fi

server_user="$(id -un)"

sudo -v

# sysadminctl updates the current user's lock setting and prompts for their password if needed.
lock_status="$(LC_ALL=C sysadminctl -screenLock status 2>&1)"
if [[ "$lock_status" != *"screenLock delay is immediate"* ]]; then
  sysadminctl -screenLock immediate -password -
  lock_status="$(LC_ALL=C sysadminctl -screenLock status 2>&1)"
  if [[ "$lock_status" != *"screenLock delay is immediate"* ]]; then
    printf 'Could not configure immediate screen lock.\n%s\n' "$lock_status" >&2
    exit 1
  fi
fi

# Keep the system awake while letting the display turn off and trigger the lock.
sudo pmset -a sleep 0 displaysleep 10

# Restore access after an outage on Macs that support restarting when power returns.
power_capabilities="$(pmset -g cap)"
if awk '$1 == "autorestart" { found = 1 } END { exit !found }' <<< "$power_capabilities"; then
  sudo pmset -c autorestart 1
fi

enable_remote_access() {
  local group="$1" service="system/$2" plist="/System/Library/LaunchDaemons/$3"

  # The local service access group restricts logins before the listener starts.
  if ! sudo dseditgroup -o read -n /Local/Default "$group" >/dev/null 2>&1; then
    sudo dseditgroup -o create -q -n /Local/Default "$group"
  fi
  sudo dseditgroup -o edit -n /Local/Default -a "$server_user" -t user "$group"

  # Both services are socket-activated. Avoid reloading active remote sessions.
  sudo launchctl enable "$service"
  if ! sudo launchctl print "$service" >/dev/null 2>&1; then
    sudo launchctl bootstrap system "$plist"
  fi
}

enable_remote_access com.apple.access_screensharing com.apple.screensharing com.apple.screensharing.plist
# SSH supports remote dev tools, terminal sessions, and forwarding local model or app ports.
enable_remote_access com.apple.access_ssh com.openssh.sshd ssh.plist

echo "macOS server settings applied. Screen Sharing and SSH access granted to $server_user."
