# macOS Home Server

Run this in Terminal on the server Mac while logged in with the account you will use remotely. The script uses `sudo` for system settings and may also ask for your Mac login password to configure the screen lock.

```bash
make macos-server
```

Finish the [iCloud sync settings](README.md#server-icloud-settings) in System Settings for the server user.

- The Mac stays awake while idle. After 10 minutes of inactivity, the display turns off. The Mac requires authentication immediately after the display turns off or the screen saver starts.
- Screen Sharing and SSH allow the account that ran the script and users already on each service's allowlist. If a service allowed all users, the script switches it to its allowlist.
- Macs that support automatic restart after power failure start up when power returns. FileVault may require a local unlock before remote services become available after a cold boot.

## Remote development

Connect using the server Mac's hostname or IP address on your home network, or use its Tailscale address if both devices are connected to your tailnet. Use Screen Sharing for GUI apps and SSH for running CLI tools, builds, or model jobs. The repository's `make pkg` installs tmux and mise. You can use tmux to keep CLI sessions running through SSH disconnects and mise to load the runtimes specified by each project.

On the client, replace `USER` and `HOST` with your Mac account and server address. This example forwards a model API on the server's loopback port 11434 to the same loopback port on the client.

```bash
ssh -N -L 127.0.0.1:11434:127.0.0.1:11434 USER@HOST
```
