
# Install

## Run install script

Running the install script requires `sudo`.

```bash
curl -fsSL https://raw.githubusercontent.com/her0e1c1/dotfiles/master/install.sh | sudo bash
```

## Run make install

```bash
make install
```

## Clone dotfiles for development

```bash
make github
```

This creates an HTTPS checkout at `~/repo/dotfiles`. Sign in through the browser
if GitHub CLI prompts you. The checkout reuses that login for `git pull` and
`git push`. `make install` uses `~/dotfiles` for the installed configuration.

## Setup VSCODE

Sign in with github account by selecting `Backup and Sync Settings`

## Setup Guides

- [macOS setup](docs/setup/macos/README.md)

## Cheatsheets

- [Command cheatsheets](docs/cheatsheets/README.md)
