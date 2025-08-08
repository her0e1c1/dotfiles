
# Install

## Run install script

```bash
curl -fsSL https://raw.githubusercontent.com/her0e1c1/dotfiles/master/install.sh | bash
```

## Setup VSCODE

Sign in with github account by selecting `Backup and Sync Settings`

## Setup Mac

### PS1の `\H` を変更

```bash
sudo scutil --set LocalHostName $NAME
```

### 同じ文字を連続入力

```bash
defaults write -g ApplePressAndHoldEnabled -bool false
```
