#!/bin/bash

set -euo pipefail

echo "=== Dotfiles Installer ==="

# 環境変数
DOTFILES_DIR=~/dotfiles
REPO_URL="https://github.com/her0e1c1/dotfiles.git"
VSCODE_HOME="${VSCODE_HOME:-$HOME/Library/Application Support/Code/User}"

# CLIツール（通常のbrew install）
BREW_PACKAGES=(
  git
  gh
  tmux
  peco
  nvim
  direnv
  tig
  node
)

# GUIアプリ（brew install --cask）
BREW_CASK_PACKAGES=(
  docker
  visual-studio-code
)

### 1. dotfiles リポジトリの取得
if [ ! -d "$DOTFILES_DIR" ]; then
    echo "📦 Cloning dotfiles repo..."
    git clone "$REPO_URL" "$DOTFILES_DIR"
else
    echo "✅ dotfiles repo already exists."
fi

cd "$DOTFILES_DIR"

### 2. dotfiles のリンク作成
echo "🔗 Installing dotfiles..."
for file in .[^.]*; do
    if [ -f "$file" ] && [ "$file" != ".git" ]; then
        ln -sf "$DOTFILES_DIR/$file" "$HOME/$file"
        echo "  linked: $file"
    fi
done

### 3. Homebrew のインストール確認
if ! command -v brew >/dev/null 2>&1; then
    echo "🍺 Homebrew not found. Installing..."
    /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
    eval "$(/opt/homebrew/bin/brew shellenv 2>/dev/null || /usr/local/bin/brew shellenv)"
else
    echo "✅ Homebrew already installed."
fi

### 4. brew パッケージのインストール (CLI)
echo "📦 Installing brew packages..."
for pkg in "${BREW_PACKAGES[@]}"; do
    if brew list --formula | grep -q "^${pkg}\$"; then
        echo "  ✅ $pkg already installed."
    else
        echo "  ⬇️ Installing $pkg..."
        brew install "$pkg"
    fi
done

### 5. brew cask パッケージのインストール (GUI)
echo "📦 Installing brew cask packages..."
for cask in "${BREW_CASK_PACKAGES[@]}"; do
    if brew list --cask | grep -q "^${cask}\$"; then
        echo "  ✅ $cask already installed."
    else
        echo "  ⬇️ Installing $cask..."
        brew install --cask "$cask"
    fi
done

### 6. VSCode 設定ファイル
if [ -d "$VSCODE_HOME" ]; then
    echo "🧠 Installing VSCode settings..."
    ln -sf "$DOTFILES_DIR/.vscode/settings.json" "$VSCODE_HOME/settings.json"
    ln -sf "$DOTFILES_DIR/.vscode/keybindings.json" "$VSCODE_HOME/keybindings.json"
    echo "VSCode settings installed."
fi

### 7. ~/.bashrc を上書き作成
echo "source ~/.profile" > ~/.bashrc
echo "✅ Created/overwritten ~/.bashrc with 'source ~/.profile'"

### 8. ~/.bash_profile を上書き作成
echo "source ~/.bashrc" > ~/.bash_profile
echo "✅ Created/overwritten ~/.bash_profile with 'source ~/.bashrc'"

echo
echo "⚠️ To use bash as your main shell, consider running:"
echo "    chsh -s /bin/bash"
echo "and restart your terminal or logout/login."
echo

echo "🎉 Install completed!"

