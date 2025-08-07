#!/bin/bash

set -euo pipefail

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

# ヘルプメッセージ表示
show_help() {
    cat << EOF
=== Dotfiles Installer ===

Usage: $0 [OPTIONS]

Options:
  -h, --help          Show this help message
  --skip-dotfiles     Skip dotfiles installation
  --skip-homebrew     Skip Homebrew installation
  --skip-packages     Skip brew package installation
  --skip-vscode       Skip VSCode settings installation
  --skip-shell        Skip shell configuration

Examples:
  $0                  Full installation
  $0 --skip-packages  Install everything except brew packages
EOF
}

# エラーメッセージ出力
error() {
    echo "❌ Error: $1" >&2
    exit 1
}

# 成功メッセージ出力
success() {
    echo "✅ $1"
}

# 情報メッセージ出力
info() {
    echo "📦 $1"
}

# dotfilesリポジトリの取得
clone_dotfiles() {
    if [ ! -d "$DOTFILES_DIR" ]; then
        info "Cloning dotfiles repo..."
        git clone "$REPO_URL" "$DOTFILES_DIR" || error "Failed to clone dotfiles repository"
    else
        success "dotfiles repo already exists."
    fi
}

# dotfilesのリンク作成
install_dotfiles() {
    info "Installing dotfiles..."
    cd "$DOTFILES_DIR" || error "Failed to change directory to $DOTFILES_DIR"
    
    for file in .[^.]*; do
        if [ -f "$file" ] && [ "$file" != ".git" ]; then
            ln -sf "$DOTFILES_DIR/$file" "$HOME/$file"
            echo "  linked: $file"
        fi
    done
    success "Dotfiles installation completed"
}

# Homebrewのインストール
install_homebrew() {
    if ! command -v brew >/dev/null 2>&1; then
        info "Installing Homebrew..."
        /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)" || error "Failed to install Homebrew"
        eval "$(/opt/homebrew/bin/brew shellenv 2>/dev/null || /usr/local/bin/brew shellenv)" || error "Failed to setup Homebrew environment"
    else
        success "Homebrew already installed."
    fi
}

# brewパッケージのインストール
install_brew_packages() {
    info "Installing brew packages..."
    for pkg in "${BREW_PACKAGES[@]}"; do
        if brew list --formula | grep -q "^${pkg}\$"; then
            echo "  ✅ $pkg already installed."
        else
            echo "  ⬇️ Installing $pkg..."
            brew install "$pkg" || error "Failed to install $pkg"
        fi
    done

    info "Installing brew cask packages..."
    for cask in "${BREW_CASK_PACKAGES[@]}"; do
        if brew list --cask | grep -q "^${cask}\$"; then
            echo "  ✅ $cask already installed."
        else
            echo "  ⬇️ Installing $cask..."
            brew install --cask "$cask" || error "Failed to install $cask"
        fi
    done
    success "Brew packages installation completed"
}

# VSCode設定ファイルのインストール
install_vscode_settings() {
    if [ -d "$VSCODE_HOME" ]; then
        info "Installing VSCode settings..."
        ln -sf "$DOTFILES_DIR/.vscode/settings.json" "$VSCODE_HOME/settings.json" || error "Failed to link VSCode settings.json"
        ln -sf "$DOTFILES_DIR/.vscode/keybindings.json" "$VSCODE_HOME/keybindings.json" || error "Failed to link VSCode keybindings.json"
        success "VSCode settings installed"
    else
        echo "⚠️ VSCode not found, skipping settings installation"
    fi
}

# シェル設定
configure_shell() {
    info "Configuring shell..."
    echo "source ~/.profile" > ~/.bashrc || error "Failed to create ~/.bashrc"
    success "Created/overwritten ~/.bashrc with 'source ~/.profile'"
    
    echo "source ~/.bashrc" > ~/.bash_profile || error "Failed to create ~/.bash_profile"
    success "Created/overwritten ~/.bash_profile with 'source ~/.bashrc'"
}

# メイン処理
main() {
    local skip_dotfiles=false
    local skip_homebrew=false
    local skip_packages=false
    local skip_vscode=false
    local skip_shell=false

    # オプション解析
    while [[ $# -gt 0 ]]; do
        case $1 in
            -h|--help)
                show_help
                exit 0
                ;;
            --skip-dotfiles)
                skip_dotfiles=true
                shift
                ;;
            --skip-homebrew)
                skip_homebrew=true
                shift
                ;;
            --skip-packages)
                skip_packages=true
                shift
                ;;
            --skip-vscode)
                skip_vscode=true
                shift
                ;;
            --skip-shell)
                skip_shell=true
                shift
                ;;
            *)
                error "Unknown option: $1"
                ;;
        esac
    done

    echo "=== Dotfiles Installer ==="

    # dotfilesリポジトリの取得
    if [ "$skip_dotfiles" = false ]; then
        clone_dotfiles
        install_dotfiles
    fi

    # Homebrewのインストール
    if [ "$skip_homebrew" = false ]; then
        install_homebrew
    fi

    # brewパッケージのインストール
    if [ "$skip_packages" = false ]; then
        install_brew_packages
    fi

    # VSCode設定
    if [ "$skip_vscode" = false ]; then
        install_vscode_settings
    fi

    # シェル設定
    if [ "$skip_shell" = false ]; then
        configure_shell
    fi

    echo
    echo "⚠️ To use bash as your main shell, consider running:"
    echo "    chsh -s /bin/bash"
    echo "and restart your terminal or logout/login."
    echo

    echo "🎉 Install completed!"
}

# スクリプト実行
main "$@"

