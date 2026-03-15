#!/bin/bash

set -euo pipefail

# 環境変数
DOTFILES_DIR=~/dotfiles
REPO_URL="https://github.com/her0e1c1/dotfiles.git"
VSCODE_HOME="${VSCODE_HOME:-$HOME/Library/Application Support/Code/User}"

# 明示的にホーム配下へリンクする dotfiles / directories
DOTFILES_TO_LINK=(
  .tmux.conf
  .gitconfig
  .gitignore_global
  .profile
  .vimrc
  .config/direnv
  .codex/skills
  .copilot/skills
)

# OSの判定
detect_os() {
  if [[ "$OSTYPE" == "darwin"* ]]; then
    echo "macos"
  elif [[ -f /etc/os-release ]]; then
    . /etc/os-release
    echo "${ID}"
  else
    echo "unknown"
  fi
}

OS=$(detect_os)

# CLIツール（Homebrew用）
BREW_PACKAGES=(
  git
  gh
  tmux
  fzf
  direnv
  tig
)

# CLIツール（apt用）
# dockerは個別にインストールすること
APT_PACKAGES=(
  git
  gh
  tmux
  fzf
  direnv
  tig
  curl
  build-essential
  locales
)

# GUIアプリ（brew install --cask）
BREW_CASK_PACKAGES=(
  # codex
  docker-desktop
  google-chrome
  visual-studio-code
  # Required for nvim to render Nerd Font glyphs correctly
  # Settings -> Text -> Font -> Change... -> JetBrainsMono Nerd Font
  font-hack-nerd-font
)

# ヘルプメッセージ表示
show_help() {
  cat <<EOF
=== Dotfiles Installer ===

Usage: $0 [OPTIONS]

Options:
  -h, --help           Show this help message
  --install-dotfiles   Install dotfiles
  --install-packages   Install package manager and packages
  --install-vscode     Install VSCode settings
  --install-shell      Install shell configuration

Examples:
  $0                   Full installation
  $0 --install-vscode  Install only VSCode settings
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

  for file in "${DOTFILES_TO_LINK[@]}"; do
    local source_path="$DOTFILES_DIR/$file"
    local target_path="$HOME/$file"

    if [ -e "$file" ]; then
      mkdir -p "$(dirname "$target_path")"
      if [ -L "$target_path" ] || [ ! -e "$target_path" ]; then
        ln -sfn "$source_path" "$target_path"
        echo "  linked: $file"
      else
        echo "  skipped: $file already exists and is not a symlink"
      fi
    else
      echo "  skipped: $file not found"
    fi
  done

  mkdir -p "$HOME/.codex"
  # Required so Codex can load repo-managed prompt templates from ~/.codex/prompts.
  if [ -L "$HOME/.codex/prompts" ] || [ ! -e "$HOME/.codex/prompts" ]; then
    ln -sfn "$DOTFILES_DIR/.codex/prompts" "$HOME/.codex/prompts"
    echo "  linked: .codex/prompts"
  else
    echo "  skipped: ~/.codex/prompts already exists and is not a symlink"
  fi

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

# aptパッケージのインストール
install_apt_packages() {
  info "Updating apt package list..."
  sudo apt update || error "Failed to update apt package list"

  info "Installing apt packages..."
  for pkg in "${APT_PACKAGES[@]}"; do
    if dpkg -l | grep -q "^ii  ${pkg}"; then
      echo "  ✅ $pkg already installed."
    else
      echo "  ⬇️ Installing $pkg..."
      sudo apt install -y "$pkg" || error "Failed to install $pkg"
    fi
  done

  # ロケールの設定
  info "Configuring locale..."
  if ! locale -a | grep -q "en_US.utf8"; then
    info "Generating en_US.UTF-8 locale..."
    sudo sed -i '/en_US.UTF-8/s/^# //g' /etc/locale.gen
    sudo locale-gen en_US.UTF-8 || error "Failed to generate locale"
  else
    echo "  ✅ en_US.UTF-8 locale already generated."
  fi

  sudo update-locale LANG=en_US.UTF-8 LC_ALL=en_US.UTF-8 LANGUAGE=en_US.UTF-8 || error "Failed to update locale"

  success "Apt packages installation completed"
}

# VSCode設定ファイルのインストール
install_vscode_settings() {
  # Linux用のVSCode設定ディレクトリも考慮
  if [[ "$OS" == "ubuntu" ]] || [[ "$OS" == "debian" ]]; then
    VSCODE_HOME="$HOME/.config/Code/User"
  fi

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
  echo "source ~/.profile" >~/.bashrc || error "Failed to create ~/.bashrc"
  success "Created/overwritten ~/.bashrc with 'source ~/.profile'"

  echo "source ~/.bashrc" >~/.bash_profile || error "Failed to create ~/.bash_profile"
  success "Created/overwritten ~/.bash_profile with 'source ~/.bashrc'"
}

# メイン処理
main() {
  local should_install_dotfiles=false
  local should_install_packages=false
  local should_install_vscode=false
  local should_install_shell=false
  local has_install_flag=false

  # オプション解析
  while [[ $# -gt 0 ]]; do
    case $1 in
    -h | --help)
      show_help
      exit 0
      ;;
    --install-dotfiles)
      should_install_dotfiles=true
      has_install_flag=true
      shift
      ;;
    --install-packages)
      should_install_packages=true
      has_install_flag=true
      shift
      ;;
    --install-vscode)
      should_install_vscode=true
      has_install_flag=true
      shift
      ;;
    --install-shell)
      should_install_shell=true
      has_install_flag=true
      shift
      ;;
    *)
      error "Unknown option: $1"
      ;;
    esac
  done

  # フラグ未指定時は全項目をインストールする
  if [ "$has_install_flag" = false ]; then
    should_install_dotfiles=true
    should_install_packages=true
    should_install_vscode=true
    should_install_shell=true
  fi

  echo "=== Dotfiles Installer ==="
  info "Detected OS: $OS"

  # dotfilesリポジトリの取得
  if [ "$should_install_dotfiles" = true ]; then
    clone_dotfiles
    install_dotfiles
  fi

  # パッケージマネージャーとパッケージのインストール
  if [ "$should_install_packages" = true ]; then
    if [[ "$OS" == "macos" ]]; then
      # macOS: Homebrewを使用
      install_homebrew
      install_brew_packages
    elif [[ "$OS" == "ubuntu" ]] || [[ "$OS" == "debian" ]]; then
      # Ubuntu/Debian: aptを使用
      install_apt_packages
    else
      echo "⚠️ Unsupported OS: $OS. Skipping package installation."
    fi
  fi

  # VSCode設定
  if [ "$should_install_vscode" = true ]; then
    install_vscode_settings
  fi

  # シェル設定
  if [ "$should_install_shell" = true ]; then
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
