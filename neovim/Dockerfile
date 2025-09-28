FROM ubuntu:24.04

# Ensure UTF-8 locale in the container (important for glyph rendering)
ENV LANG=C.UTF-8 \
    LC_ALL=C.UTF-8

# 基本ツールのインストール（ripgrep, fd, fzf, Node.js, Python など）
RUN apt-get update && DEBIAN_FRONTEND=noninteractive apt-get install -y \
    git \
    curl \
    unzip \
    tar \
    ripgrep \
    fd-find \
    fzf \
    bat \
    git-delta \
    universal-ctags \
    jq \
    eza \
    build-essential \
    python3-pip \
    python3-venv \
    nodejs npm \
    libfuse2 \
    ca-certificates \
    && rm -rf /var/lib/apt/lists

# Neovim (stable, arm64) を AppImage から導入（URL を固定）
RUN curl -LO https://github.com/neovim/neovim/releases/download/stable/nvim-linux-arm64.appimage \
    && chmod u+x nvim-linux-arm64.appimage \
    && ./nvim-linux-arm64.appimage --appimage-extract \
    && mv squashfs-root /opt/nvim \
    && ln -sf /opt/nvim/usr/bin/nvim /usr/local/bin/nvim \
    && rm nvim-linux-arm64.appimage

# ユーザー作成
RUN useradd -ms /bin/bash dev
USER dev
WORKDIR /home/dev

# LazyVim スターターをクローン
RUN git clone https://github.com/LazyVim/starter ~/.config/nvim \
    && rm -rf ~/.config/nvim/.git

# 初回プラグインインストール + Mason 更新/ツール導入
ARG MASON_TOOLS="lua-language-server stylua pyright debugpy ruff-lsp ruff black isort"
RUN nvim --headless \
  "+Lazy! sync" \
  "+MasonUpdate" \
  "+MasonInstall ${MASON_TOOLS}" \
  +qa

CMD ["nvim"]
