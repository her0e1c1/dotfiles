FROM ubuntu:24.04

# Ensure UTF-8 locale in the container (important for glyph rendering)
ENV LANG=C.UTF-8 \
    LC_ALL=C.UTF-8

# Install basic tools (ripgrep, fd, fzf, Node.js, Python, etc.)
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

# Install Neovim (stable)
ARG TARGETARCH=arm64
RUN if [ "${TARGETARCH}" = "arm64" ]; then \
      curl -Lo nvim.tar.gz https://github.com/neovim/neovim/releases/download/stable/nvim-linux-arm64.tar.gz \
      && tar -xzf nvim.tar.gz \
      && mv nvim-linux-arm64 /opt/nvim \
      && ln -sf /opt/nvim/bin/nvim /usr/local/bin/nvim \
      && rm nvim.tar.gz; \
    else \
      curl -Lo nvim.appimage https://github.com/neovim/neovim/releases/download/stable/nvim-linux-${TARGETARCH}.appimage \
      && chmod u+x nvim.appimage \
      && ./nvim.appimage --appimage-extract \
      && mv squashfs-root /opt/nvim \
      && ln -sf /opt/nvim/usr/bin/nvim /usr/local/bin/nvim \
      && rm nvim.appimage; \
    fi

# Create a user (use root to avoid permission errors on WSL)
# RUN useradd -ms /bin/bash dev
# USER dev
# WORKDIR /home/dev

# Clone the LazyVim starter
RUN git clone https://github.com/LazyVim/starter ~/.config/nvim \
    && rm -rf ~/.config/nvim/.git

# Initial plugin install
RUN nvim --headless \
  "+Lazy! sync" \
  +qa

# Mason update/tool install (separate to avoid concurrent installs)
ARG MASON_TOOLS="lua-language-server stylua pyright debugpy ruff-lsp ruff black isort"
RUN nvim --headless \
  "+MasonUpdate" \
  "+MasonInstall ${MASON_TOOLS}" \
  +qa

CMD ["nvim"]
