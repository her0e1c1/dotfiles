.PHONY: install dot pkg macos env github

ENVRC_SOURCE := $(CURDIR)/.envrc.example
ENVRC_TARGET ?= $(HOME)/.envrc
GITHUB_DOTFILES_DIR := $(HOME)/repo/dotfiles
GITHUB_DOTFILES_URL := https://github.com/her0e1c1/dotfiles.git

install:
	./install.sh

dot:
	./install.sh --install-dotfiles

pkg:
	./install.sh --install-packages

macos:
	./install.sh --configure-macos

env:
	@test -f "$(ENVRC_SOURCE)" || { echo "missing: $(ENVRC_SOURCE)"; exit 1; }
	@if [ -e "$(ENVRC_TARGET)" ]; then \
		echo "skipped: $(ENVRC_TARGET) already exists"; \
	else \
		cp "$(ENVRC_SOURCE)" "$(ENVRC_TARGET)"; \
		echo "created: $(ENVRC_TARGET)"; \
	fi

# Reuse the GitHub CLI login for HTTPS operations in the development checkout.
github:
	@if [ -e "$(GITHUB_DOTFILES_DIR)" ] || [ -L "$(GITHUB_DOTFILES_DIR)" ]; then \
		echo "$(GITHUB_DOTFILES_DIR) already exists" >&2; \
		exit 1; \
	fi
	@gh auth status --hostname github.com >/dev/null 2>&1 || \
		gh auth login --hostname github.com --git-protocol https --web
	git clone -c 'credential.https://github.com.helper=' \
		-c 'credential.https://github.com.helper=!gh auth git-credential' \
		"$(GITHUB_DOTFILES_URL)" "$(GITHUB_DOTFILES_DIR)"
