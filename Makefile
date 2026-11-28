.PHONY: install dot pkg macos macos-server env github

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

# iCloud sync is configured per user, so open its settings from a graphical login.
macos-server:
	@test "$$(uname -s)" = Darwin || { echo "macos-server requires macOS" >&2; exit 1; }
	./scripts/macos-server.sh
	@printf '\n%s\n' 'Finish iCloud setup in the server user’s graphical session (Screen Sharing also works).' \
	  'In System Settings > Apple Account > iCloud > Drive, turn off Sync this Mac and verify it is off.' \
	  'Under iCloud > See All, turn off the other app sync settings you do not need.'
	@if [ -z "$${SSH_CONNECTION:-}" ] && [ "$$(launchctl managername 2>/dev/null)" = Aqua ]; then \
		open -b com.apple.systempreferences 'x-apple.systempreferences:com.apple.systempreferences.AppleIDSettings:icloud' \
			|| echo 'Open System Settings manually to finish iCloud setup.' >&2; \
	fi

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
