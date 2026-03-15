.PHONY: install dot pkg env

ENVRC_SOURCE := $(CURDIR)/.envrc.example
ENVRC_TARGET ?= $(HOME)/.envrc

install:
	./install.sh

dot:
	./install.sh --install-dotfiles

pkg:
	./install.sh --install-packages

env:
	@test -f "$(ENVRC_SOURCE)" || { echo "missing: $(ENVRC_SOURCE)"; exit 1; }
	@if [ -e "$(ENVRC_TARGET)" ]; then \
		echo "skipped: $(ENVRC_TARGET) already exists"; \
	else \
		cp "$(ENVRC_SOURCE)" "$(ENVRC_TARGET)"; \
		echo "created: $(ENVRC_TARGET)"; \
	fi
