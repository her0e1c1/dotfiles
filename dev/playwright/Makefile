
.DEFAULT_GOAL := bash

COMPOSE_FILE := compose.yaml

.PHONY: up
up:
	devcontainer up --workspace-folder $(CURDIR)

.PHONY: down
down:
	docker compose -f $(COMPOSE_FILE) down

.PHONY: setup
setup:
	# Build the devcontainer image before refreshing container-side dependencies.
	devcontainer build --workspace-folder $(CURDIR)
	# Install dependencies into the named volume without starting the long-lived devcontainer.
	docker compose -f $(COMPOSE_FILE) run --rm workspace npm ci

.PHONY: install
install:
	# Refresh dependencies in the container-only node_modules volume without rebuilding the image.
	docker compose -f $(COMPOSE_FILE) run --rm workspace npm install

.PHONY: test
test:
	# Run tests in a one-off container so they use the same compose-based workflow as install/ui.
	docker compose -f $(COMPOSE_FILE) run --rm workspace npm test

.PHONY: ui
ui:
	# Publish the Playwright UI port only for this one-off container run.
	docker compose -f $(COMPOSE_FILE) run --rm -p 9323:9323 workspace npm run ui

.PHONY: bash
bash:
	devcontainer exec --workspace-folder $(CURDIR) bash
