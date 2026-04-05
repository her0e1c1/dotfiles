
.DEFAULT_GOAL := bash

.PHONY: up
up:
	devcontainer up --workspace-folder $(CURDIR)

.PHONY: down
down:
	docker compose -f .devcontainer/compose.yaml down

.PHONY: setup
setup:
	# Build the devcontainer image before refreshing container-side dependencies.
	devcontainer build --workspace-folder $(CURDIR)
	# Install dependencies into the named volume without starting the long-lived devcontainer.
	docker compose -f .devcontainer/compose.yaml run --rm workspace npm ci

.PHONY: test
test:
	devcontainer exec --workspace-folder $(CURDIR) npm test

.PHONY: ui
ui:
	# Publish the Playwright UI port only for this one-off container run.
	docker compose -f .devcontainer/compose.yaml run --rm -p 9323:9323 workspace npm run ui

.PHONY: bash
bash:
	devcontainer exec --workspace-folder $(CURDIR) bash
