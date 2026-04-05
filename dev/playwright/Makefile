
.DEFAULT_GOAL := bash

.PHONY: up
up:
	devcontainer up --workspace-folder $(CURDIR)

.PHONY: down
down:
	docker compose -f .devcontainer/compose.yaml down

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
