
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

.PHONY: bash
bash:
	devcontainer exec --workspace-folder $(CURDIR) bash
