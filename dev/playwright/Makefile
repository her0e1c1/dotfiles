
.DEFAULT_GOAL := bash

.PHONY: dev
dev:
	devcontainer exec --workspace-folder $(CURDIR) /bin/bash

.PHONY: up
up:
	devcontainer up --workspace-folder $(CURDIR)

.PHONY: bash
bash:
	docker compose run --rm dev /bin/bash

.PHONY: down
down:
	docker compose down

.PHONY: clean
clean:
	docker compose down -v --remove-orphans

.PHONY: setup
setup:
	devcontainer build --workspace-folder $(CURDIR)
	docker compose run --rm dev npm ci

.PHONY: install
install:
	docker compose run --rm dev npm install

.PHONY: test
test:
	docker compose run --rm dev npm test

.PHONY: ui
ui:
	docker compose run --rm -p 9323:9323 dev npm run ui
