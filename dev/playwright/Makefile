
.DEFAULT_GOAL := bash

TARGET = $(shell find . -type f -name '*.ts')
COMMANDS = install ci
NPM_SCRIPTS = $(shell node -p "Object.keys(require('./package.json').scripts).map((name) => name.replace(/:/g, '-')).join(' ')")

.PHONY: $(TARGET)
$(TARGET):
	docker compose run --rm dev npx playwright test $@

.PHONY: $(NPM_SCRIPTS)
$(NPM_SCRIPTS):
	docker compose run --rm dev npm run $(subst -,:,$@)

.PHONY: $(COMMANDS)
$(COMMANDS):
	docker compose run --rm dev npm $@

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
