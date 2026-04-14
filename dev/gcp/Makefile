.DEFAULT_GOAL := sh

# Required so devcontainer resolves the workspace from this Makefile's directory.
HERE := $(patsubst %/,%,$(dir $(abspath $(lastword $(MAKEFILE_LIST)))))
SUBDIR := $(patsubst /%,%,$(patsubst $(HERE)%,%,$(CURDIR)))

SCRIPT_TARGETS := $(shell find . -type f -name '*.sh' | sort)
script_dir = $(patsubst %/,%,$(dir $(1)))

.PHONY: $(SCRIPT_TARGETS)
$(SCRIPT_TARGETS):
	@docker compose run --rm -q \
		-w /workspace/$(SUBDIR) \
		--entrypoint direnv \
		dev \
		exec $(call script_dir,$@) bash $@ $(ARGS)

.PHONY: sh
sh:
	devcontainer exec --workspace-folder "$(HERE)" /bin/bash

.PHONY: build
build:
	devcontainer build --workspace-folder "$(HERE)" --config "$(HERE)/.devcontainer.build.json" --image-name dev-gcp:latest

.PHONY: up
up:
	devcontainer up --workspace-folder "$(HERE)"

.PHONY: down
down:
	docker compose down

.PHONY: clean
clean:
	docker compose down -v --remove-orphans

.PHONY: pull
pull:
	docker compose pull

.PHONY: login
login:
	docker compose run --rm -q dev gcloud auth login --no-launch-browser

.PHONY: auth
auth:
	docker compose run --rm -q dev gcloud auth application-default login --no-launch-browser

.PHONY: gke
gke:
	docker compose run --rm -q dev sh -cu \
		'gcloud container clusters get-credentials "$${CLUSTER_NAME}" --location "$${CLUSTER_LOCATION}" --project "$${GOOGLE_CLOUD_PROJECT}"'

.PHONY: proxy-sql
proxy-sql:
	docker compose --profile sql down proxy-sql
	docker compose --profile sql up -d proxy-sql

.PHONY: proxy-gke
proxy-gke:
	docker compose --profile gke down proxy-gke
	docker compose --profile gke up -d proxy-gke

.PHONY: adminer
adminer:
	docker compose --profile adminer --profile sql down adminer
	docker compose --profile adminer --profile sql up -d --wait proxy-sql adminer

.PHONY: mysql
mysql:
	docker compose --profile sql up -d --wait proxy-sql
	docker compose run --rm -q --entrypoint /workspace/scripts/mysql.sh dev $(ARGS)

.PHONY: k9s
k9s:
	docker compose --profile k9s --profile gke run --rm -q k9s
