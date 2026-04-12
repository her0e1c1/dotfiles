.DEFAULT_GOAL := sh

# Required so devcontainer resolves the workspace from this Makefile's directory.
WORKSPACE_FOLDER := $(patsubst %/,%,$(dir $(abspath $(lastword $(MAKEFILE_LIST)))))

.PHONY: sh
sh:
	devcontainer exec --workspace-folder "$(WORKSPACE_FOLDER)" /bin/bash

.PHONY: build
build:
	devcontainer build --workspace-folder "$(WORKSPACE_FOLDER)" --config "$(WORKSPACE_FOLDER)/.devcontainer.build.json" --image-name dev-gcp:latest

.PHONY: up
up:
	devcontainer up --workspace-folder "$(WORKSPACE_FOLDER)"

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
	docker compose run --rm dev gcloud auth login --no-launch-browser

.PHONY: auth
auth:
	docker compose run --rm dev gcloud auth application-default login --no-launch-browser

.PHONY: gke
gke:
	docker compose run --rm dev sh -cu \
		'gcloud container clusters get-credentials "$${CLUSTER_NAME}" --location "$${CLUSTER_LOCATION}" --project "$${GOOGLE_CLOUD_PROJECT}"'

.PHONY: proxy-sql
proxy-sql:
	docker compose down proxy-sql
	docker compose up -d proxy-sql

.PHONY: proxy-gke
proxy-gke:
	docker compose down proxy-gke
	docker compose up -d proxy-gke

.PHONY: adminer
adminer:
	docker compose --profile adminer down adminer
	docker compose --profile adminer up -d --wait proxy-sql adminer

.PHONY: mysql
mysql:
	docker compose up -d --wait proxy-sql
	docker compose run --rm dev sh -cu \
		'mysql --protocol=TCP -h "$${MYSQL_HOST}" -P "$${MYSQL_PORT}" -u "$${MYSQL_USER}" $${MYSQL_DATABASE:+$${MYSQL_DATABASE}}'

.PHONY: k9s
k9s:
	docker compose run --rm k9s
