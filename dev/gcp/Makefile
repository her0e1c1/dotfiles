.DEFAULT_GOAL := sh

.PHONY: sh
sh:
	devcontainer exec --workspace-folder . /bin/bash

.PHONY: build
build:
	devcontainer build --workspace-folder .

.PHONY: up
up:
	devcontainer up --workspace-folder .

.PHONY: down
down:
	docker compose down

.PHONY: clean
clean:
	docker compose down -v --remove-orphans

.PHONY: pull
pull:
	docker compose pull

.PHONY: proxy
proxy:
	docker compose down proxy
	docker compose up -d proxy

.PHONY: adminer
adminer:
	docker compose --profile adminer down adminer
	docker compose --profile adminer up -d --wait proxy adminer

.PHONY: k9s
k9s:
	docker compose run --rm k9s

.PHONY: login
login:
	docker compose run --rm dev gcloud auth login --no-launch-browser

.PHONY: auth
auth:
	docker compose run --rm dev gcloud auth application-default login --no-launch-browser

.PHONY: kube
kube:
	docker compose run --rm dev sh -cu '\
		gcloud container clusters get-credentials "$${CLUSTER_NAME}" --location "$${CLUSTER_LOCATION}" --project "$${GOOGLE_CLOUD_PROJECT}" \
	'

.PHONY: mysql
mysql:
	docker compose up -d --wait proxy
	docker compose run --rm dev sh -cu \
		'mysql --protocol=TCP -h "$${MYSQL_HOST}" -P "$${MYSQL_PORT}" -u "$${MYSQL_USER}" $${MYSQL_DATABASE:+$${MYSQL_DATABASE}}'
