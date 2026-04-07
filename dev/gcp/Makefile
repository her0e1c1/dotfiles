.DEFAULT_GOAL := bash

SERVICE ?= gcp
SHELL_NAME ?= bash

.PHONY: bash
bash:
	docker compose run --rm $(SERVICE) $(SHELL_NAME)

.PHONY: up
up:
	docker compose up -d

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
	docker compose run --rm gcp gcloud auth login --no-launch-browser

.PHONY: auth
auth:
	docker compose run --rm gcp gcloud auth application-default login --no-launch-browser
