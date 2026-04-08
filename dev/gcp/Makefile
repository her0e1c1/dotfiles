.DEFAULT_GOAL := bash

SERVICE ?= gcp
SHELL_NAME ?= bash

.PHONY: bash
bash:
	docker compose run --rm --entrypoint $(SHELL_NAME) $(SERVICE)

.PHONY: bash-mysql
bash-mysql: bash
bash-mysql: SERVICE = mysql

.PHONY: up
up:
	docker compose up -d

.PHONY: proxy-up
proxy-up:
	docker compose up -d proxy

.PHONY: proxy-down
proxy-down:
	docker compose stop proxy

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

.PHONY: mysql
mysql: proxy-up
	docker compose run --rm --entrypoint sh mysql -c \
		'mysql --protocol=TCP -h "$${MYSQL_HOST}" -P "$${MYSQL_PORT}" -u "$${MYSQL_USER}" $${MYSQL_DATABASE:+$${MYSQL_DATABASE}}'
