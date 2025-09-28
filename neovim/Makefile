.PHONY: build build-no-cache run

## Build or update the image via docker compose
build:
	docker compose build --pull

## Build without cache (force full rebuild)
build-no-cache:
	docker compose build --no-cache --pull

## Run a one-off container in the current directory
run:
	docker compose run --rm nvim
