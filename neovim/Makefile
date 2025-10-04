.PHONY: build build-no-cache build-arm64 build-x86_64 run

## Build or update the image via docker compose
build:
	docker compose build --pull

## Build without cache (force full rebuild)
build-no-cache:
	docker compose build --no-cache --pull

## Build for ARM64 (macOS Apple Silicon)
build-arm64:
	docker compose build --pull --build-arg TARGETARCH=arm64

## Build for x86_64 (WSL/Intel Mac)
build-x86_64:
	docker compose build --pull --build-arg TARGETARCH=x86_64

## Run a one-off container in the current directory
run:
	docker compose run --rm nvim
