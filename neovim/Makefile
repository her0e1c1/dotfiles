.PHONY: build build-no-cache build-arm64 build-x86_64 run

UNAME_M := $(shell uname -m)
IS_ARM64 := $(if $(filter arm64 aarch64,$(UNAME_M)),1,0)
PLATFORM ?= $(if $(IS_ARM64),linux/arm64,linux/amd64)
TARGETARCH ?= $(if $(IS_ARM64),arm64,x86_64)

## Build or update the image via docker compose
build:
ifeq ($(IS_ARM64),1)
	$(MAKE) build-arm64
else
	$(MAKE) build-x86_64
endif

## Build without cache (force full rebuild)
build-no-cache:
	PLATFORM=$(PLATFORM) TARGETARCH=$(TARGETARCH) docker compose build --no-cache --pull

## Build for ARM64 (macOS Apple Silicon)
build-arm64:
	PLATFORM=linux/arm64 TARGETARCH=arm64 docker compose build --pull

## Build for x86_64 (WSL/Intel Mac)
build-x86_64:
	PLATFORM=linux/amd64 TARGETARCH=x86_64 docker compose build --pull

## Run a one-off container in the current directory
run:
	PLATFORM=$(PLATFORM) TARGETARCH=$(TARGETARCH) docker compose run --rm nvim
