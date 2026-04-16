#!/usr/bin/env bash
set -euo pipefail

: "${CLUSTER_NAME:?CLUSTER_NAME is required}"
: "${DEFAULT_LOCATION:?DEFAULT_LOCATION is required}"

gcloud container clusters get-credentials "${CLUSTER_NAME}" \
  --location "${DEFAULT_LOCATION}"
