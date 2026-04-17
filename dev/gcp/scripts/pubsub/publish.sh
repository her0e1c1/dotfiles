#!/usr/bin/env bash
set -euo pipefail

if [[ -z "${TOPIC:-}" ]]; then
  echo "TOPIC is required" >&2
  exit 1
fi

json_data="${1:-${JSON_DATA:-}}"

if [[ -z "${json_data}" ]]; then
  echo "JSON_DATA is required" >&2
  exit 1
fi

gcloud pubsub topics publish "${TOPIC}" --message="${json_data}"
