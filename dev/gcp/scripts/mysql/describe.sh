#!/usr/bin/env bash
set -euo pipefail

if [[ -z "${CLOUDSQL_CONNECTION_NAME:-}" ]]; then
  echo "CLOUDSQL_CONNECTION_NAME is required" >&2
  exit 1
fi

instance_name="${INSTANCE_NAME:-${CLOUDSQL_CONNECTION_NAME##*:}}"
if [[ $# -gt 0 && "${1}" != -* ]]; then
  instance_name="${1}"
  shift
fi

if [[ -z "${instance_name}" ]]; then
  echo "failed to determine instance name" >&2
  exit 1
fi

gcloud sql instances describe "${instance_name}" "$@"
