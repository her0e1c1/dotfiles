#!/usr/bin/env bash
set -euo pipefail

if [[ -z "${CLOUDSQL_CONNECTION_NAME:-}" ]]; then
  echo "CLOUDSQL_CONNECTION_NAME is required" >&2
  exit 1
fi

instance_name="${CLOUDSQL_CONNECTION_NAME##*:}"
MACHINE_TYPE="${1:-${MACHINE_TYPE:-}}"

if [[ -z "${MACHINE_TYPE}" ]]; then
  # Default to the current tier so callers can omit the argument without hard-coding an arbitrary machine type.
  MACHINE_TYPE="$(
    gcloud sql instances describe "${instance_name}" \
      --format='value(settings.tier)'
  )"
fi

if [[ -z "${MACHINE_TYPE}" ]]; then
  echo "failed to determine machine type for instance ${instance_name}" >&2
  exit 1
fi

gcloud sql instances patch "${instance_name}" --tier="${MACHINE_TYPE}"
