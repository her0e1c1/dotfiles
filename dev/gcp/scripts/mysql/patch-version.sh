#!/usr/bin/env bash
set -euo pipefail

if [[ -z "${CLOUDSQL_CONNECTION_NAME:-}" ]]; then
  echo "CLOUDSQL_CONNECTION_NAME is required" >&2
  exit 1
fi

VERSION="${1:-${VERSION:-MYSQL_8_4}}"
instance_name="${CLOUDSQL_CONNECTION_NAME##*:}"

gcloud sql instances patch "${instance_name}" --database-version="${VERSION}"
