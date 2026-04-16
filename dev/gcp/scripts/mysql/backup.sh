#!/usr/bin/env bash
set -euo pipefail

if [[ -z "${CLOUDSQL_CONNECTION_NAME:-}" ]]; then
  echo "CLOUDSQL_CONNECTION_NAME is required" >&2
  exit 1
fi

poll_interval_seconds="${POLL_INTERVAL_SECONDS:-10}"
instance_name="${CLOUDSQL_CONNECTION_NAME##*:}"

if [[ -z "${instance_name}" ]]; then
  echo "failed to parse instance name from CLOUDSQL_CONNECTION_NAME=${CLOUDSQL_CONNECTION_NAME}" >&2
  exit 1
fi

# Cloud SQL backup commands expect the instance ID, not the full project:region:instance connection name.
backup_id="$(
  gcloud sql backups create \
    --instance="${instance_name}" \
    --format='value(id)' \
    "$@"
)"

if [[ -z "${backup_id}" ]]; then
  echo "failed to create backup for instance ${instance_name}" >&2
  exit 1
fi

echo "Created backup ${backup_id} for instance ${instance_name}" >&2

while true; do
  status="$(
    gcloud sql backups describe "${backup_id}" \
      --instance="${instance_name}" \
      --format='value(status)'
  )"

  case "${status}" in
    SUCCESSFUL)
      echo "Backup ${backup_id} completed successfully" >&2
      exit 0
      ;;
    FAILED | DELETED | OBSOLETE | SKIPPED)
      echo "Backup ${backup_id} finished with status ${status}" >&2
      exit 1
      ;;
    "")
      echo "Backup ${backup_id} returned an empty status" >&2
      exit 1
      ;;
  esac

  sleep "${poll_interval_seconds}"
done
