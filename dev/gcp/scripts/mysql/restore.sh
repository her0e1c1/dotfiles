#!/usr/bin/env bash
set -euo pipefail

if [[ -z "${CLOUDSQL_CONNECTION_NAME:-}" ]]; then
  echo "CLOUDSQL_CONNECTION_NAME is required" >&2
  exit 1
fi

instance_name="${CLOUDSQL_CONNECTION_NAME##*:}"
restore_instance="${RESTORE_INSTANCE:-${instance_name}}"
backup_instance="${BACKUP_INSTANCE:-${instance_name}}"
backup_id="${BACKUP_ID:-}"
if [[ $# -gt 0 && "${1}" != -* ]]; then
  backup_id="${1}"
  shift
fi
extra_args=("$@")

if [[ -z "${backup_id}" ]]; then
  # Default to the newest successful backup for the source instance when no backup ID is provided.
  backup_id="$(
    gcloud sql backups list \
      --instance="${backup_instance}" \
      --filter='status=SUCCESSFUL' \
      --limit=1 \
      --format='value(id)'
  )"
fi

if [[ -z "${backup_id}" ]]; then
  echo "failed to determine backup ID for instance ${backup_instance}" >&2
  exit 1
fi

gcloud sql backups restore "${backup_id}" \
  --restore-instance="${restore_instance}" \
  --backup-instance="${backup_instance}" \
  "${extra_args[@]}"
