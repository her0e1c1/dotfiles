#!/usr/bin/env bash
set -euo pipefail

: "${CLUSTER_NAME:?CLUSTER_NAME is required}"
: "${DEFAULT_LOCATION:?DEFAULT_LOCATION is required}"

current_ip="$(curl -fsSL https://api.ipify.org)"

if [[ ! "${current_ip}" =~ ^[0-9]+(\.[0-9]+){3}$ ]]; then
  echo "Failed to detect a valid IPv4 address: ${current_ip}" >&2
  exit 1
fi

gcloud container clusters update "${CLUSTER_NAME}" \
  --location "${DEFAULT_LOCATION}" \
  --enable-master-authorized-networks \
  --master-authorized-networks "${current_ip}/32"

echo "Authorized ${current_ip}/32 on cluster ${CLUSTER_NAME} (${DEFAULT_LOCATION})"
