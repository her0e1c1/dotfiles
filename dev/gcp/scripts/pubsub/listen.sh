#!/usr/bin/env bash
set -euo pipefail

cleanup() {
  gcloud pubsub subscriptions delete "$PUBSUB_SUBSCRIPTION" --quiet >/dev/null 2>&1 || true
}

trap cleanup EXIT INT TERM

gcloud pubsub subscriptions create "$PUBSUB_SUBSCRIPTION" \
  --topic "$PUBSUB_TOPIC"

while true; do
  message_data="$(
    gcloud pubsub subscriptions pull "$PUBSUB_SUBSCRIPTION" \
      --auto-ack \
      --limit=1 \
      --format='value(message.data)' ||
      true
  )"

  if [[ -n "$message_data" ]]; then
    printf '%s' "$message_data" | base64 --decode
    printf '\n'
  fi

  sleep 1
done
