#!/usr/bin/env bash
set -euo pipefail

build_id="$(
  gcloud builds triggers run "$BUILD_TRIGGER_ID" \
    --location "$DEFAULT_LOCATION" \
    --project "$GOOGLE_CLOUD_PROJECT" \
    --format='value(metadata.build.id)'
)"

if [[ -z "$build_id" ]]; then
  echo "Failed to get build_id from gcloud builds triggers run output" >&2
  exit 1
fi

echo "Started build: $build_id"

gcloud beta builds log "$build_id" \
  --region "$DEFAULT_LOCATION" \
  --project "$GOOGLE_CLOUD_PROJECT" \
  --stream

status="$(
  gcloud builds describe "$build_id" \
    --region "$DEFAULT_LOCATION" \
    --project "$GOOGLE_CLOUD_PROJECT" \
    --format='value(status)'
)"

case "$status" in
  SUCCESS)
    echo "Build succeeded: $build_id"
    ;;
  FAILURE|INTERNAL_ERROR|TIMEOUT|CANCELLED|EXPIRED)
    echo "Build finished with status=$status: $build_id" >&2
    exit 1
    ;;
  *)
    echo "Build finished with unexpected status=$status: $build_id" >&2
    exit 1
    ;;
esac
