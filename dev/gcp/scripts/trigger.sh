#!/usr/bin/env bash
set -euo pipefail

gcloud builds triggers run "$BUILD_TRIGGER_ID" \
  --location "$DEFAULT_LOCATION" \
  --project "$GOOGLE_CLOUD_PROJECT"
