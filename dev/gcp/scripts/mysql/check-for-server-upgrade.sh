#!/usr/bin/env bash
TARGET_VERSION=${TARGET_VERSION:-8.0.11}
OUTPUT_FILE=${OUTPUT_FILE:-upgrade-check.txt}

mysqlsh \
  --mysql \
  --host="${MYSQL_HOST}" \
  --port="${MYSQL_PORT}" \
  --user="${MYSQL_USER}" \
  -- util check-for-server-upgrade \
  --target-version="${TARGET_VERSION}" |
  tee "${OUTPUT_FILE}"
