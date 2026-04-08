#!/usr/bin/env bash
TARGET_VERSION=${1:-8.4.8}

printf '%s\n' "${MYSQL_PWD}" | mysqlsh \
  --passwords-from-stdin \
  -- \
  util checkForServerUpgrade \
  "${MYSQL_USER}@${MYSQL_HOST}:${MYSQL_PORT}" \
  --target-version="${TARGET_VERSION}"
