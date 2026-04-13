#!/usr/bin/env bash
set -euo pipefail

mysql \
  --protocol=TCP \
  -h "$MYSQL_HOST" \
  -P "$MYSQL_PORT" \
  -u "$MYSQL_USER" \
  ${MYSQL_DATABASE:+"$MYSQL_DATABASE"} \
  "$@"
