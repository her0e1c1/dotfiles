#!/usr/bin/env bash
set -euo pipefail

script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

bash "${script_dir}/auth.sh"

if [[ -v USE_AUTHORIZE_NETWORK ]]; then
  bash "${script_dir}/authorize-network.sh"
fi
