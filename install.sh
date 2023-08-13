#!/bin/bash

echo "try to run install.sh"

if [ "$CODESPACES" ==  true ]; then
  apt-get update -y
  apt-get install -y tig peco;
fi
