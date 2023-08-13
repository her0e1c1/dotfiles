#!/bin/bash

echo "try to run install.sh"

if [ "$CODESPACES" ==  true ]; then
  sudo apt-get update -y
  sudo apt-get install -y tig peco;
fi
