#!/bin/bash

echo "try to run install.sh　at $(pwd)"

if [ "$CODESPACES" ==  true ]; then
  sudo apt-get update -y
  sudo apt-get install -y tig peco;
  for file in .*; do
        if [ -f $file ]; then
            ln -sf $file ~/$file
        fi
    done
fi
