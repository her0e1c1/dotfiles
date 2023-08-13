#!/bin/bash

echo "try to run install.sh　at $(pwd)"

if [ "$CODESPACES" ==  true ]; then
  sudo apt-get update -y
  sudo apt-get install -y tig peco tmux;
  for file in .*; do
        if [ -f $file ]; then
            cp -f $file ~/$file
        fi
    done
fi
