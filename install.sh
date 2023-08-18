#!/bin/bash

echo "try to run install.sh　at $(pwd)"

if [ "$CODESPACES" ==  true ]; then
  SUDO=''
  if (( $EUID != 0 )); then
      SUDO='sudo'
  fi
  $SUDO apt-get update -y
  $SUDO sudo apt-get install -y tig peco tmux vim;
  for file in .*; do
        if [ -f $file ]; then
            cp -f $file ~/$file
        fi
    done
fi
