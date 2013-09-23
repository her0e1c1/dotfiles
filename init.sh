#!/bin/sh

mkdir -p ~/.ssh
chmod 700 ~/.ssh

if uname -a|grep -q "Ubuntu"
then
    chsh -s /usr/bin/zsh
fi

