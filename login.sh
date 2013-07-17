#!/bin/sh
ssh-add ~/.ssh/git_id_rsa

old_path=`pwd`
for path in "/home/ishii/s/csphinx" "/home/ishii/github/sphinx_document"; do
 cd $path;
 watchmedo shell-command --patterns='*.rst' --recursive --wait --command="make html" & # 2>&1 /dev/null &
done
cd $old_path
