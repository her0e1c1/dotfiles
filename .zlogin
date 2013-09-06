if [ `uname` = "FreeBSD" ]; then
    eval `ssh-agent`
    ssh-add ~/.ssh/git_id_rsa
fi
