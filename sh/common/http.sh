
sample-for() {
    for i in 1 2 3; do echo $i; done
    for i in `seq 1 3`; do echo $i; done
    for i in `echo 1 2 3`; do echo $i; done
    for i in `echo "1 2 3"`; do echo $i; done
}

sample-read() {
    read -p "Please input your command: " input_command
    echo $input_command
}

sample_quote() {
    echo $'It\'s Shell Programming'  # bash/zsh
    echo 'It'\''s Shell Programming'
    echo "It's Shell Programming"
    echo 'It'"'"'s Shell Programming'
    local q="'"
    echo 'It${q}s Shell Programming'
}
sample_http_stream() {
    urldecode "data:application/octet-stream,field1%2Cfield2%0Afoo%2Cbar%0Agoo%2Cgai%0A"
}

# pstree, pkill, pgrep

# 便利
# tmux capture-pane

show-message () { echo ERROR > /dev/pts/0 }
