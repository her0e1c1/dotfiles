# docker alias
# 以下のコマンドで、バックアップしたvolumeをマウントできる
# docker run -d -v "$name":/var/lib/mysql mysql 
docker_backup() {
    local data=$1 dir=$2
    docker run --rm --volumes-from "$data" -v $(pwd):/backup busybox sh -c "cd $dir && tar cvf /backup/backup.tar ."
}

docker_restore() {
    local name=$1 backup=$2
    docker volume create --name "$name"
    docker run --rm -v "$name:/volume" -v `pwd`:/backup centos tar xvf "/backup/$backup" -C /volume
}

docker_volume_exists() {
    local volume=$1
    if ! docker volume inspect $1 >/dev/null 2>&1; then
        echo "$1 volume doesn't exist"
        return 1
    fi
}

docker_volume_copy() {
    docker_volume_exists $1 || return 1
    [ $# -eq 2 ] || return 1
    local src=$1 dst=$2
    echo "copy $src $dst"
    docker volume create --name "$dst"
    # -Tは、既存のディレクトリにコピー(GNU)
    docker run --rm -v "$src":/src -v "$dst":/dst centos sh -c "cp -r /src -T /dst"
}

docker_volume_mount() {
    local name=$1 dest=/volume
    docker volume inspect $name
    echo "mount $name $dest"
    docker run --rm -itv "$name:$dest" --workdir "$dest" busybox sh
}

docker_volume_remove() {
    for volume in $@; do
        echo "remove $volume volume"
        docker volume rm $volume
    done
}

docker_process_exists() {
    if ! docker inspect $1 >/dev/null 2>&1; then
        echo "$1 process doesn't exist"
        return 1
    fi
}

docker_process_remove() {
    for p in `docker ps -q`; do
        docker rm -f $p
    done
}

docker_cp() {
    local src=$1  # container
    local dir=$2
    local dst=$3
    docker volume create --name "$dst"
    # -Tは、既存のディレクトリにコピー(GNU)
    docker run --rm --volumes-from "$src" -v "$dst":/backup centos sh -c "cp -r $dir -T /backup"
}

docker_run() { 
    if [ $# -eq 0 ]; then
        docker images
    else
        local name=$1; shift
        local cmd=/bin/bash
        [ $# -ne 0 ] && cmd=$@
        sh -c "docker run --rm -v /Users/mbp:/Users/mbp --detach-keys ctrl-q,q -it $name $cmd"
    fi
}
alias dr=docker_run

docker_volume_help() {
    echo "docker_volume              show volumes"
    echo "docker_volume NAME         enter NAME volume"
    echo "docker_volume SRC DST      copy volume SRC to DST"
    echo "docker_volume -r [NAME...] remove volumes"
}

docker_volume() {
    local rflag=false
    while getopts rh OPT; do
        case $OPT in
            r) rflag=true;;
            h) docker_volume_help; return 0;;
        esac
    done
    shift $((OPTIND - 1))
    if $rflag; then
        docker_volume_remove $@
    elif [ $# -eq 0 ]; then
        docker volume ls
    else
        docker_volume_exists $1 || return 1
        if [ $# -eq 2 ]; then
            docker_volume_copy $1 $2
        else
            docker_volume_mount $1
        fi
    fi
}
alias dv=docker_volume

docker_exec_help() {
    echo "docker_exec               show process"
    echo "docker_exec NAME          enter NAME process"
    echo "docker_exec NAME [CMD...] enter NAME process and run CMD"
    echo "docker_exec -r [NAME...]  remove processes"
}

docker_exec() {
    local rflag=false
    while getopts rh OPT; do
        case $OPT in
            r) rflag=true;;
            h) docker_exec_help; return 0;;
        esac
    done
    shift $((OPTIND - 1))
    if $rflag; then
        docker_volume_remove $@
    elif [ $# -eq 0 ]; then
        docker ps
    else
        local name=$1; shift
        local cmd=/bin/bash
        [ $# -ne 0 ] && cmd=$@
        sh -c "docker exec -it --detach-keys ctrl-q,q $name $cmd"
    fi
}
alias de=docker_exec
