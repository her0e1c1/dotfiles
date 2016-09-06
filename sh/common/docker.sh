# docker alias
# 以下のコマンドで、バックアップしたvolumeをマウントできる
# docker run -d -v "$name":/var/lib/mysql mysql 
docker-volume-backup() {
    local data=$1 dir=$2
    docker run --rm --volumes-from "$data" -v $(pwd):/backup busybox sh -c "cd $dir && tar cvf /backup/backup.tar ."
}

docker-volume-dump() {
    local volume=$1
    docker run --rm -v $volume:/volume -v $(pwd):/backup busybox sh -c "cd /volume && tar cvf /backup/backup.tar ."
}

docker-volume-restore() {
    local name=$1 backup=$2
    docker volume create --name "$name"
    docker run --rm -v "$name:/volume" -v `pwd`:/backup centos tar xvf "/backup/$backup" -C /volume
}

docker-volume-exists() {
    local volume=$1
    if ! docker volume inspect $1 >/dev/null 2>&1; then
        echo "$1 volume doesn't exist"
        return 1
    fi
}

docker-volume-copy() {
    docker-volume-exists $1 || return 1
    [ $# -eq 2 ] || return 1
    local src=$1 dst=$2
    echo "copy $src $dst"
    docker volume create --name "$dst"
    # -Tは、既存のディレクトリにコピー(GNU)
    docker run --rm -v "$src":/src -v "$dst":/dst centos sh -c "cp -r /src -T /dst"
}

docker-volume-mount() {
    local name=$1 dest=/volume
    docker volume inspect $name
    echo "mount $name $dest"
    docker run --rm -itv "$name:$dest" --workdir "$dest" busybox sh
}

docker-volume-remove() {
    for volume in $@; do
        echo "remove $volume volume"
        docker volume rm $volume
    done
}

docker-process-exists() {
    if ! docker inspect $1 >/dev/null 2>&1; then
        echo "$1 process doesn't exist"
        return 1
    fi
}

docker-process-remove() {
    docker rm `docker ps -aq`
    # for p in `docker ps -qa`; do docker rm -f $p; done
}

docker-cp() {
    local src=$1  # container
    local dir=$2
    local dst=$3
    docker volume create --name "$dst"
    # -Tは、既存のディレクトリにコピー(GNU)
    docker run --rm --volumes-from "$src" -v "$dst":/backup centos sh -c "cp -r $dir -T /backup"
}

docker-run() { 
    if [ $# -eq 0 ]; then
        docker images
    else
        local name=$1; shift
        local cmd=/bin/bash
        [ $# -ne 0 ] && cmd=$@
        sh -c "docker run --rm -v /Users/mbp:/Users/mbp -w /Users/mbp --detach-keys ctrl-q,q -it $name $cmd"
    fi
}
alias dr=docker-run

docker-volume-help() {
    echo "docker_volume              show volumes"
    echo "docker_volume NAME         enter NAME volume"
    echo "docker_volume SRC DST      copy volume SRC to DST"
    echo "docker_volume -r [NAME...] remove volumes"
}

docker-volume() {
    local rflag=false
    while getopts rh OPT; do
        case $OPT in
            r) rflag=true;;
            h) docker_volume_help; return 0;;
        esac
    done
    shift $((OPTIND - 1))
    if $rflag; then
        docker-volume-remove $@
    elif [ $# -eq 0 ]; then
        docker volume ls
    else
        docker-volume-exists $1 || return 1
        if [ $# -eq 2 ]; then
            docker-volume-copy $1 $2
        else
            docker-volume-mount $1
        fi
    fi
}
alias dv=docker-volume

docker-exec-help() {
    echo "docker_exec               show process"
    echo "docker_exec NAME          enter NAME process"
    echo "docker_exec NAME [CMD...] enter NAME process and run CMD"
    echo "docker_exec -r [NAME...]  remove processes"
}

docker-exec() {
    local rflag=false
    while getopts rh OPT; do
        case $OPT in
            r) rflag=true;;
            h) docker-exec-help; return 0;;
        esac
    done
    shift $((OPTIND - 1))
    if $rflag; then
        docker-volume-remove $@
    elif [ $# -eq 0 ]; then
        docker ps
    else
        local name=$1; shift
        if [ $# -ne 0 ]; then
            # $@はかなり特殊な変数(配列っぽい動きする。そのため他の変数に代入できないっぽい)
            docker exec -it --detach-keys ctrl-q,q $name $@
        else
            docker exec -it --detach-keys ctrl-q,q $name /bin/bash
        fi
    fi
}
alias de=docker-exec
alias dei="docker exec -i"

docker-compose-update() {
    local file=docker-compose.yml
    local project=docker
    local down=false
    while getopts df:p: OPT; do
        case $OPT in
            d) down=true;;
            f) file=$OPTARG;;
            p) project=$OPTARG;;
        esac
    done
    shift $((OPTIND - 1))

    docker-compose -f $file -p $project down -v
    $down && return 0
    docker-compose -f $file -p $project up -d
}

docker-rename-image() { docker tag $1 $2; docker rmi $1 }

# 1ファイルを指定して、ホスト側で書き換えたのをうわ書き
docker-edit() {
    local name=$1;
    local p=$2;
    local base=`basename $p`
    local temp="`mktemp`_$base"
    local dest="$name:$p"
    docker cp $dest $temp
    emacsclient -t $temp
    docker cp $temp $dest
}

docker-sync-help() {
    echo "docker-sync NAME SRC [watchmedo options]"
}

# docker run のタイミングでsyncもできるようにするか(指定したディレクトリを監視するみたいな)
# または、cp cpを2回繰り返す! (または docker-sync name /path ./host_side)
# host側のイベントを取りにいけない...
docker-sync () {
    if [ $# -eq 1 ]; then
        docker exec -it $1 /bin/bash
        return 0
    fi

    local name=$1; shift
    local src=$1; shift
    local sync=".docker-sync/$name/`basename $src`"
    local trim=`perl -E '\$_=\$ARGV[0]; s#/*\$## and say' $src`
    local working=`docker-working $name`
    
    echo "cd $working"
    \cd $working

    # コンテナに必ずしもrsyncがインストールされているとは限らないが必須
    if ! docker exec $name which rsync; then
        echo "Install rsync on $name"
        docker exec $name apt-get update -y;
        if ! docker exec $name apt-get install -y rsync; then
            return 1
        fi
    fi

    if ! docker exec $name test -d $sync; then
        local d=`dirname $sync`
        docker exec $name sh -c "[ ! -d $d ] && mkdir -p $d"

        # echo "cp $src $sync on host"
        # docker exec $name cp -r $src $sync

        echo "rsync $sync on host"
        # docker exec $name rsync -avz --exclude '*.git*' $trim/ $sync
        docker exec $name rsync -avz $trim/ $sync
    fi
    if docker exec $name test -d $sync; then
        if [ -d "$sync" ]; then
            local d=~/$sync
            if [ ! -d $d ]; then
                mkdir -p `dirname $d`
                echo "create a symbol link $d"
                ln -s "$sync" $d
            fi
            echo "start sync ..."
            watchmedo shell-command -R "$sync" -c "docker exec $name rsync -avz --exclude '*.git*' $sync/ $trim" $@
        else
            echo "You can't sync on `pwd`. Go to $sync on host"
        fi
    else
        echo "$sync dir is not found on docker."
    fi
}

docker-working() {
    docker inspect $1 | python -c 'import sys, json; print(json.loads(sys.stdin.read())[0]["Mounts"][0]["Source"])'
}

docker-commit () {
    local name=$1; shift;
    local repo=$1; shift;
    if [ $# -ge 3 ]; then
        if [ -n `docker images -q $repo` ]; then
            docker exec $name $@ && docker commit $name $repo
        else
            echo "No repositry $repo"
        fi
    fi
}

docker-compose-all() {
    docker-compose `perl -E 'say map {" -f \$_"} reverse <docker-compose*.yml>'` $@
}

docker-port() {
    local name=$1; shift
    local ip=$(perl -E "\$_=qq#$DOCKER_HOST#; m#//(.*):#; say \$1")
    if [ $# -eq 1 ]; then
        local index=$(perl -E "\$_=qq/$1/; s/p\$//; say")
        if perl -E "'$1'=~/p\$/ or exit 1"; then
            docker port $name | perl -nlE "@a=split ':'; say qq#\$a[-1]#" | perl -E "chomp(@a=<stdin>); say @a[$index]"
        else
            docker port $name | perl -nlE "@a=split ':'; say qq#$ip:\$a[-1]#" | perl -E "chomp(@a=<stdin>); say @a[$index]"
        fi
    else
        docker port $name | perl -nlE "@a=split ':'; say qq#$ip:\$a[-1]#"
    fi
}

docker-mysqldump() {
    if [ $# -eq 1 ]; then
        docker exec -it $1 mysql
        return
    fi

    local force=false
    while getopts f OPT; do
        case $OPT in
            f) force=true;;
        esac
    done
    shift $((OPTIND - 1))

    local name=$1; shift
    local v=$1; shift
    local dir=~/.docker-mysqldump/$name
    [ ! -d "$dir" ] && mkdir -p $dir

    # -N skip header, -B remove spaces
    # for _db in $(echo "$(docker run --rm -it "mysql:$v" mysql -NBe 'show databases')"); do
        # local db=`perl -E "\\$ARGV[0] =~ /(\w+)/ and say \\$1" $_db`
        # if [ -z "$db" ]; then
        #     echo "ERROR : db name is wrong"
        #     continue
        # elif [ $db = "information_schema" ]; then
        #     continue
        # fi
    for db in $@; do
        local filepath="$dir/$db.sql"
        if $force; then
            rm $filepath
        fi
        if [ ! -f "$filepath" ]; then
            if ! docker run --rm -it "mysql:$v" sh -c 'test -f ~/.my.cnf'; then
                echo "ERROR: ~/.my.cnf does not exist on $name"
                return
            fi
            echo "mysqldump > $filepath"
            docker run --rm -it "mysql:$v" mysqldump "$db" > $filepath
        fi
        if ! docker exec $name mysql -e "use $db"; then
            echo "CREATE: $db on $name"
            docker exec $name mysql -e "create database $db;"
        fi
        if ! docker exec -i $name mysql $db < $filepath; then
            echo "ERROR: you can not store $db < $filepath on $name"
            return
        fi
        echo "DONE: $name mysql $db < $filepath"
    done
}

# secure ssh dump command

alias cmd="docker exec -it --detach-keys ctrl-q,q cmd python main.py"
alias dev="docker exec -it --detach-keys ctrl-q,q dev python curl.py"
alias algo-erl='docker exec -it --detach-keys ctrl-q,q algo-erl sh run.sh erl'
alias algo-py="docker exec -it --detach-keys ctrl-q,q algo-py python"
alias algo-gosh="docker exec -it --detach-keys ctrl-q,q algo-gosh gosh"
alias algo-js="docker exec -it --detach-keys ctrl-q,q algo-js node"
alias algo-go="docker exec -it --detach-keys ctrl-q,q algo-go run"

PATH_DOCKER_ONELINER="/Users/mbp/workspace/sandbox/dict/data/oneliner"

docker_oneliner () {
    if [ $# -eq 0 ]; then
       echo "Need container_name"
       return 1
    fi
    name=$1; shift
    container_name="oneliner-$name"
    \cd $PATH_DOCKER_ONELINER
    if ! docker ps --format "{{.Names}}" | grep $container_name > /dev/null ; then
        docker-compose up -d $container_name
    fi
    docker exec -it --detach-keys ctrl-q,q $container_name bash ./bin/$name $@
}

alias one=docker_oneliner
docker-kill() { docker rm -f `docker ps -aq` }

# なんかdocker-composeで環境に入りたい
# DOCKER_LANGUAGE_PATH="/Users/mbp/workspace/sandbox/algo/"
# docker-compose-language() {
#     for p in 
#         docker-compose up -d $lang
#     done
# }
