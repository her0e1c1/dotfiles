
watch () {
    local number=1
    while getopts n: OPT; do
        case $OPT in
        n) number=$OPTARG;;
        esac
    done
    shift $((OPTIND - 1))

    if [ "$#" -lt 1 ]; then
        echo "No command"; return 1
    fi

    local cmd="$@"
    while true; do
        clear;
        sh -c "$cmd"
        sleep $number
    done;
}

# Check whether a command exists or not
exists(){ test -e "$(which $1)" }

repeat_commands () {
    if [ $# -ge 2 ]; then
        local n=$1; shift
        for i in `seq $n`; do $@ ;done;
    else
        echo "repeat needs more than one arguments"
    fi
}

debug (){
    # debug like set -x ; $CMD ; set +x
    set -x
    $@
    set +x
}

# Serve static files
# python -c 'import bottle as b; b.route("<p:path>")(lambda p: b.static_file(p, root=".")); b.run(host="0.0.0.0", port=8000)'
# use Jinja2
py_jinja2() { python -c 'import bottle as b; b.route("<p:path>")(lambda p: b.jinja2_template(p[1:], **dict(b.request.params.items()))); b.run(host="0.0.0.0", debug=True, port=8000)'; }

py_show_json() { python -mjson.tool }
f_tree() { pwd; find . | sort | sed '1d;s/^\.//;s/\/\([^/]*\)$/|--\1/;s/\/[^/|]*/|  /g'}
f_watch() {while true ; do clear ; df -h ; sleep 5 ; done}

## REGEX
f_basename() { echo ${1##*/} }       # 左からマッチしたものを除外(greedy)
f_ext() { echo ${1#*.} }             # 左からマッチしたものを除外(non greedy)
f_without_ext() { echo ${1%%.*} }    # 右からマッチしたものを除外(greedy)
f_dirname() { echo ${1%/*} }         # 右からマッチしたものを除外(non greedy)
