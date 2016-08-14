
alias urlencode='python -c "import sys, urllib as ul; print(ul.quote_plus(sys.argv[1]))"'
alias urldecode='python -c "import sys, urllib as ul; print(ul.unquote_plus(sys.argv[1]))"'
alias timestamp='python -c "import sys, datetime as d; print(d.datetime.fromtimestamp(float(sys.argv[1])))"'

if [ -e `which watchmedo` ]; then
    watch_dir () {
        local dir
        [ $# -eq 0 ] && dir="." || dir=$1; shift;
        watchmedo shell-command -R $dir -c 'echo ${watch_src_path} -- ${watch_event_type} -- ${watch_object} -- ${watch_dest_path}'
    }
    alias watch_cmd='watchmedo shell-command'
fi

# python -c "import multiprocessing as mp; import itertools as it; [mp.Process(target=lambda: [i for i in it.count() if not i]).start() for i in range(mp.cpu_count())]"
