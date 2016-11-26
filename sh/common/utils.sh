# Serve static files
# python -c 'import bottle as b; b.route("<p:path>")(lambda p: b.static_file(p, root=".")); b.run(host="0.0.0.0", port=8000)'
# use Jinja2
py_jinja2() { python -c 'import bottle as b; b.route("<p:path>")(lambda p: b.jinja2_template(p[1:], **dict(b.request.params.items()))); b.run(host="0.0.0.0", debug=True, port=8000)'; }

py_show_json() { python -mjson.tool }
f_tree() { pwd; find . | sort | sed '1d;s/^\.//;s/\/\([^/]*\)$/|--\1/;s/\/[^/|]*/|  /g'}

## OVER LOAD
check-cpu () {
    local num=1
    [ $# -eq 1 ] && num=$1
    for i in `seq $num`; do yes >> /dev/null & ; done
}
# yes | xargs -L 1 -P 8 >> /dev/null
# perl -e 'while(1) {}'
# perl -e '$hn=`hostname`; while(1){$i++;$h{$i}=$i . " $hn" . 'x'x4000 }'
# perl -e 'while(1){$i++;$h{$i}=$i}' 
