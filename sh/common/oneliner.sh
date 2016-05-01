# 汎用性の高いコマンド(exisits)
# FILEPATHをとるもの
# STRING/REGEX
# DB COMMAND
# NETA

# chomp(@a=<stdin>)

# NETA
# 
# fizz buzz
# 単項プラス演算子
# perl -E 'say +(Fizz)[$_%3].(Buzz)[$_%5]||$_ for 1..100'"
# Hello world in C
# echo '#include <stdio.h>\nint main(){printf(\"hello world\");} ' |clang -x c - -o a.out && ./a.out")

perl_to_upper() {perl -plE "tr/a-z/A-Z/"}
perl_to_lower() {perl -plE "tr/A-Z/a-z/"}
perl_get_image_tags() {curl $1 2> /dev/null | perl -nlE 'say $& if /<img.*?(gif|png|jpg).*?>/'}
perl_color() {perl -E 'print qq/\x1b[38;5;${_}mC$_ / for 0..255; say'}

perl_duplicated_words() {perl -0777 -ne 'print qq/$.: $_/ while /\b(\w+)\b\s+\b\1\b/gi'}

perl_filter_line() {
    # のファイルについて、.jsを含む行をファイル名と合わせて表示. なお、$ARGVでファイル名表示"
    local s='say qq/$ARGV $_/'
    s="$s if /$1/"
    xargs perl -nlE "'$s'"
}

# Show all variables in mysql. Trim each line
# mysql db -e 'show variables;' | perl_mysql_parse_variables
perl_mysql_parse_variables() {
    # Get key = value lines, trime each line and show key = val
    perl -lE '@a=<stdin>; say @a[4..$#a-1]'|
    perl -nlE 'say split " "' | 
    perl -nlE '@a=split "\\|"; say "$a[1] = $a[2]"'
}

perl_parse_single_quote() {
    # 'STRING' のようなクオートの文字列を抜き出す
    # echo "this is a 'HOGE' hoge 'FOO' " |perl -nlE "while(m/'(.*?)'/g){say $+;}"
    # gをつけることで$_の開始位置が巡る
    perl -nlE "while(m/'(.*?)'/g){say $+;}"
}

# FILEPATH
perl_insert_first_line() { xargs perl -i -plE "say qq/$1/ if 1..1; \$.=0 if eof" }

# sync selected directories in the two directories
# ; perl -E 'for ("css", "fonts", "js"){ system "rsync -avz --delete src/html/$_ dest/public/" }'

db_filter_insert_into() { perl -nlE 'say if /^INSERT INTO/' }
db_psql_dump_url() {docker run --rm -it postgres:9.5 pg_dump --data-only --column-inserts -d $1}
db_psql_dump_to_docker_mysql() {
    # このコマンドは、やらないほうがいい。一応メモとして残しておくけど, (一度一時ファイルに保存する)
    # cmd URL CONTAINER mysql DBNAME
    local url=$1; shift
    db_psql_dump_url $url | db_filter_insert_into | docker exec -i $@
}
