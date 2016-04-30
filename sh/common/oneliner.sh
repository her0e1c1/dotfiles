# chomp(@a=<stdin>)

perl_to_upper() {perl -plE "tr/a-z/A-Z/"}
perl_to_lower() {perl -plE "tr/A-Z/a-z/"}
perl_get_image_tags() {curl $1 2> /dev/null | perl -nlE 'say $& if /<img.*?(gif|png|jpg).*?>/'}

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
