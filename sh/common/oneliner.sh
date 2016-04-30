
# Show all variables in mysql. Trim each line
# mysql db -e 'show variables;' | perl_mysql_parse_variables
perl_mysql_parse_variables() {
    perl -nlE '@a=(split " ", $_); $k=$a[1]; $v=$a[3]||1; $v=="|" && ($v=1); say "$k=$v" if $k && 4 .. ((eof) - 1)'
}
