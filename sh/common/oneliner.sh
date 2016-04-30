# chomp(@a=<stdin>)

# Show all variables in mysql. Trim each line
# mysql db -e 'show variables;' | perl_mysql_parse_variables
perl_mysql_parse_variables() {
    # Get key = value lines, trime each line and show key = val
    perl -lE '@a=<stdin>; say @a[4..$#a-1]'|
    perl -nlE 'say split " "' | 
    perl -nlE '@a=split "\\|"; say "$a[1] = $a[2]"'
}
