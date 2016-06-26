

# function
fg255(){
    perl -E 'print qq/\x1b[38;5;${_}mC$_ / for 0..255; say'
}

bg255(){
    perl -E 'print qq/\x1b[38;5;${_}mC$_ / for 0..255; say'
}

PERL_MODULES=$(cat <<EOF
'File::Spec::Functions qw(:ALL)'
'Cwd qw(chdir abs_path cwd fastcwd fast_abs_path realpath)'
'File::Basename'
'List::Util qw(first max maxstr min minstr reduce shuffle sum)'
'MIME::Base64'
'Digest::MD5  qw(md5 md5_hex md5_base64)'
'File::Copy qw(cp mv)'
'POSIX'
EOF
)
#'"My::Utils qw(:ALL)"'

PERL_OPTION=`perl -e 'print sprintf " %s ", join " ", map {"-M$_"} split /\n/, $ARGV[0] ' "$PERL_MODULES"`
# PERL_OPTION="$PERL_OPTION -I $LIB_PERL"


perl_pkill() { ps aux |perl -nlE "@s=split; qx/kill \$s[1]/ if \$s[10] =~ m#$1#" }

   
