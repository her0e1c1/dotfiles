perl-line-drop () {
    [ $# -eq 1 ] && local l=$1 || local l=10
    perl -nlE "say if not 1..$l"
}

perl-pkill() {
    if [ $# -eq 0 ]; then ps aux;
    elif [ $# -eq 1 ]; then ps aux | perl -nlE "@s=split; qx/kill -9 \$s[1]/ if \$s[10] =~ m#$1#";
    elif [ $# -eq 2 -a $2 = "n" ]; then ps aux | perl -nlE "@s=split; say qq/kill -9 \$s[1] \$s[10]/ if \$s[10] =~ m#$1#";
    fi
}

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


# 創作コマンド
# watchmedo shell-command -R src -c 'f() { docker exec -it ss_ejabberd_1 sh -c '\''ps aux |perl -nlE "@s=split; qx/kill \$s[1]/ if \$s[10] =~ m#^/usr/lib/erlang/erts-7.3/bin/beam#" && rsync -avz ./sourcesage-ejabberd/ /docker/sourcesage-ejabberd/ && cd /docker/sourcesage-ejabberd/ && make && EJABBERD_CONFIG_PATH=docker/ejabberd.yml erl -pa ebin -pa deps/*/ebin -pa test -pa deps/elixir/lib/*/ebin/ -s ejabberd;'\''; }; f'
