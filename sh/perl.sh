### Perl for one liner
PERL_MODULES=(
    '"File::Spec::Functions qw(:ALL)"'
    '"Cwd qw(chdir abs_path cwd fastcwd fast_abs_path realpath)"'
    '"File::Basename"'
    '"List::Util qw(first max maxstr min minstr reduce shuffle sum)"'
    '"MIME::Base64"'
    '"Digest::MD5  qw(md5 md5_hex md5_base64)"'
    '"File::Copy qw(cp mv)"'
    '"POSIX"'
    '"My::Utils qw(:ALL)"'
)
PERL_OPTION=`perl -e 'print sprintf " %s ", join " ", map {"-M$_"} @ARGV' $PERL_MODULES`
PERL_OPTION="$PERL_OPTION -I $LIB_PERL"

alias prm='perl -E ''
use File::Basename;
use File::Spec;
$d="$ENV{HOME}/.trash";
mkdir $d unless -d $d; $c=1;
for(@ARGV){while(1){
  unless(-e){say "no $_ exists"; last};
  $base = basename $_;
  $base .= "_$c" if($c !=1);
  $o = File::Spec->catfile($d, $base);
  $cmd = "mv $_ $o";
  unless(-e $o){say $cmd; system $cmd; last};$c++;
}}
'''
