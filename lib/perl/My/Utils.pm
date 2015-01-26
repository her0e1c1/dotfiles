# This file contains utility functions which
# makes it typeless for you to execute oneliners


# Alias substr
sub ss {
    my($a, $b, $c) = @_;
    if (defined $c){
        substr($a, $b, $c);
    } elsif (defined $b){
        substr($a, $b);
    } else {
        $a;
    }
};

1
