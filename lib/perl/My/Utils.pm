# This file contains utility functions which
# makes it typeless for you to execute oneliners

@EXPORT = qw(ss p);


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

sub p {
    print "@_\n";
};

1
