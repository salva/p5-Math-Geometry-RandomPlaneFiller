package Math::Geometry::RandomPlaneFiller;

our $VERSION = '0.01';

use 5.010;
use strict;
use warnings;



sub new {
    my ($class, %opts) = @_;
    my $o = delete $opts{o} // [0, 0];
    my $d = delete $opts{d} // [1, 1];
    my ($o0, $o1) = Math::Vector::Real->box($o, $o + $d);
    my $self = { root => Math::Geometry::RandomPlaneFiller::Region->new($o0, $o1) };
    bless $self, $class;
}

package Math::Geometry::RandomPlaneFiller::Shape;

sub area_of_intersection_with_rectangle {
    my $class = ref(shift);
    die "virtual method area_of_intersection_with_rectangle not implemented by class '$class'";
}

sub is_touching {
    my $class = ref(shift);
    die "virtual method is_touching not implemented by class '$class'";
}

sub distante_to_shape {
    my $class = ref(shift);
    die "virtual method distance_to_shape not implemented by class '$class'";
}

sub distance_to_rectangle {
    my $class = ref(shift);
    die "virtual method distance_to_rectangle not implemented by class '$class'";
}

package Math::Geometry::RandomPlaneFiller::Shape::Circle;
our @ISA = qw(Math::Geometry::RandomPlaneFiller::Shape);

use constant o     => 0;
use constant r     => 1;
use constant slots => 2;

sub new {
    my ($class, $o, $r) = @_;
    my $self = [];
    @{$self}[o, r] = ($o, $r);
    bless $self, $class;
}

sub area_of_intersection_with_rectangle {
    my $self = shift;
    Math::Geometry::IntersectionArea::intersection_area_circle_rectangle($self->[o], $self->[r], @_);
}

sub is_touching {
    my ($self, $other) = @_;
    my $total = $self->[r] + $other->[r];
    $total * $total >= ($self->[o] - $other->[o])->norm2;
}

sub distance_to_rectangle {
    my $self = shift;
    my $n = V(@{$self->[o]});
    if ($n->[0] < $o0->[0]) {
        $n->[0] = $o0->[0];
    }
    elsif ($n->[0] > $o1->[0]) {
        $n->[0] = $o1->[0]
    }
    if ($n->[1] < $o0->[1]) {
        $n->[1] = $o0->[1];
    }
    elsif ($n->[1] > $o1->[1]) {
        $n->[1] = $o1->[1];
    }
    my $d = $n->dist($self->[o]) - $self->[r];
    return ($d < 0 ? 0 : $d);
}

package Math::Geometry::RandomPlaneFiller::Region;

use constant max_shapes_per_region => 10;

use constant o0    => 0; # corner 0
use constant o1    => 1; # corner 1
use constant free  => 2; # free area
use constant objs  => 3; # objects
use constant sel   => 4; # selector
use constant sr0   => 5; # subregion 0
use constant sr1   => 6; # subregion 1
use constant slots => 7; #

sub new {
    my ($class, $o0, $o1) = @_;
    my $o0 = shift;
    my $o1 = shift;
    my $d = $o1 - $o0;
    my $area = 1;
    $area *= $_ for @$d;
    my $self = [];
    @{$self}[o0, o1, free, objs] = ($o0, $o1, $area, []);
    bless $self, $class;
    $self;
}

sub _divide {
    my $self = shift;
    my $objs = delete $self->[objs] or return;
    my $o0 = $self->[o0];
    my $o1 = $self->[o1];
    my $diag = $o1 - $o0;
    my $sel = ($d->[0] > $d->[1] ? 0 : 1);
    my $middle = 0.5 * ($o0->[$sel] + $o1->[$sel]);
    my $p0 = V(@$o1);
    my $p1 = V(@$o0);
    $p0->[$sel] = $p1->[$sel] = $middle;
    my $sr0 = Math::Geometry::RandomPlaneFiller::Region->new($o0, $p0);
    my $sr1 = Math::Geometry::RandomPlaneFiller::Region->new($p1, $o1);
    $self->_insert($_) for @$objs;
}

sub _insert {
    my $self = shift;
    my $shape = shift;
    if (my $area = $shape->area_of_intersection_with_rectangle($self->[o0], $self->[o1])) {
        if (my $objs = $self->[objs]) {
            if (@$objs < max_shapes_per_region) {
                push @$objs, $shape;
                $self->[free] -= $area;
            }
            $self->_divide;
        }
        my $sr0 = $self->[sr0];
        my $sr1 = $self->[sr1];
        $sr0->insert($shape);
        $sr1->insert($shape);
        $self->[area] = $sr0->[area] + $sr1->[area];
    }
}



1;
__END__


=head1 NAME

Math::Geometry::RandomPlaneFiller - Perl extension for blah blah blah

=head1 SYNOPSIS

  use Math::Geometry::RandomPlaneFiller;
  blah blah blah

=head1 DESCRIPTION

Stub documentation for Math::Geometry::RandomPlaneFiller, created by h2xs. It looks like the
author of the extension was negligent enough to leave the stub
unedited.

Blah blah blah.

=head2 EXPORT

None by default.



=head1 SEE ALSO

Mention other useful documentation such as the documentation of
related modules or operating system documentation (such as man pages
in UNIX), or any relevant external documentation such as RFCs or
standards.

If you have a mailing list set up for your module, mention it here.

If you have a web site set up for your module, mention it here.

=head1 AUTHOR

Salvador Fandiño, E<lt>salva@E<gt>

=head1 COPYRIGHT AND LICENSE

Copyright (C) 2014 by Salvador Fandiño

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself, either Perl version 5.18.2 or,
at your option, any later version of Perl 5 you may have available.


=cut
