package Math::Geometry::RandomPlaneFiller;

our $VERSION = '0.01';

use 5.010;
use strict;
use warnings;
use Scalar::Util qw();
use Math::Vector::Real;
use Math::Vector::Real::Random;

sub new {
    my ($class, %opts) = @_;
    my $o = delete $opts{o} // [0, 0];
    $o = V(@$o); # promote to Math::Vector::Real
    my $d = delete $opts{d} // [1, 1];
    my ($o0, $o1) = Math::Vector::Real->box($o, $o + $d);
    my $self = { root => Math::Geometry::RandomPlaneFiller::Region->new($o0, $o1),
                 max_dist => ($o1 - $o0)->norm,
               };
    bless $self, $class;
}

sub random_free_point {
    my $self = shift;
    my $root = $self->{root};
    return if $root->is_full;
    $root->random_free_point;
}

sub find_touching_shape {
    my ($self, $shape) = @_;
    my $t = $self->{root}->find_touching($shape);
}

sub insert {
    my ($self, $shape) = @_;
    $self->{root}->insert($shape);
}

sub draw_regions {
    my ($self, $cb) = @_;
    $self->{root}->draw_regions($cb);
}

sub find_nearest_to_point {
    my ($self, $p, $n, $max_dist) = @_;
    $max_dist //= $self->{max_dist};
    $self->{root}->find_nearest_to_point($p, $n || 1, $max_dist * $max_dist);
}

sub find_nearest_custom {
    my ($self, $finder, $n) = @_;
    $self->{root}->find_nearest_custom($finder, $n);
}

sub find_touching_circle {
    my ($self, $p, $r) = @_;
    $self->{root}->find_nearest_to_point($p, undef, $r * $r);
}

package Math::Geometry::RandomPlaneFiller::Shape;

sub area_of_intersection_with_rectangle {
    my $class = ref(shift);
    die "virtual method area_of_intersection_with_rectangle not implemented by class '$class'";
}

sub is_touching_point {
    my $class = ref(shift);
    die "virtual method is_touching_point not implemented by class '$class'";
}

sub is_touching_shape {
    my $class = ref(shift);
    die "virtual method is_touching_shape not implemented by class '$class'";
}

sub is_touching_rectangle {
    my $class = ref(shift);
    die "virtual method is_touching_rectangle not implemented by class '$class'";
}

sub distante_to_shape {
    my $class = ref(shift);
    die "virtual method distance_to_shape not implemented by class '$class'";
}

sub distance_to_rectangle {
    my $class = ref(shift);
    die "virtual method distance_to_rectangle not implemented by class '$class'";
}

sub distance_to_point {
    my $class = ref(shift);
    die "virtual method distance_to_point not implemented by class '$class'";
}

package Math::Geometry::RandomPlaneFiller::Shape::Circle;
our @ISA = qw(Math::Geometry::RandomPlaneFiller::Shape);

use Math::Geometry::IntersectionArea;

use constant o     => 0;
use constant r     => 1;
use constant slots => 2;

sub center { shift->[o] }
sub radius { shift->[r] }

sub new {
    my ($class, $o, $r) = @_;
    my $self = [];
    @{$self}[o, r] = (Math::Vector::Real::V(@$o), $r);
    bless $self, $class;
}

sub area_of_intersection_with_rectangle {
    my $self = shift;
    Math::Geometry::IntersectionArea::intersection_area_circle_rectangle($self->[o], $self->[r], @_);
}

sub is_touching_shape {
    my ($self, $other) = @_;
    my $total = $self->[r] + $other->[r];
    $total * $total >= ($self->[o] - $other->[o])->norm2;
}

sub is_touching_point {
    my ($self, $p) = @_;
    my $r = $self->[r];
    $self->[o]->dist2($p) < $r * $r;
}

sub distance_to_point {
    my ($self, $p) = @_;
    my $d = $self->[o]->dist($p) - $self->[r];
    $d < 0 ? 0 : $d;
}

sub distance_to_rectangle {
    my $self = shift;
    my $o = $self->[o];
    my $d = $o->nearest_in_box(@_)->dist($o) - $self->[r];
    $d < 0 ? 0 : $d;
}

sub is_touching_rectangle {
    my $self = shift;
    my $n = $self->[o]->nearest_in_box(@_);
    my $r = $self->[r];
    $n->dist2($self->[o]) < $r * $r;
}

package Math::Geometry::RandomPlaneFiller::Region;

use constant max_shapes_per_region => 5;

use constant o0    => 0; # corner 0
use constant o1    => 1; # corner 1
use constant iarea => 3; # inverse of total area
use constant free  => 4; # free area
use constant objs  => 5; # objects
use constant sel   => 6; # selector
use constant sr0   => 7; # subregion 0
use constant sr1   => 8; # subregion 1
use constant slots => 9; #

sub is_full { shift->[free] <= 0 }

sub new {
    my ($class, $o0, $o1) = @_;
    my $d = $o1 - $o0;
    my $area = 1;
    $area *= $_ for @$d;
    my $iarea = 1 / ($area || 1);
    my $self = [];
    @{$self}[o0, o1, iarea, free, objs] = ($o0, $o1, $iarea ,$area, []);
    bless $self, $class;
    $self;
}

sub draw_regions {
    my ($self, $cb) = @_;
    if ($self->[objs]) {
        $cb->($self->[o0], $self->[o1], $self->[free] * $self->[iarea]);
    }
    else {
        $self->[sr0]->draw_regions($cb);
        $self->[sr1]->draw_regions($cb);
    }
}

sub random_free_point {
    my $self = shift;
    if (my $objs = $self->[objs]) {
        if ($self->[free] * $self->[iarea] > 0.5) {
            my $o0 = $self->[o0];
            my $o1 = $self->[o1];
        OUT: while (1) {
                my $p = $o0 + ($o1 - $o0)->random_in_box;
                for (@$objs) {
                    next OUT if $_->is_touching_point($p)
                }
                return $p;
            }
        }
        $self->_divide;
    }

    my $sr0 = $self->[sr0];
    my $sr = ( (rand($self->[free]) < $sr0->[free])
               ? $sr0
               : $self->[sr1] );
    $sr->random_free_point;
}

sub insert {
    my $self = shift;
    my $shape = shift;
    my $o0 = $self->[o0];
    my $o1 = $self->[o1];
    if ($shape->is_touching_rectangle($o0, $o1)) {
        if (my $objs = $self->[objs]) {
            if (@$objs < max_shapes_per_region) {
                push @$objs, $shape;
                $self->[free] -= $shape->area_of_intersection_with_rectangle($o0, $o1);
                return;
            }
            $self->_divide;
        }
        $self->_insert_into_subtrees($shape);
    }
}

sub _divide {
    my $self = shift;
    my $objs = delete $self->[objs] or return;
    my $o0 = $self->[o0];
    my $o1 = $self->[o1];
    my $diagonal = $o1 - $o0;
    my $selector = $self->[sel] = ($diagonal->[0] > $diagonal->[1] ? 0 : 1);
    my $middle = 0.5 * ($o0->[$selector] + $o1->[$selector]);
    my $p0 = Math::Vector::Real::V(@$o1);
    my $p1 = Math::Vector::Real::V(@$o0);
    $p0->[$selector] = $p1->[$selector] = $middle;
    my $sr0 = $self->[sr0] = Math::Geometry::RandomPlaneFiller::Region->new($o0, $p0);
    my $sr1 = $self->[sr1] = Math::Geometry::RandomPlaneFiller::Region->new($p1, $o1);
    $self->_insert_into_subtrees(@$objs);
}

sub _insert_into_subtrees {
    my $self = shift;
    my $sr0 = $self->[sr0];
    my $sr1 = $self->[sr1];
    for (@_) {
        $sr0->insert($_);
        $sr1->insert($_);
    }
    $self->[free] = $sr0->[free] + $sr1->[free];
}

sub _merge_unique_shape {
    my ($u, $v) = @_;
    @$u or return $v;
    @$v or return $u;

    my @r;
    while (@$u) {
        if (@$v) {
            my $dir = (Scalar::Util::refaddr($u->[0]) <=> Scalar::Util::refaddr($v->[0]));
            if ($dir < 0) {
                push @r, shift @$u;
            }
            else {
                push @r, shift @$v;
                $dir or shift @$u; # remove duplicates
            }
        }
        else {
            push @r, @$u;
            return \@r;
        }
    }
    push @r, @$v;
    return \@r;
}

sub find_touching {
    my ($self, $shape) = @_;
    my @queue; # eliminate recursion
    my @touching;
    my %shape_seen;
    while (1) {
        if ($shape->is_touching_rectangle($self->[o0], $self->[o1])) {
            if (my $objs = $self->[objs]) {
                for my $shape2 (@$objs) {
                    next if $shape_seen{$shape2}++;
                    next unless $shape->is_touching_shape($shape2);
                    return 1 unless wantarray;
                    push @touching, $shape2;
                }
                $self = pop @queue or last;
            }
            else {
                push @queue, $self->[sr1];
                $self = $self->[sr0];
            }
        }
    }
    return @touching;
}

sub find_nearest_to_point {
    my ($self, $p, $n, $max_dist2) = @_;
    my (@queue, @queue_d2);
    my @top;
    my %shape_d2;
    while (1) {
        if (my $objs = $self->[objs]) {
            for my $shape (@$objs) {
                next if exists $shape_d2{$shape}; # remove duplicates
                my $d = $shape->distance_to_point($p);
                my $d2 = $shape_d2{$shape} = $d * $d;

                my @d2 = map $shape_d2{$_}, @top;
                # print STDERR "top_d2: @d2 <-- $d2\n";

                next unless $d2 < $max_dist2;
                my $ix = @top;
                while ($ix and $d2 < $shape_d2{$top[$ix - 1]}) { --$ix }
                splice @top, $ix, 0, $shape;
                if (defined $n and @top >= $n) {
                    pop @top if @top > $n;
                    $max_dist2 = $shape_d2{$top[-1]};
                }
            }
        }
        else {
            for my $sr (@{$self}[sr0, sr1]) {
                my $d2 = $p->nearest_in_box($sr->[o0], $sr->[o1])->dist2($p);
                if ($d2 <  $max_dist2) {
                    my $ix = @queue;
                    while ($ix and $d2 > $queue_d2[$ix - 1]) { --$ix }
                    splice @queue, $ix, 0, $sr;
                    splice @queue_d2, $ix, 0, $d2;
                    # print STDERR "queue_d2: @queue_d2\n";
                }
            }
        }

        $self = pop @queue or last;
        pop(@queue_d2) < $max_dist2 or last;
    }
    return @top;
}

sub _find_nearest_custom {
    my ($self, $finder, $n, $max_d) = @_;
    my @top;
    my $shape_d;
    my @queue;
    my @queue_d;
    while (1) {
	if (my $objs = $self->[objs]) {
	    for my $shape (@$objs) {
		next if exists $shape_d{$shape};
		my $d = $shape_d{$shape} = $finder->distance_to_shape($shape);
		next if !defined $d or (defined $max_d and $d > $max_d);
		my $ix = @top;
                while ($ix and $d < $shape_d{$top[$ix - 1]}) { --$ix }
                splice @top, $ix, 0, $shape;
                if (defined $n and @top >= $n) {
                    pop @top if @top > $n;
                    $max_dist = $shape_d{$top[-1]};
                }
	    }
	}
    	else {
	    for my $r (@{$self}[sr0, sr1]) {
		my $d = $finder->distance_to_box($sr->[o0], $sr->[o1]);
		next if !defined $d or (defined $max_d and $d > $max_d);
		my $ix = @queue;
		while ($ix and $d > $queue_d[$ix - 1]) { --$ix }
		splice @queue, $ix, 0, $sr;
		splice @queue_d2, $ix, 0, $d2;
	    }
	}
	$self = pop @queue or last;
	pop(@queue) < $max_dist or last;
    }
    return @top;
}

package Math::Geometry::RandomPlaneFiller::Finder;

sub distance_to_shape {
    my ($self, $shape) = @_;
    die "virtual method 'distance_to_shape' not implemented by class '$class'";
}

sub distance_to_box {
    my ($self, $shape) = @_;
    die "virtual method 'distance_to_box' not implemented by class '$class'";
}

package Math::Geometry::RandomPlaneFiller::RadiusOfTangentCircunferenceFinder;

# given a circunference C defined by its center $o and a point $p,
# this class defined the distance to a point $q as the radius of the
# circunference tangent to C that contains both the points $p and $q

sub new {
    my ($class, $o, $p) = @_;

    my $v = ($p - $o)->versor;
    my $n = Math::Vector::Real::V($v[1], -$v[0]);
    my $self = [$p, $v, $n] = @_;
    bless $self, $class;
}

sub distance_to_shape {
    my ($self, $shape) = @_;

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
