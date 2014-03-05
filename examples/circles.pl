#!/usr/bin/perl

use strict;
use warnings;

use Math::Geometry::RandomPlaneFiller;
use Math::Vector::Real::Random;
use GD::Image;

system "rm out-*.png 2>/dev/null";

my $filler = Math::Geometry::RandomPlaneFiller->new;

my @circles;

#for () {
    # [0.406766682005586,0.310050818768479,0.5523052052343],
    # [0.159998876771159,0.87957465497956,0.186037274921857]
#    my ($r, @o) = @$_;
#    my $circle = Math::Geometry::RandomPlaneFiller::Shape::Circle->new(\@o, $r);
#    $filler->insert($circle);
#    $im->filledEllipse((map { $_ * 1000 } @o, $r, $r), $blue);
#}

for (0..10000) {
    my $o = $filler->random_free_point or last;
    my $r = rand 0.2;
    for (1..8) {
        my $circle = Math::Geometry::RandomPlaneFiller::Shape::Circle->new($o, $r);
        if ($filler->find_touching($circle)) {
            $r *= 0.5;
        }
        else {
            print STDERR "r: $r, o: $o, i: $_\n";
            push @circles, [$o, $r];
            $filler->insert($circle);
            last;
        }
    }
    $_ % 10 or draw($_, $o, $r);
}

sub draw {
    my ($n, $o, $r) = @_;
    my $im = GD::Image->new(1000, 1000);
    my $white = $im->colorAllocate(255,255,255);
    my $red = $im->colorAllocate(255, 0, 0);
    my $blue = $im->colorAllocate(0, 0, 255);
    my @gray = map $im->colorAllocate(($_/100 * 255) x 3), 0..100;

    my $draw_region = sub {
        my ($p0, $p1, $prob) = @_;
        $im->filledRectangle((map { 1000 * $_ } @$p0, @$p1), $gray[int($prob * 100)]);
        $im->rectangle((map { 1000 * $_ } @$p0, @$p1, $white));
        $im->string(GD::gdSmallFont, (map 1000*$_, @$p0), sprintf("%d",$prob * 100), $blue);
    };

    $filler->draw_regions($draw_region);

    for my $circle (@circles) {
        my ($o, $r) = @$circle;
        $im->filledEllipse((map { $_ * 1000 } @$o, 2*$r, 2*$r), $red);
    }

    if (defined $o) {
        $im->ellipse((map { $_ * 1000 } @$o, 2*$r, 2*$r), $blue);
        $im->filledEllipse((map { $_ * 1000 } @$o), 5, 5, $blue);
    }

    open my $fh, ">out-$n.png";
    print $fh $im->png;
    close $fh;
}


