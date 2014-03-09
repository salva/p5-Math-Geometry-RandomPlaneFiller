#!/usr/bin/perl

use strict;
use warnings;

use Math::Geometry::RandomPlaneFiller;
use Math::Geometry::Conic qw(point_equidistant_to_three_circunferences);

use GD::Image;

my $max_r = 1;

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

my $last_log = -1;
for (1..1000000) {
    my $o = $filler->random_free_point or last;
    my $o0 = $o;
    if (1) {
        my $r = rand 0.2;
        for (1..100) {
            if ($filler->find_touching_circle($o, $r)) {
                $r *= 0.9;
            }
            else {
                # print STDERR "r: $r, o: $o, i: $_\n";
                push @circles, [$o, $r];
                $filler->insert(Math::Geometry::RandomPlaneFiller::Shape::Circle->new($o, $r));
                last;
            }
        }

        my $log = int(100 * log $_);
        if ($log != $last_log) {
            print STDERR "drawing n: $_\n";
            draw($_, $o, $o0, $r, []);
            $last_log = $log;
        }

    }
    else {
        my $r;
        my @nearest = $filler->find_nearest_to_point($o, 5);
        my ($first, $second, @more) = @nearest;
        for my $third (@more) {
            if (my ($o1, $r1) = point_equidistant_to_three_circunferences(map { $_->center, $_->radius }
                                                                          $first, $second, $third)) {
                if ($r1 < $max_r) {
                    print STDERR "tangent to 3 found\n";
                    ($o, $r) = ($o1, $r1);
                }
                last;
            }
            else {
                print STDERR "point_equidistant_to_three_circunferences returned undef\n";
            }
        }
        unless (defined $r) {
            if (defined $first) {
                my $r1 = $o->dist($first->center) - $first->radius;
                if ($r1 < $max_r) {
                    $r = $r1
                }
            }
            $r //= rand($max_r);
        }

        print STDERR "r: $r, o: $o, o0: $o0\n";
        push @circles, [$o, $r];
        $filler->insert(Math::Geometry::RandomPlaneFiller::Shape::Circle->new($o, $r));

        my $log = int(10 * log $_);
        if ($log != $last_log) {
            print STDERR "drawing n: $_\n";
            draw($_, $o, $o0, $r, [grep defined, @nearest[0..2]]);
            $last_log = $log;
        }
    }
}

sub draw {
    my ($n, $o, $o0, $r, $tangents) = @_;
    my $im = GD::Image->new(1000, 1000);
    my $white = $im->colorAllocate(255,255,255);
    my $red = $im->colorAllocate(255, 0, 0);
    my $blue = $im->colorAllocate(0, 0, 255);
    my $green = $im->colorAllocate(0, 255, 0);
    my @gray = map $im->colorAllocate(($_/100 * 255) x 3), 0..100;

    my $draw_region = sub {
        my ($p0, $p1, $prob) = @_;
        my $gix = int($prob * 100);
        $im->filledRectangle((map { 1000 * $_ } @$p0, @$p1), ($gix < 0 ? $red : $gray[int $gix]));
        $im->rectangle((map { 1000 * $_ } @$p0, @$p1, $white));
        # $im->string(GD::gdSmallFont, (map 1000*$_, @$p0), sprintf("%d",$prob * 100), $blue);
    };

    $filler->draw_regions($draw_region);

    for my $circle (@circles) {
        my ($o, $r) = @$circle;
        $im->filledEllipse((map { $_ * 1000 } @$o, 2*$r, 2*$r), $red);
    }

    for my $tangent (@$tangents) {
        my ($o, $r) = ($tangent->center, $tangent->radius);
        $im->filledEllipse((map { $_ * 1000 } @$o, 2*$r, 2*$r), $green);
    }


    if (defined $o) {
        # $im->ellipse((map { $_ * 1000 } @$o, 2*$r, 2*$r), $blue);
        # $im->filledEllipse((map { $_ * 1000 } @$o), 5, 5, $blue);
    }
    if (defined $o0) {
        # $im->filledEllipse((map { $_ * 1000 } @$o0), 5, 5, $blue);
    }

    open my $fh, ">", sprintf "out-%07d.png", $n;
    print $fh $im->png;
    close $fh;
}


