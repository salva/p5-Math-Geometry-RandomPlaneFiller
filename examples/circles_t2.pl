#!/usr/bin/perl

use strict;
use warnings;

use Math::Geometry::RandomPlaneFiller;
# use Math::Geometry::Conic qw(point_equidistant_to_three_circunferences);

use GD::Image;

my $width = 960;

my $max_r = 1;
my $sep_regions = (shift(@ARGV) ? 1 : 0);

system "rm out-*.png 2>/dev/null";

my $filler = Math::Geometry::RandomPlaneFiller->new;
my ($b0, $b1) = $filler->box;

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
    my $r = rand 0.2;
    my ($o0, $r0) = ($o, $r);
    my ($nearest, $second, $rater);
    my $p_rater;
    # print STDERR "random circle at $o, r=$r\n";
    my $b = $o0->nearest_in_box_border($b0, $b1);
    my $rb = $o0->dist2($b);
    $rb > 0 or next;
    my $v;
    if ($nearest = $filler->find_nearest_to_point($o, 1, $rb)) {
        $v = ($o - $nearest->center)->versor;
        $b = $nearest->center + $v * $nearest->radius;
        $rater = Math::Geometry::RandomPlaneFiller::DiameterOfTangentCircunferenceRater
            ->new_from_point_and_vector($b, $v);
    }
    elsif ($rb < $r) {
        $v = ($o - $b)->versor;
        $rater = Math::Geometry::RandomPlaneFiller::DiameterOfTangentCircunferenceRater
            ->new_from_point_and_vector($b, $o - $b);
    }
    if ($rater) {
        $r = 0.5 * $rater->rate_box_inside($b0, $b1, 2 * $r);
        if ($second = $filler->find_best_rated($rater, 1, 2 * $r)) {
            my $r1 = 0.5 * $rater->rate_shape($second);
            if ($r1 < 0.0001 * $r) {
                # print STDERR "r1 is too small: $r1\n";
                next;
            }
            $r = $r1;
        }
        $o = $b + $r * $v;
    }

    push @circles, [$o, $r];

    my $log = int(100 * log $_);
    if ($log != $last_log) {
        print STDERR "drawing n: $_\n";
        draw($_, $o, $o0, $r, [grep defined, $nearest, $second], $rater);
        $last_log = $log;
    }

    if (1) {
        my $rs = $r * .99999;
        my ($b, $rb) = $o->nearest_in_box_border($b0, $b1);
        if (defined $rb and $rb < $rs) {
            warn "too close to border => b: $b, rb: $rb, o: $o, r: $r\n";
        }
        if (my $n = $filler->find_nearest_to_point($o, 1, $rs)) {
            my $nr = $n->radius;
            my $nc = $n->center;
            warn "too close to circle => nc: $nc, nr: $nr, o: $o, r: $r\n";
        }
    }

    $filler->insert(Math::Geometry::RandomPlaneFiller::Shape::Circle->new($o, $r));

}

sub draw {
    my ($n, $o, $o0, $r, $tangents, $rater) = @_;
    my $im = GD::Image->new($width * ($sep_regions ? 2 : 1), $width);
    my $white = $im->colorAllocate(255,255,255);
    my $red = $im->colorAllocate(255, 0, 0);
    my $blue = $im->colorAllocate(0, 0, 255);
    my $green = $im->colorAllocate(0, 255, 0);
    my $orange = $im->colorAllocate(200, 100, 50);
    my $black = $im->colorAllocate(0, 0, 0);

    my @gray = map $im->colorAllocate(($_/100 * 255) x 3), 0..100;

    my $draw_region = sub {
        my ($p0, $p1, $prob) = @_;
        my $gix = int($prob * 100);
        $im->filledRectangle((map $width * $_, $p0->[0] + $sep_regions, $p0->[1], $p1->[0] + $sep_regions, $p1->[1]),
                             ($gix < 0 ? $red : $gray[int $gix]));

        # (map { $width * $_ } @$p0, @$p1), ($gix < 0 ? $red : $gray[int $gix]));
        #$im->rectangle((map { $width * $_ } @$p0, @$p1, $red));

        # $im->string(GD::gdSmallFont, (map $width*$_, @$p0), sprintf("%d",$prob * 100), $blue);
        #if ($rater) {
        #    my $rate = $rater->rate_box($p0, $p1);
        #    my $txt = (defined $rate ? int($rate * 100) : 'U');
        #    $im->string(GD::gdSmallFont,  (map $width*$_, @$p0), $txt, $blue);
        #}
    };

    $filler->draw_regions($draw_region);

    $im->filledRectangle(0, 0, $width, $width, $black) if $sep_regions;

    for my $circle (@circles) {
        my ($o, $r) = @$circle;
        # $im->filledEllipse((map { $_ * $width } @$o, 2*$r, 2*$r), $red);
        $im->ellipse((map { $_ * $width } @$o, 2*$r, 2*$r), $white);
    }

    for my $tangent (@$tangents) {
        my ($o, $r) = ($tangent->center, $tangent->radius);
        $im->ellipse((map { $_ * $width } @$o, 2*$r, 2*$r), $green);
    }

    if (defined $o) {
        $im->ellipse((map { $_ * $width } @$o, 2*$r, 2*$r), $blue);
        $im->filledEllipse((map { $_ * $width } @$o), 5, 5, $orange);
    }
    if (defined $o0) {
        $im->filledEllipse((map { $_ * $width } @$o0), 5, 5, $black);
    }

    open my $fh, ">", sprintf "out-%07d.png", $n;
    print $fh $im->png;
    close $fh;
}


