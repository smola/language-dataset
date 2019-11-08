use v6;

use Vector;
use Nurbs;
use Polynomial;
use SVG;
use SVGPad;

sub MakePath($curve, Range $range, SVGPad $pad)
{
    my @points = RangeOfSize($range.from, $range.to, 40).map({$pad.xy2mn($curve.evaluate($_))});
    my $start = @points.shift;
    my $path = "M {$start.coordinates[0]} {$start.coordinates[1]}";
    for @points -> $v 
    {
        $path ~= " L {$v.coordinates[0]} {$v.coordinates[1]}";
    }
    return $path;
}

my @control_points = (Vector.new(-1, -2),
                      Vector.new(1, 0),
                      Vector.new(1, 1),
                      Vector.new(0, 1),
                      Vector.new(1, 2),
                      Vector.new(1, 2),
                      Vector.new(1, 2));
my @knots = (-1, -1, -1, -1, 1, 2, 2, 3, 3, 3, 3);
my Nubs $nubs = Nubs.new(3, KnotVector.new(@knots), @control_points);
my Polynomial $poly1 = $nubs.evaluate(0, Polynomial.new(0.0, 1.0));
my Polynomial $poly2 = $nubs.evaluate(1.5, Polynomial.new(0.0, 1.0));
my Polynomial $poly3 = $nubs.evaluate(2.5, Polynomial.new(0.0, 1.0));

my $pad = SVGPad.new(Vector2.new(-2.5, -2.5), Vector2.new(2.5, 2.5), 
                     Vector2.new(0, 0), Vector2.new(700, 700));
my @dot_points = (-1, 1, 2, 3).map({$pad.xy2mn($nubs.evaluate($_))});
my $svg = svg => [
    :width(700), :height(700),
    circle => [
        :cx(@dot_points[0].coordinates[0]), :cy(@dot_points[0].coordinates[1]), :r(4)
    ],
    circle => [
        :cx(@dot_points[1].coordinates[0]), :cy(@dot_points[1].coordinates[1]), :r(4)
    ],
    circle => [
        :cx(@dot_points[2].coordinates[0]), :cy(@dot_points[2].coordinates[1]), :r(4)
    ],
    circle => [
        :cx(@dot_points[3].coordinates[0]), :cy(@dot_points[3].coordinates[1]), :r(4)
    ],
    path => [
        :d(MakePath($nubs, -1..3, $pad)), :stroke("black"), :stroke-width(3), :fill("none")
    ],
    path => [
        :d(MakePath($poly1, -2..2, $pad)), :stroke("red"), :stroke-width(1), :fill("none")
    ],
    path => [
        :d(MakePath($poly2, 0..3, $pad)), :stroke("green"), :stroke-width(1), :fill("none")
    ],
    path => [
        :d(MakePath($poly3, 1..4, $pad)), :stroke("blue"), :stroke-width(1), :fill("none")
    ]
];

say SVG.serialize($svg);
