// lp_draw.asy
// Sara Fish, July 6 2019

/* L_p Drawing Library
- ball: draws L_p ball. Now only works with integer p because of precision issues.
    Syntax: ball( center, radius, p, color)
       e.g. ball( (0,0), 1, 3, blue )
            makes a blue unit ball in L_3.
    Set p = 0 for infty.
*/ 

unitsize(1cm);
import contour;

//Draws a ball with center, radius, in L_p, color. 
void ball(pair center, real radius, int p, pen color=gray(0))
{
    if (p >= 1) {
        real f1(real x, real y) {return (x-center.x)^p + (y-center.y)^p - radius^p; }
        real f2(real x, real y) {return (x-center.x)^p + (center.y - y)^p - radius^p; }
        real f3(real x, real y) {return (center.x - x)^p + (y-center.y)^p - radius^p; }
        real f4(real x, real y) {return (center.x - x)^p + (center.y - y)^p - radius^p; }
        pair bottom_left = center - (radius, radius);
        pair upper_right = center + (radius, radius);
        guide[][] g1 = contour(f1, bottom_left, upper_right, new real[] {0});
        guide[][] g2 = contour(f2, bottom_left, upper_right, new real[] {0});
        guide[][] g3 = contour(f3, bottom_left, upper_right, new real[] {0});
        guide[][] g4 = contour(f4, bottom_left, upper_right, new real[] {0});
        draw(g1[0], color);
        draw(g2[0], color);
        draw(g3[0], color);
        draw(g4[0], color);
    } else if (p == 0) {
        pair bottom_left = center + (-radius, -radius);
        pair bottom_right = center + (radius, -radius);
        pair upper_left = center + (-radius, radius);
        pair upper_right = center + (radius, radius);
        draw( bottom_left -- bottom_right -- upper_right -- upper_left -- bottom_left, color);
    }
}

// Returns the distance between p1 and p2 in L_p.
real dist(pair p1, pair p2, int p)
{
    return ( abs(p1.x - p2.x)^p + abs(p1.y - p2.y)^p )^(1/p);
}
