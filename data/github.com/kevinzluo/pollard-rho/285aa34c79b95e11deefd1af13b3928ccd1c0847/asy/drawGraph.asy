size(10cm);
int p = 151;
defaultpen(fontsize(6pt));
real moduno(real ind) {
	if(ind <= 1) return ind;
    else return ind-1;
}
for(int i = 0; i < p; ++i) {
	real ang = 90-360/p * i + 360;
	pair pt = dir(ang);
    dot(pt);
    if(ang > 90 && ang < 270) {
    	label(rotate(180 + ang)*("$" + string(i) + "$"), pt, pt);
    } else {
    	label(rotate(ang)*("$" + string(i) + "$"), pt, pt);
    }

    int j = (i*i+1) % p;

    real ang2 = 90 - 360/p * j + 360;
    pair pt2 = dir(ang2);

    /* axialshade
    pair offset = 0.001 * (rotate(90) * (pt2 - pt));
    axialshade(pt - offset -- pt + offset -- pt2 + offset -- pt2 - offset --cycle, red, pt, black, dir(ang2));
    */

    draw(pt -- pt2, 0.5pt, EndArrow(size=5, filltype=NoFill));

    /* rgbdraw: draw(pt -- dir(ang2), rgb(moduno(1/3+i/p),moduno(2/3+i/p),moduno(i/p))); */
}
