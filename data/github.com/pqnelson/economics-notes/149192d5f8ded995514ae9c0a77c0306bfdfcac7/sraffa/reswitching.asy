import graph;
size(350);

real f(real r) {
  real r_percent = 0.01*r;
  real R = 0.25;
  real w = 1 - (r_percent/R);
  real pA = 20.0 * w * ((1.0 + r_percent)^8);
  real pB = 19.0*w + w*((1.0 + r_percent)^25);
  return pA - pB;
}

draw(graph(f,0,25,n=2000));


pen thin=linewidth(0.5*linewidth());
xaxis(Label("$r$",0.87),xmin=0, xmax=25,
      Ticks(scale(.7)*Label(align=E),NoZero,
	    Step=1,
	    Size=1mm, pTick=black,ptick=gray));
yaxis("Value",
      Ticks(scale(.7)*Label(),NoZero,
	    Step=1,step=.25,Size=1mm,size=.5mm,
            pTick=black,ptick=gray));