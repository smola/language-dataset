size(10cm);
import graph;
xaxis("$x$");
yaxis("$y$");
real f1(real x) {return sqrt(x);}
real f2(real x) {return -sqrt(x);}
real f3(real x) {return 1/x;}
real f4(real x) {return x;}
draw(graph(f1,0,4));
draw(graph(f2,0,4));
draw(graph(f3,-4,-0.25));
draw(graph(f3,0.25,4));
draw(graph(f4,-4,4));
label("$\frac{x}{y}=1$",(3.5,3.5),SE);
label("$xy=1$",(0.25,4),SE);
label("$y^2-x=0$",(3,-2),SE);
label("Octagon",(-2.1,2.1),SE);
draw((3,0)--(2.1,2.1)--(0,3)--(-2.1,2.1)--(-3,0)--(-2.1,-2.1)--(0,-3)--(2.1,-2.1)--(3,0));
