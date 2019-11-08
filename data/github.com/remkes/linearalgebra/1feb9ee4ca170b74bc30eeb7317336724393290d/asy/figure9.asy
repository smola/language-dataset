size(10cm);
import graph;
usepackage("amsmath");

xlimits(0,5);
ylimits(0,5);

xaxis("$x$",Ticks("%"));
yaxis("$y$",Ticks("%"));

draw((0,0)--(4,1),Arrow); 
draw((0,0)--(1,4),Arrow); 
draw((32/17,8/17)--(1,4),Arrow); 
draw((0,0)--(32/17,8/17),Arrow); 

label("$\begin{pmatrix} 1 \\ 4 \end{pmatrix}$",(0.5,2),SE);
label("$\begin{pmatrix} 4 \\ 1 \end{pmatrix}$",(4,1),E);
label("$\mathrm{Proj}_{\scriptsize \begin{pmatrix} 4 \\ 1 \end{pmatrix}}
\begin{pmatrix} 1 \\ 4 \end{pmatrix}$",(2,0.7),SE);
label("$\mathrm{Perp}_{\scriptsize \begin{pmatrix} 4 \\1 \end{pmatrix}} 
\begin{pmatrix} 1 \\ 4 \end{pmatrix}$",(1.3,3),NE);
