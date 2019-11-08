//settings.outformat = "pdf";       //Enable to output pdf files directly.
settings.outformat = "prc";     //Enable to output prc files to load into TeX
import solids;                    //Enables filling and drawing of surfaces of revolution
import three;											//Enables 3D plotting
size(5cm,0);
viewportmargin=4mm;
currentprojection=orthographic(
camera=(9,6,2),
up=(0,0,1),
target=(0,0,0),
zoom=0.8,
center=true); 										//Center puts rotation at center.
pen color=black+opacity(0.95);		//High opacity - for solid sphere slices.
pen colorBE=red+opacity(0.95);		//High opacity - for solid sphere slices.
render render=render(compression=0.5,merge=true); 				 //Set render field. 0.5 is low resolution.

real rC = 0.20;

triple p001 = (0.000,0.000,0.000);
triple p002 = (0.000,0.000,1.540);
triple p003 = (-0.177,-1.441,2.054);
triple p004 = (-1.239,-2.163,1.205);
triple p005 = (-2.588,-2.148,1.948);
triple p006 = (-2.352,-1.828,3.436);
triple p007 = (-2.885,-2.985,4.301);
triple p008 = (-1.914,-3.243,5.468);
triple p009 = (-0.494,-3.466,4.916);
triple p010 = (0.426,-3.964,6.047);
triple p011 = (-0.396,-4.797,7.047);
triple p012 = (-0.702,-3.948,8.295);
triple p013 = (0.526,-3.938,9.224);
triple p014 = (1.753,-3.425,8.449);
triple p015 = (2.013,-4.337,7.236);
triple p016 = (2.350,-5.759,7.724);
triple p017 = (1.874,-6.787,6.682);
triple p018 = (2.418,-6.402,5.294);
triple p019 = (1.244,-6.070,4.355);
triple p020 = (1.632,-6.411,2.904);
triple p021 = (2.388,-7.753,2.877);
triple p022 = (1.382,-8.911,3.013);
triple p022 = (0.016,-8.357,3.459);
triple p023 = (-0.850,-8.067,2.220);
triple p024 = (-1.953,-7.057,2.589);
triple p025 = (-2.259,-7.157,4.095);
triple p026 = (-1.218,-6.343,4.885);
triple p027 = (-0.525,-7.255,5.915);
triple p028 = (-0.746,-8.729,5.527);
triple p029 = (-2.256,-9.005,5.396);
triple p030 = (-2.512,-10.520,5.499);
triple p031 = (-3.777,-10.883,4.700);
triple p032 = (-3.765,-10.140,3.352);
triple p033 = (-4.130,-11.119,2.221);
triple p034 = (-3.637,-10.557,0.875);
triple p035 = (-2.846,-11.643,0.123);
triple p036 = (-1.997,-10.989,-0.983);
triple p037 = (-0.722,-10.387,-0.364);
triple p038 = (0.166,-9.804,-1.480);
triple p039 = (1.302,-10.792,-1.801);
triple p040 = (2.644,-10.038,-1.839);
triple p041 = (3.324,-10.265,-3.202);
triple p042 = (2.453,-11.200,-4.061);
triple p043 = (0.965,-10.880,-3.822);
triple p044 = (0.176,-12.193,-3.661);
triple p045 = (-1.279,-11.874,-3.273);
triple p046 = (-1.779,-10.671,-4.095);
triple p047 = (-0.712,-10.281,-5.135);
triple p048 = (-0.825,-11.210,-6.359);
triple p049 = (-1.168,-12.637,-5.892);
triple p050 = (-0.194,-13.638,-6.539);

draw(shift(p001)*scale(rC,rC,rC)*unitsphere,colorBE,render);
draw(shift(p002)*scale(rC,rC,rC)*unitsphere,color,render);
draw(shift(p003)*scale(rC,rC,rC)*unitsphere,color,render);
draw(shift(p004)*scale(rC,rC,rC)*unitsphere,color,render);
draw(shift(p005)*scale(rC,rC,rC)*unitsphere,color,render);
draw(shift(p006)*scale(rC,rC,rC)*unitsphere,color,render);
draw(shift(p007)*scale(rC,rC,rC)*unitsphere,color,render);
draw(shift(p008)*scale(rC,rC,rC)*unitsphere,color,render);
draw(shift(p009)*scale(rC,rC,rC)*unitsphere,color,render);
draw(shift(p010)*scale(rC,rC,rC)*unitsphere,color,render);
draw(shift(p011)*scale(rC,rC,rC)*unitsphere,color,render);
draw(shift(p012)*scale(rC,rC,rC)*unitsphere,color,render);
draw(shift(p013)*scale(rC,rC,rC)*unitsphere,color,render);
draw(shift(p014)*scale(rC,rC,rC)*unitsphere,color,render);
draw(shift(p015)*scale(rC,rC,rC)*unitsphere,color,render);
draw(shift(p016)*scale(rC,rC,rC)*unitsphere,color,render);
draw(shift(p017)*scale(rC,rC,rC)*unitsphere,color,render);
draw(shift(p018)*scale(rC,rC,rC)*unitsphere,color,render);
draw(shift(p019)*scale(rC,rC,rC)*unitsphere,color,render);
draw(shift(p020)*scale(rC,rC,rC)*unitsphere,color,render);
draw(shift(p021)*scale(rC,rC,rC)*unitsphere,color,render);
draw(shift(p022)*scale(rC,rC,rC)*unitsphere,color,render);
draw(shift(p023)*scale(rC,rC,rC)*unitsphere,color,render);
draw(shift(p024)*scale(rC,rC,rC)*unitsphere,color,render);
draw(shift(p025)*scale(rC,rC,rC)*unitsphere,color,render);
draw(shift(p026)*scale(rC,rC,rC)*unitsphere,color,render);
draw(shift(p027)*scale(rC,rC,rC)*unitsphere,color,render);
draw(shift(p028)*scale(rC,rC,rC)*unitsphere,color,render);
draw(shift(p029)*scale(rC,rC,rC)*unitsphere,color,render);
draw(shift(p030)*scale(rC,rC,rC)*unitsphere,color,render);
draw(shift(p031)*scale(rC,rC,rC)*unitsphere,color,render);
draw(shift(p032)*scale(rC,rC,rC)*unitsphere,color,render);
draw(shift(p033)*scale(rC,rC,rC)*unitsphere,color,render);
draw(shift(p034)*scale(rC,rC,rC)*unitsphere,color,render);
draw(shift(p035)*scale(rC,rC,rC)*unitsphere,color,render);
draw(shift(p036)*scale(rC,rC,rC)*unitsphere,color,render);
draw(shift(p037)*scale(rC,rC,rC)*unitsphere,color,render);
draw(shift(p038)*scale(rC,rC,rC)*unitsphere,color,render);
draw(shift(p039)*scale(rC,rC,rC)*unitsphere,color,render);
draw(shift(p040)*scale(rC,rC,rC)*unitsphere,color,render);
draw(shift(p041)*scale(rC,rC,rC)*unitsphere,color,render);
draw(shift(p042)*scale(rC,rC,rC)*unitsphere,color,render);
draw(shift(p043)*scale(rC,rC,rC)*unitsphere,color,render);
draw(shift(p044)*scale(rC,rC,rC)*unitsphere,color,render);
draw(shift(p045)*scale(rC,rC,rC)*unitsphere,color,render);
draw(shift(p046)*scale(rC,rC,rC)*unitsphere,color,render);
draw(shift(p047)*scale(rC,rC,rC)*unitsphere,color,render);
draw(shift(p048)*scale(rC,rC,rC)*unitsphere,color,render);
draw(shift(p049)*scale(rC,rC,rC)*unitsphere,color,render);
draw(shift(p050)*scale(rC,rC,rC)*unitsphere,colorBE,render);

draw((p001--p002--p003--p004--p005--p006--p007--p008--p009--p010--p011--p012--p013--p014--p015--p016--p017--p018--p019--p020--p021--p022--p023--p024--p025--p026--p027--p028--p029--p030--p031--p032--p033--p034--p035--p036--p037--p038--p039--p040--p041--p042--p043--p044--p045--p046--p047--p048--p049--p050),black+linewidth(1));
