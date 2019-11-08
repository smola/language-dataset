# Traveling Salesman Problem (incorrect one because of lack of sub-tour cancellation)

set PLACES;
param lat{PLACES};
param lng{PLACES};

# compute great circle distances and minimum travel times
param d2r := 3.1415926/180;
param alpha{a in PLACES, b in PLACES} := sin(d2r*(lat[a]-lat[b])/2)**2
      + cos(d2r*lat[a])*cos(d2r*lat[b])*sin(d2r*(lng[a]-lng[b])/2)**2;
param gcdist{a in PLACES, b in PLACES} := 2*6371*atan(sqrt(alpha[a,b]),sqrt(1-alpha[a,b]));

# Path constraints
var x{PLACES, PLACES} binary;

# must leave from all places
s.t. lv1 {a in PLACES}: sum{b in PLACES} x[a,b] = 1;

# must arrive at all places
s.t. lv2 {a in PLACES}: sum{b in PLACES} x[b,a] = 1;

minimize obj: sum{a in PLACES} (sum{b in PLACES} (x[a,b] * gcdist[a,b]));

solve;

printf "\nTour:\n\n";
for {a in PLACES} {
	for {b in PLACES} {
		# GLPK has no IF ... THEN ... ELSE for expressions
		for {{0}: x[a,b]}{
			printf "%3s -> %3s\n", a, b;
		}
	}
}
printf "\nTotal tour distance: %f\n\n", obj;

data;

param : PLACES :         lat            lng :=
        ATL       33.6366995    -84.4278639
        BOS       42.3629722    -71.0064167
        DEN       39.8616667   -104.6731667
        DFW       32.8968281    -97.0379958
        JFK       40.6397511    -73.7789256
        LAX       33.9424955   -118.4080684
        ORD       41.9816486    -87.9066714
        STL       38.7486972    -90.3700289
;

end;
