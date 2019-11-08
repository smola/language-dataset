#points
param n;
#vehicles  
param u;
#time intervals
param k := u*n;
#param k := 3;
set Q := {1..n};
set U := {1..u};
set K := {1..k}; 
#itineraries
param t{Q,Q} binary;
param location{U};
param destination{U};
param F{Q,Q} binary;
#c2
var gamma >=0; 
#c3
var x{U,K,Q,Q} binary; 
var uxu{U,K,Q,Q} binary;
var uxu2{U,K,Q,Q,Q} binary;

#objective function, equation 1
maximize time: gamma;

# FLOW CONSTRAINTS

#graph constraints: only itineraries can be traversed by vehicles
subject to graph{i in U, j in K, s in Q, d in Q}: x[i,j,s,d] <= t[s,d];

#minimise waiting time
subject to c4: (sum{i in U, s in Q, j in K, d in Q: d == destination[i]} x[i,j,s,d]) >= gamma;

#only one itinerary per time
subject to c5{i in U, j in K}: sum{s in Q, d in Q} x[i,j,s,d] = 1;

#each vehicle starts from its location
subject to c6{i in U}: sum{s in Q, d in Q: s==location[i]} x[i,1,s,d] = 1;

#all vehicles reach their destination eventually
subject to c7{i in U}: sum{s in Q,d in Q: d==destination[i]} x[i,k,s,d] = 1;

#constraints linearise 1,2,3 used to linearise  uxu[i,j,s,d] = x[i,j-1,s,d]*x[i,j,s,d]
subject to linearise1{i in U, j in {2..k}, s in Q, d in Q}:
		uxu[i,j,s,d]<= x[i,j-1,s,d];
subject to linearise2{i in U, j in {2..k}, s in Q, d in Q}:
		uxu[i,j,s,d]<= x[i,j,s,d];
subject to linearise3{i in U, j in {2..k}, s in Q, d in Q}:
		uxu[i,j,s,d]>= x[i,j-1,s,d] + x[i,j,s,d] - 1;

#constraints linearise 4,5,6 used to linearise  uxu2[i,j,s,q,d] = x[i,j-1,s,q]*x[i,j,q,d]
subject to linearise4{i in U, j in {2..k}, s in Q, q in Q, d in Q}:
		uxu2[i,j,s,q,d]<= x[i,j-1,s,q];
subject to linearise5{i in U, j in {2..k}, s in Q, q in Q, d in Q}:
		uxu2[i,j,s,q,d]<= x[i,j,q,d];
subject to linearise6{i in U, j in {2..k}, s in Q,q in Q, d in Q}:
		uxu2[i,j,s,q,d]>= x[i,j-1,s,q] + x[i,j,q,d] - 1;

#flow constraints, for each k each vehicle  i stays idle or move into an adjacent itinerary
subject to c8{i in U, q in Q, j in {2..k}}:
(sum{s in Q:s!=q} x[i,j-1,s,q])  -  
(sum{s in Q, d in Q:d!=q && s!=q} uxu2[i,j,s,q,d]  +  sum{s in Q:s!=q} uxu[i,j,s,q])  = 0;
	
#complement previous constraints: vehicles do not "jump"  itineraries
subject to c9{i in U, q in Q, j in {2..k}, s1 in Q, s2 in Q: s1!=s2}: 
  x[i,j-1,s1,q]+x[i,j,s2,q] <= 1; 
 
# SAFETY

#no collisions on itineraries
subject to c10{j in K, s in Q, d in Q}: sum{i in U} x[i,j,s,d] <= 1;

#no collisions on points
subject to c11_1{q in Q, j in {2..k}}: 
	sum{i in U, s in Q} x[i,j-1,s,q] - 1 <= sum{i in U, s in Q} uxu[i,j,s,q];
subject to c11_2{q in Q, j in {2..k}}: 
	 sum{i in U, s in Q} uxu[i,j,s,q] <= sum{i in U, s in Q} x[i,j-1,s,q];

#routes must not pass through damaged itineraries
subject to c12{s in Q, d in Q: F[s,d]==1}: sum{i in U,j in K} x[i,j,s,d]=0;
