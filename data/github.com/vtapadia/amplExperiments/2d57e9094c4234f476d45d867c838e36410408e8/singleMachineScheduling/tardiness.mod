param N;
set I := 1..N;
set JOBS;

param DURATION {JOBS}, >= 0;
param RELEASE_DATE {JOBS}, >= 0;
param DUE_DATE {i in JOBS}, >= RELEASE_DATE[i];

param bigM := 10000;

# Variable
var y {I, I}, binary;
var t {I}, >= 0;
# Slack
var s {I}, >= 0;

var tar = sum {i in I} s[i];

minimize tardiness: 
	tar;

subject to r1{i in I, j in I: i<>j}:
	t[i]+DURATION[i] <= t[j] + bigM*(1-y[i,j]);
subject to r2{i in I, j in I: i<>j}:
	t[j]+DURATION[j] <= t[i] + bigM*(y[i,j]);
subject to r3{i in I}:
	t[i]+DURATION[i]<=DUE_DATE[i]+s[i];
subject to r4{i in I}:
	t[i]>=RELEASE_DATE[i];

