/* Number of problems */
param I_count, integer, > 0;

/* Number of strategies */
param J_count, integer, > 0;

set I := 1..I_count;
set J := 1..J_count;

/* The time required to solve i with j. */
param T{i in I, j in J}, >= 0;

/* The time strategy j is allowed to run. */
var R{j in J}, >= 0;

/* Problem i is solved by strategy j. */
var S{i in I, j in J}, binary;

/* The total time to run strategies is bounded by 300 seconds. */
subject to upperbound:
    sum{j in J} R[j] <= 300;

s.t. phi{i in I}: sum{j in J} S[i,j] <= 1;
s.t. psi{i in I, j in J}: R[j] >= S[i,j] * T[i,j];

/* Maximize the number of problems solved with this assignment. */
maximize obj: sum{i in I, j in J} S[i,j];

solve;

printf{j in J} "%5d %5f\n", j, R[j];
printf "     Total: %10g\n", sum{i in I, j in J} S[i,j];

data;

param I_count := 2;
param J_count := 2;
param T :  1  2 :=
    1 0.1 0.2
    2 0.4 0.2;

end;
