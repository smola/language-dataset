## Task Set:

# task index
set Ti := {read "task_util.lst" as "<1n>" comment "#" };

# task utilization
param U[Ti] := read "task_util.lst" as "<1n>2n" comment "#";

# cost of communication (between modules)
param Cc[Ti*Ti] := read "communication_cost.lst" as "<1n,2n>3n" comment "#" default 0;

# cost of interference (between modules)
param Ifc[Ti*Ti] := read "interference_cost_finite.lst" as "<1n,2n>3n" comment "#" default 0;

# number of modules
param m := read "m.dat" as "1n" use 1 comment "#";

# module index
set Mi := {1..m};

# maximum allowed utilization
param max_util := read "max_util.dat" as "1n" use 1 comment "#";

## Auxiliary Functions
# TODO
defnumb kronecker_delta(i,k):= if i==k then 1 else 0 end; 

## Variables

var A1[Ti*Mi] binary;
var A2[Ti*Mi*Ti*Mi] binary;

## TODO: add additional variables,
# if necessary

## TODO: add objective

## Minimize interference cost and communication cost

minimize cost : sum<i,k> in Ti*Mi : sum<j,l> in Ti*Mi:(1-kronecker_delta(i,j))*A2[i,k,j,l]*(Cc[i,j]*(1 - kronecker_delta(k,l)) + Ifc[i,j] * kronecker_delta(k,l));

##Constraints

## Every task assigned to only 1 module

subto C_1: forall <i> in Ti do           
                 sum<j> in Mi : A1[i,j] == 1;

## Total utilization of each module less than its maximum capacity. 

subto C_2: forall <j> in Mi do         
                 sum<i> in Ti : U[i] * A1[i,j] <= max_util ;

## Constraint3 to ensure A[i,k,j,l] = A[i,k] * A[j,l]

subto C_3: forall <i,k> in Ti*Mi do  
                forall <j,l> in Ti*Mi do   
                   vif A1[i,k] == 1 and A1[j,l] == 1
                      then A2[i,k,j,l] == 1 
                      else A2[i,k,j,l] == 0 end;   
                      
## TODO: add constraints
                      
                                  
          
