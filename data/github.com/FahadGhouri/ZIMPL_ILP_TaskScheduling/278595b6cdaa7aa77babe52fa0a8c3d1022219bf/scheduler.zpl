## Task Set:

# task indices
set Ti := {read "taskset.lst" as "<1n>" comment "#" };

# task release times relative to start of the period
param Tr[Ti] := read "taskset.lst" as "<1n>2n" comment "#";

# task deadlines relative to start of the period
param Td[Ti] := read "taskset.lst" as "<1n>3n" comment "#";

# task execution times
param Te[Ti] := read "taskset.lst" as "<1n>4n" comment "#";

# global period
param p := read "period.dat" as "1n" use 1 comment "#";

## Variables

# start execution at ts
var ts[<i> in Ti] integer >= 0 <= p;

# end execution at te
var te[<i> in Ti] integer >= 0 <= p;


## TODO: add additional variables,
# if necessary

## TODO: add objective
#For keeping the Time between release and start of a task as short as possible to reduce slack time inbetween
minimize slack: sum<i> in Ti: ts[1]-Tr[1];


## Constraints
#For the Task Allocation time equal to the Execution time
subto C_1: forall <i> in Ti do te[i] - ts[i] == Te[i];

#For Tasks to be started only after they are released
subto C_2: forall <i> in Ti do ts[i] >= Tr[i];

#For Tasks to end before their deadlines
subto C_3: forall <i> in Ti do te[i] <= Td[i];

#For Tasks to have non-Preemption and have seperated alloaction
subto C_4: forall <i> in Ti do 
            forall <j> in Ti with j!=i do           
                vif (ts[j] >= ts[i] and ts[j] < te[i]) then ts[j] == (ts[i]+Te[i]) end;

## TODO: add constraints
