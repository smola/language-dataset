param n := 20;
param d := 7;
param m := 3;
param l := 15; 

set intervals := {1..n};
set activities := {1..m};
set employees := {1..l};
set days := {1..d};

param workTime[employees] := <1> 28, <2> 28, <3> 28 default 56;
param reqWorkAct[<i,a> in intervals*activities] := a;
param workerSkill[activities*employees] := <1,1> 0, <2,3> 0 default 1;

var W[days*intervals*activities*employees] binary;
var shiftStart[days*intervals*employees] binary;
var Slack[days*intervals*activities] >= 0;

subto meetRequirement:
    forall <r,i,a> in days*intervals*activities:
        sum <j> in employees:
            W[r,i,a,j] >= reqWorkAct[i,a] - Slack[r,i,a];

subto meetSkillSet:
    forall <r,i,a,j> in days*intervals*activities*employees:
        W[r,i,a,j] <= workerSkill[a,j];

subto meetMaxTime:
    forall <j> in employees:
        sum <r,i,a> in days*intervals*activities:
            W[r,i,a,j] <= workTime[j];

subto oneActivityPerInterval:
    forall <r> in days:
        forall <i> in intervals:
            forall <j> in employees:
                sum <a> in activities:
                    W[r,i,a,j] <= 1;

minimize SlackObjective:
    sum <r,i,a> in days*intervals*activities:
        Slack[r,i,a];