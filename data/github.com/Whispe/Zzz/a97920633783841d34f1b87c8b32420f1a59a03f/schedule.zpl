# Basic schedule solver.

set Day := {1..7};
param base_fun_rate := 1; # Part (d).
param sleepy_work_rate := 0.75; # Part (f).

var work[Day]; # Part (b).
var sleep[Day]; # Part (b).
var play[Day]; # Part (b).
var events[Day]; # Number of hours spent on events each day.
var sleepy[Day]; # Part (f).

# Part (d): fun events.
set E := { read "events.txt" as "<1s>" }; # The set of events by name.
param edays[E] := read "events.txt" as "<1s> 2n"; # The set of events by name and day of occurrence.
param ehours[E] := read "events.txt" as "<1s> 3n"; # The set of events by name and duration in hours.
param efunrate[E] := read "events.txt" as "<1s> 4n"; # The set of events by name and fun rate.

var attendevent[E] binary; # Whether or not we go to the event.
var attendancedurations[E]; # How long we must spend on the event.
var toteventfun; # Total amount of fun obtained from going to the events for the week.

subto calcattendancedurations: forall <e> in E: attendancedurations[e] == attendevent[e] * ehours[e];

var eventsperday[Day*E]; # Number of event hours per day for a given event.
subto spendhoursonevent: forall <e> in E: eventsperday[edays[e],e] == attendevent[e] * attendancedurations[e];
subto attendeventsbyday: forall <d> in Day: events[d] == sum <e> in E: eventsperday[d,e];
subto calctoteventfun: toteventfun == sum <e> in E: efunrate[e] * attendancedurations[e];

# Part (e): assignments.
set A := { read "assignments.txt" as "<1s>" }; # The set of assignemnts by name.
param adeadline[A] := read "assignments.txt" as "<1s> 2n"; # The set of assignments by name and due date.
param ahours[A] := read "assignments.txt" as "<1s> 3n"; # The set of assignments by name and hours to complete.
param apenrate[A] := read "assignments.txt" as "<1s> 4n"; # The set of assignments by name and penatly rate if not completed.

var hoursdone[A];
var hoursskipped[A];
var workrate[Day];
var workperday[Day*A]; # Number of hours per day working on a particular assignment.
subto noextrawork: forall <a> in A: sum <d> in Day: workperday[d,a] <= ahours[a];
subto nolatework: forall <d,a> in Day*A: if d>adeadline[a] then workperday[d,a] == 0 else 0==0 end;
subto calcsleepyworkrate: forall <d> in Day: workrate[d] == (1-sleepy[d]) + sleepy[d]*sleepy_work_rate;
subto calchoursdone: forall <a> in A: hoursdone[a] == sum <d> in Day: workrate[d] * workperday[d,a];
subto calchoursskipped: forall <a> in A: hoursskipped[a] == ahours[a] - hoursdone[a];
subto calcwork: forall <d> in Day: work[d] == sum <a> in A: workperday[d,a];

# Part (f): sleepy.
#subto detsleepy: forall <d> in Day without {1,2}: vif sleep[d-2] + sleep[d-1] + sleep[d] < 24 then sleepy[d] == 1 end;
param m := -25;
subto detsleepy: forall <d> in Day without {1,2}: sleep[d-2] + sleep[d-1] + sleep[d] >= 24 + 0.001 + (m-0.001)*sleepy[d];

# Universal constraints.
subto twentyfourhours: forall <d> in Day: work[d] + sleep[d] + play[d] + events[d] == 24; # Part (a).
subto threedayssleepeighteen: forall <d> in Day without {6,7}: sleep[d] + sleep[d+1] + sleep[d+2] >= 18; # Part (c).

var totplayfun;
var incompleteworkpenalty;
subto totalplayfun: totplayfun == sum <d> in Day: base_fun_rate * play[d];
subto totalincompletepenalty: incompleteworkpenalty == sum <a> in A: apenrate[a] * hoursskipped[a];
maximize totfun: totplayfun + toteventfun - incompleteworkpenalty;
