set side := { 0 .. 8 };
set square := {0 .. 2} * {0 .. 2};

var x [side*side*side] binary;

subto values: forall <m,n> in side*side do sum <k> in side:x[m,n,k]==1;
subto rows: forall <n,k> in side*side do sum <m> in side:x[m,n,k]==1;
subto cols: forall <m,k> in side*side do sum <n> in side:x[m,n,k]==1;

subto left_upper: forall <k> in side do sum <m,n> in square:x[m,n,k]==1;
subto left_mid: forall <k> in side do sum <m,n> in square:x[m+3,n,k]==1;
subto left_lower: forall <k> in side do sum <m,n> in square:x[m+6,n,k]==1;

subto mid_upper: forall <k> in side do sum <m,n> in square:x[m,n+3,k]==1;
subto mid_mid: forall <k> in side do sum <m,n> in square:x[m+3,n+3,k]==1;
subto mid_lower: forall <k> in side do sum <m,n> in square:x[m+6,n+3,k]==1;

subto right_upper: forall <k> in side do sum <m,n> in square:x[m,n+6,k]==1;
subto right_mid: forall <k> in side do sum <m,n> in square:x[m+3,n+6,k]==1;
subto right_lower: forall <k> in side do sum <m,n> in square:x[m+6,n+6,k]==1;


subto x_119: x[0,0,8] == 1;
subto x_148: x[0,3,7] == 1;
subto x_172: x[0,6,3] == 1;
subto x_187: x[0,7,6] == 1;
subto x_193: x[0,8,2] == 1;

subto x_244: x[1,3,3] == 1;

subto x_323: x[2,1,2] == 1;
subto x_332: x[2,2,1] == 1;
subto x_396: x[2,8,5] == 1;

subto x_411: x[3,0,0] == 1;
subto x_438: x[3,2,7] == 1;

subto x_535: x[4,2,4] == 1;
subto x_558: x[4,4,7] == 1;
subto x_573: x[4,6,2] == 1;

subto x_629: x[5,1,8] == 1;
subto x_665: x[5,5,4] == 1;
subto x_686: x[5,7,5] == 1;

subto x_751: x[6,4,0] == 1;
subto x_776: x[6,6,5] == 1;
subto x_782: x[6,7,1] == 1;

subto x_862: x[7,5,1] == 1;
subto x_878: x[7,6,7] == 1;

subto x_915: x[8,0,4] == 1;
subto x_933: x[8,2,2] == 1;
subto x_999: x[8,8,8] == 1;
