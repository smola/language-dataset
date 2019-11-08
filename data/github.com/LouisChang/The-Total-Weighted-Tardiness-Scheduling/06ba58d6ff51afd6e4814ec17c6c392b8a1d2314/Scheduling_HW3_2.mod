set JOBS;

set TIME;

param Processing {JOBS} >= 0;

param Cost {JOBS, TIME} >= 0;


var x {JOBS, TIME} >= 0,binary;

#param z;

minimize Obj:

        sum {j in JOBS, k in TIME} Cost[j,k] * x[j,k];


subject to processing {j in JOBS}: # each job's processing time must be met #

        sum {k in TIME} x[j,k] = Processing[j];


subject to slots {k in TIME}: # each time slot must be assigned a job #

        sum {j in JOBS} x[j,k] = 1;
        
#subject to continous{j in JOBS, m in TIME, n in TIME, k in TIME:m < k < n}:
#		x[j,k] >= x[j,m]+x[j,n]-1;


subject to control_1 {k in TIME: 80 <= k <= 89}:
	x[5,k] =1;
subject to control_2 {k in TIME: 63 <= k <= 79}:
	x[6,k] =1;
subject to control_3 {k in TIME: 47 <= k <= 62}:
	x[7,k] =1;
	
subject to control_4 {k in TIME: 35 <= k <= 46}:
	x[3,k] =1;
subject to control_5 {k in TIME: 17 <= k <= 34}:
	x[2,k] =1;