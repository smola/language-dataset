/*
Nested, Subsampling Design

The data, log and output from SAS for the Nested/Subsampling example from the course notes are given below. This will be updated from time to time!

Here are the input data and SAS statements
*/

data subsamp1;
input trt tree apple wt;
cards;
   1  1  1  313.063
   1  1  2  329.132
   1  1  3  334.278
   1  1  4  330.088
   1  1  5  334.987
   1  1  6  325.075
   1  2  1  333.936
   1  2  2  326.155
   1  2  3  352.854
   1  2  4  350.791
   1  2  5  318.560
   1  2  6  323.473
   1  3  1  345.494
   1  3  2  349.296
   1  3  3  339.190
   1  3  4  338.942
   1  3  5  331.370
   1  3  6  339.097
   1  4  1  340.840
   1  4  2  336.798
   1  4  3  313.810
   1  4  4  333.880
   1  4  5  343.068
   1  4  6  319.171
   2  1  1  349.271
   2  1  2  336.695
   2  1  3  352.797
   2  1  4  348.486
   2  1  5  352.077
   2  1  6  341.423
   2  2  1  356.880
   2  2  2  356.256
   2  2  3  364.950
   2  2  4  360.570
   2  2  5  362.104
   2  2  6  371.829
   2  3  1  324.161
   2  3  2  340.130
   2  3  3  334.580
   2  3  4  342.813
   2  3  5  327.415
   2  3  6  333.571
   2  4  1  338.742
   2  4  2  340.348
   2  4  3  362.837
   2  4  4  340.782
   2  4  5  348.730
   2  4  6  325.444
   3  1  1  387.868
   3  1  2  372.807
   3  1  3  380.505
   3  1  4  391.804
   3  1  5  388.935
   3  1  6  361.860
   3  2  1  377.948
   3  2  2  380.033
   3  2  3  361.913
   3  2  4  363.098
   3  2  5  365.375
   3  2  6  382.121
   3  3  1  363.583
   3  3  2  387.727
   3  3  3  373.021
   3  3  4  362.931
   3  3  5  378.928
   3  3  6  364.442
   3  4  1  374.851
   3  4  2  361.291
   3  4  3  377.389
   3  4  4  366.722
   3  4  5  374.187
   3  4  6  380.383
   ;
proc glm; /* ANOVA ignoring trees (wrong!) */
class trt;
model wt = trt;
lsmeans trt/stderr pdiff;
contrast ' trt 1 - (trt 2+3)/2' trt 1 -.5 -.5;
estimate ' trt 1 - (trt 2+3)/2' trt 1 -.5 -.5;
run;

proc glm; /* Nested ANOVA, testing trt against tree Mean Square */
class trt tree;
model wt = trt tree(trt)/xpx solution;
random tree(trt); /* Specifying tree within trt as a random effect */
test h=trt e=tree(trt); /* Explicitly testing MS trt against MS tree */
lsmeans trt/stderr pdiff e=tree(trt); /* Least Squares Means, using
                                         MS tree as the appropriate error */
contrast ' trt 1 - (trt 2+3)/2' trt 1 -.5 -.5 tree(trt) .25 .25 .25 .25
  -.125 -.125 -.125 -.125 -.125 -.125 -.125 -.125/e=tree(trt);
estimate ' trt 1 - (trt 2+3)/2' trt 1 -.5 -.5 tree(trt) .25 .25 .25 .25
  -.125 -.125 -.125 -.125 -.125 -.125 -.125 -.125;
/* Explicitly construct contrasts for SS trt (Type I) */
contrast ' SS trt' trt 4 -4 0 tree(trt) 1 1 1 1 -1 -1 -1 -1 0 0 0 0,
                   trt 4 - -4 tree(trt) 1 1 1 1 0 0 0 0 -1 -1 -1 -1;
run;

proc mixed; /* Mixed model analysis, as it should be done */
class trt tree;
model wt = trt; /* specify only fixed effects */
random tree(trt); /* specify tree within trt as a random effect */
lsmeans trt;
contrast ' trt 1 - (trt 2+3)/2' trt 1 -.5 -.5;
estimate ' trt 1 - (trt 2+3)/2' trt 1 -.5 -.5;
run;

proc sort; /* sort data, by trt and tree within trt */
by trt tree;
run;

proc means; /* compute mean for each tree, and output to a new dataset */
var wt;
by trt tree;
output out=pmeans mean=gmean;
run;

proc glm data=pmeans; /* Analyse tree means, using a 1-way ANOVA */
class trt;
model gmean = trt;
lsmeans trt/stderr pdiff;
contrast ' trt 1 - (trt 2+3)/2' trt 1 -.5 -.5;
estimate ' trt 1 - (trt 2+3)/2' trt 1 -.5 -.5;
run;
