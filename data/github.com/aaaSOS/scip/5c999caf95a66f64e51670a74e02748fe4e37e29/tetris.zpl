param imax := 7;
param jmax := 7;
param kmaxl := 7;
param kmaxj := 7;
param kmaxi := 7;
param kmaxo := 7;
param kmaxs := 7;
param kmaxz := 7;
param kmaxt := 7;


set IL := { 1 .. imax };
set JL := { 1 .. jmax };
set KL := { 1 .. kmaxl };
set KJ := { 1 .. kmaxj };
set KI := { 1 .. kmaxi };
set KO := { 1 .. kmaxo };
set KS := { 1 .. kmaxs };
set KZ := { 1 .. kmaxz };
set KT := { 1 .. kmaxt };


set LL := { 1 .. 4 };
var 1L[IL*JL*KL*LL] binary;
var 2L[IL*JL*KL*LL] binary;
var 3L[IL*JL*KL*LL] binary;
var 4L[IL*JL*KL*LL] binary;
var 1J[IL*JL*KJ*LL] binary;
var 2J[IL*JL*KJ*LL] binary;
var 3J[IL*JL*KJ*LL] binary;
var 4J[IL*JL*KJ*LL] binary;
var 1I[IL*JL*KI*LL] binary;
var 2I[IL*JL*KI*LL] binary;
var 1O[IL*JL*KO*LL] binary;
var 1S[IL*JL*KS*LL] binary;
var 2S[IL*JL*KS*LL] binary;
var 1Z[IL*JL*KZ*LL] binary;
var 2Z[IL*JL*KZ*LL] binary;
var 1T[IL*JL*KT*LL] binary;
var 2T[IL*JL*KT*LL] binary;
var 3T[IL*JL*KT*LL] binary;
var 4T[IL*JL*KT*LL] binary;
var N[IL*JL] binary;
var M[JL] binary;
var CON1L[{ 1 .. (imax - 2) }*{ 1 .. (jmax - 1) }*KL] binary;
var CON2L[{ 1 .. (imax - 1) }*{ 1 .. (jmax - 2) }*KL] binary;
var CON3L[{ 1 .. (imax - 2) }*{ 1 .. (jmax - 1) }*KL] binary;
var CON4L[{ 1 .. (imax - 1) }*{ 1 .. (jmax - 2) }*KL] binary;
var CON1J[{ 1 .. (imax - 1) }*{ 1 .. (jmax - 2) }*KJ] binary;
var CON2J[{ 1 .. (imax - 2) }*{ 1 .. (jmax - 1) }*KJ] binary;
var CON3J[{ 1 .. (imax - 1) }*{ 1 .. (jmax - 2) }*KJ] binary;
var CON4J[{ 1 .. (imax - 2) }*{ 1 .. (jmax - 1) }*KJ] binary;
var CON1I[{ 1 .. imax }*{ 1 .. (jmax - 3) }*KI] binary;
var CON2I[{ 1 .. (imax - 3) }*{ 1 .. jmax }*KI] binary;
var CON1O[{ 1 .. (imax - 1) }*{ 1 .. (jmax - 1) }*KO] binary;
var CON1S[{ 1 .. (imax - 2) }*{ 1 .. (jmax - 1) }*KS] binary;
var CON2S[{ 1 .. (imax - 1) }*{ 1 .. (jmax - 2) }*KS] binary;
var CON1Z[{ 1 .. (imax - 2) }*{ 1 .. (jmax - 1) }*KZ] binary;
var CON2Z[{ 1 .. (imax - 1) }*{ 1 .. (jmax - 2) }*KZ] binary;
var CON1T[{ 1 .. (imax - 2) }*{ 1 .. (jmax - 1) }*KT] binary;
var CON2T[{ 1 .. (imax - 1) }*{ 1 .. (jmax - 2) }*KT] binary;
var CON3T[{ 1 .. (imax - 2) }*{ 1 .. (jmax - 1) }*KT] binary;
var CON4T[{ 1 .. (imax - 1) }*{ 1 .. (jmax - 2) }*KT] binary;
var CON1L[{ 1 .. (imax - 2) }*{ 1 .. (jmax - 1) }*KL] binary;
var CON2L[{ 1 .. (imax - 1) }*{ 1 .. (jmax - 2) }*KL] binary;
var CON3L[{ 1 .. (imax - 2) }*{ 1 .. (jmax - 1) }*KL] binary;
var CON4L[{ 1 .. (imax - 1) }*{ 1 .. (jmax - 2) }*KL] binary;
var CON1J[{ 1 .. (imax - 1) }*{ 1 .. (jmax - 2) }*KJ] binary;
var CON2J[{ 1 .. (imax - 2) }*{ 1 .. (jmax - 1) }*KJ] binary;
var CON3J[{ 1 .. (imax - 1) }*{ 1 .. (jmax - 2) }*KJ] binary;
var CON4J[{ 1 .. (imax - 2) }*{ 1 .. (jmax - 1) }*KJ] binary;
var CON1I[{ 1 .. imax }*{ 1 .. (jmax - 3) }*KI] binary;
var CON2I[{ 1 .. (imax - 3) }*{ 1 .. jmax }*KI] binary;
var CON1O[{ 1 .. (imax - 1) }*{ 1 .. (jmax - 1) }*KO] binary;
var CON1S[{ 1 .. (imax - 2) }*{ 1 .. (jmax - 1) }*KS] binary;
var CON2S[{ 1 .. (imax - 1) }*{ 1 .. (jmax - 2) }*KS] binary;
var CON1Z[{ 1 .. (imax - 2) }*{ 1 .. (jmax - 1) }*KZ] binary;
var CON2Z[{ 1 .. (imax - 1) }*{ 1 .. (jmax - 2) }*KZ] binary;
var CON1T[{ 1 .. (imax - 2) }*{ 1 .. (jmax - 1) }*KT] binary;
var CON2T[{ 1 .. (imax - 1) }*{ 1 .. (jmax - 2) }*KT] binary;
var CON3T[{ 1 .. (imax - 2) }*{ 1 .. (jmax - 1) }*KT] binary;
var CON4T[{ 1 .. (imax - 1) }*{ 1 .. (jmax - 2) }*KT] binary;
maximize packing:
    sum <j> in JL : M[j];
subto con1: forall <i,j> in IL*JL do
   (sum <k,l> in KL*LL : 1L[i,j,k,l]) 
+ (sum <k,l> in KL*LL : 2L[i,j,k,l])                       
+ (sum <k,l> in KL*LL : 3L[i,j,k,l]) 
+ (sum <k,l> in KL*LL : 4L[i,j,k,l]) 
+ (sum <k,l> in KJ*LL : 1J[i,j,k,l]) 
+ (sum <k,l> in KJ*LL : 2J[i,j,k,l]) 
+ (sum <k,l> in KJ*LL : 3J[i,j,k,l]) 
+ (sum <k,l> in KJ*LL : 4J[i,j,k,l]) 
+ (sum <k,l> in KI*LL : 1I[i,j,k,l]) 
+ (sum <k,l> in KI*LL : 2I[i,j,k,l]) 
+ (sum <k,l> in KO*LL : 1O[i,j,k,l]) 
+ (sum <k,l> in KS*LL : 1S[i,j,k,l]) 
+ (sum <k,l> in KS*LL : 2S[i,j,k,l]) 
+ (sum <k,l> in KZ*LL : 1Z[i,j,k,l]) 
+ (sum <k,l> in KZ*LL : 2Z[i,j,k,l]) 
+ (sum <k,l> in KT*LL : 1T[i,j,k,l]) 
+ (sum <k,l> in KT*LL : 2T[i,j,k,l]) 
+ (sum <k,l> in KT*LL : 3T[i,j,k,l]) 
+ (sum <k,l> in KT*LL : 4T[i,j,k,l]) 
<= 1;

subto con2l: forall <k,l> in KL*LL do
    sum <i,j> in IL*JL: (1L[i,j,k,l] + 2L[i,j,k,l] + 3L[i,j,k,l] + 4L[i,j,k,l]) <= 1;
subto con2j: forall <k,l> in KJ*LL do
    sum <i,j> in IL*JL: (1J[i,j,k,l] + 2J[i,j,k,l] + 3J[i,j,k,l] + 4J[i,j,k,l]) <= 1;
subto con2i: forall <k,l> in KI*LL do
    sum <i,j> in IL*JL: (1I[i,j,k,l] + 2I[i,j,k,l]) <= 1;
subto con2o: forall <k,l> in KO*LL do
    sum <i,j> in IL*JL: 1O[i,j,k,l] <= 1;
subto con2s: forall <k,l> in KS*LL do
    sum <i,j> in IL*JL: (1S[i,j,k,l] + 2S[i,j,k,l]) <= 1;
subto con2z: forall <k,l> in KZ*LL do
    sum <i,j> in IL*JL: (1Z[i,j,k,l] + 2Z[i,j,k,l]) <= 1;
subto con2t: forall <k,l> in KT*LL do
    sum <i,j> in IL*JL: (1T[i,j,k,l] + 2T[i,j,k,l] + 3T[i,j,k,l] + 4T[i,j,k,l]) <= 1;

subto con31l: forall <i,j,k> in { 1 .. (imax - 2) }*{ 1 .. (jmax - 1) }*KL do
    1l[i,j,k,1] + 1l[i+1,j,k,2] + 1l[i+2,j,k,3] + 1l[i+2,j+1,k,4] == CON1L[i,j,k] * 4;
subto con32l: forall <i,j,k> in { 1 .. (imax - 1) }*{ 1 .. (jmax - 2) }*KL do
    2l[i,j,k,1] + 2l[i+1,j,k,2] + 2l[i,j+1,k,3] + 2l[i,j+2,k,4] == CON2L[i,j,k] * 4;
subto con33l: forall <i,j,k> in { 1 .. (imax - 2) }*{ 1 .. (jmax - 1) }*KL do
    3l[i,j,k,1] + 3l[i,j+1,k,2] + 3l[i+1,j+1,k,3] + 3l[i+2,j+1,k,4] == CON3L[i,j,k] * 4;
subto con34l: forall <i,j,k> in { 1 .. (imax - 1) }*{ 1 .. (jmax - 2) }*KL do
    4l[i+1,j,k,1] + 4l[i+1,j+1,k,2] + 4l[i+1,j+2,k,3] + 4l[i,j+2,k,4] == CON4L[i,j,k] * 4;

subto con31j: forall <i,j,k> in { 1 .. (imax - 1) }*{ 1 .. (jmax - 2) }*KJ do
    1j[i,j,k,1] + 1j[i+1,j,k,2] + 1j[i+1,j+1,k,3] + 1j[i+1,j+2,k,4] == CON1J[i,j,k] * 4;
subto con32j: forall <i,j,k> in { 1 .. (imax - 2) }*{ 1 .. (jmax - 1) }*KJ do
    2j[i,j,k,1] + 2j[i,j+1,k,2] + 2j[i+1,j,k,3] + 2j[i+2,j,k,4] == CON2J[i,j,k] * 4;
subto con33j: forall <i,j,k> in { 1 .. (imax - 1) }*{ 1 .. (jmax - 2) }*KJ do
    3j[i,j,k,1] + 3j[i,j+1,k,2] + 3j[i,j+2,k,3] + 3j[i+1,j+2,k,4] == CON3J[i,j,k] * 4;
subto con34j: forall <i,j,k> in { 1 .. (imax - 2) }*{ 1 .. (jmax - 1) }*KJ do
    4j[i,j+1,k,1] + 4j[i+1,j+1,k,2] + 4j[i+2,j+1,k,3] + 4j[i+2,j,k,4] == CON4J[i,j,k] * 4;

subto con31i: forall <i,j,k> in { 1 .. (imax - 3) }*{ 1 .. jmax }*KI do
    1i[i,j,k,1] + 1i[i+1,j,k,2] + 1i[i+2,j,k,3] + 1i[i+3,j,k,4] == CON1I[i,j,k] * 4;
subto con32i: forall <i,j,k> in { 1 .. imax }*{ 1 .. (jmax - 3) }*KI do
    2i[i,j,k,1] + 2i[i,j+1,k,2] + 2i[i,j+2,k,3] + 2i[i,j+3,k,4] == CON2I[i,j,k] * 4;

subto con31o: forall <i,j,k> in { 1 .. (imax - 1) }*{ 1 .. (jmax - 1) }*KO do
    1o[i,j,k,1] + 1o[i+1,j,k,2] + 1o[i,j+1,k,3] + 1o[i+1,j+1,k,4] == CON1O[i,j,k] * 4;

subto con31s: forall <i,j,k> in { 1 .. (imax - 2) }*{ 1 .. (jmax - 1) }*KS do
    1s[i,j,k,1] + 1s[i+1,j,k,2] + 1s[i+1,j+1,k,3] + 1s[i+2,j+1,k,4] == CON1S[i,j,k] * 4;
subto con32s: forall <i,j,k> in { 1 .. (imax - 1) }*{ 1 .. (jmax - 2) }*KS do
    2s[i+1,j,k,1] + 2s[i+1,j+1,k,2] + 2s[i,j+1,k,3] + 2s[i,j+2,k,4] == CON2S[i,j,k] * 4;

subto con31z: forall <i,j,k> in { 1 .. (imax - 2) }*{ 1 .. (jmax - 1) }*KZ do
    1z[i,j+1,k,1] + 1z[i+1,j+1,k,2] + 1z[i+1,j,k,3] + 1z[i+2,j,k,4] == CON1Z[i,j,k] * 4;
subto con32z: forall <i,j,k> in { 1 .. (imax - 1) }*{ 1 .. (jmax - 2) }*KZ do
    2z[i,j,k,1] + 2z[i,j+1,k,2] + 2z[i+1,j+1,k,3] + 2z[i+1,j+2,k,4] == CON2Z[i,j,k] * 4;

subto con31t: forall <i,j,k> in { 1 .. (imax - 2) }*{ 1 .. (jmax - 1) }*KT do
    1t[i,j+1,k,1] + 1t[i+1,j+1,k,2] + 1t[i+2,j+1,k,3] + 1t[i+1,j,k,4] == CON1T[i,j,k] * 4;
subto con32t: forall <i,j,k> in { 1 .. (imax - 1) }*{ 1 .. (jmax - 2) }*KT do
    2t[i,j,k,1] + 2t[i,j+1,k,2] + 2t[i,j+2,k,3] + 2t[i+1,j+1,k,4] == CON2T[i,j,k] * 4;
subto con33t: forall <i,j,k> in { 1 .. (imax - 2) }*{ 1 .. (jmax - 1) }*KT do
    3t[i,j,k,1] + 3t[i+1,j,k,2] + 3t[i+2,j,k,3] + 3t[i+1,j+1,k,4] == CON3T[i,j,k] * 4;
subto con34t: forall <i,j,k> in { 1 .. (imax - 1) }*{ 1 .. (jmax - 2) }*KT do
    4t[i,j+1,k,1] + 4t[i+1,j,k,2] + 4t[i+1,j+1,k,3] + 4t[i+1,j+2,k,4] == CON4T[i,j,k] * 4;

subto con41l:
subto con42: forall <i,j> in IL*JL do
    sum <k> in KL:
    (
        (sum <icon,jcon> in {1 .. (imax - 2)}*{1 .. (jmax - 1)}: CON1L[icon,jcon,k])
        *
        (sum <l> in LL:1L[i,j,k,l])
        +
        (sum <icon,jcon> in {1 .. (imax - 2)}*{1 .. (jmax - 1)}: CON2L[icon,jcon,k])
        *
        (sum <l> in LL:2L[i,j,k,l])
        +
        (sum <icon,jcon> in {1 .. (imax - 2)}*{1 .. (jmax - 1)}: CON3L[icon,jcon,k])
        *
        (sum <l> in LL:3L[i,j,k,l])
        +
        (sum <icon,jcon> in {1 .. (imax - 2)}*{1 .. (jmax - 1)}: CON4L[icon,jcon,k])
        *
        (sum <l> in LL:4L[i,j,k,l])
    )
    +



subto con51l: forall <i,j,k> in { 1 .. (imax - 2) }*{ 1 .. (jmax - 1) }*KL do
    1l[i,j,k,1] + 1l[i+1,j,k,2] + 1l[i+2,j,k,3] + 1l[i+2,j+1,k,4] == con1l[i,j,k] * 4;
subto con52l: forall <i,j,k> in { 1 .. (imax - 1) }*{ 1 .. (jmax - 2) }*KL do
    2l[i,j,k,1] + 2l[i+1,j,k,2] + 2l[i,j+1,k,3] + 2l[i,j+2,k,4] == con2l[i,j,k] * 4;
subto con53l: forall <i,j,k> in { 1 .. (imax - 2) }*{ 1 .. (jmax - 1) }*KL do
    3l[i,j,k,1] + 3l[i,j+1,k,2] + 3l[i+1,j+1,k,3] + 3l[i+2,j+1,k,4] == con3l[i,j,k] * 4;
subto con54l: forall <i,j,k> in { 1 .. (imax - 1) }*{ 1 .. (jmax - 2) }*KL do
    4l[i+1,j,k,1] + 4l[i+1,j+1,k,2] + 4l[i+1,j+2,k,3] + 4l[i,j+2,k,4] == con5l[i,j,k] * 4;

subto con51j: forall <i,j,k> in { 1 .. (imax - 1) }*{ 1 .. (jmax - 2) }*KJ do
    1j[i,j,k,1] + 1j[i+1,j,k,2] + 1j[i+1,j+1,k,3] + 1j[i+1,j+2,k,4] == con1j[i,j,k] * 4;
subto con52j: forall <i,j,k> in { 1 .. (imax - 2) }*{ 1 .. (jmax - 1) }*KJ do
    2j[i,j,k,1] + 2j[i,j+1,k,2] + 2j[i+1,j,k,3] + 2j[i+2,j,k,4] == con2j[i,j,k] * 4;
subto con53j: forall <i,j,k> in { 1 .. (imax - 1) }*{ 1 .. (jmax - 2) }*KJ do
    3j[i,j,k,1] + 3j[i,j+1,k,2] + 3j[i,j+2,k,3] + 3j[i+1,j+2,k,4] == con3j[i,j,k] * 4;
subto con54j: forall <i,j,k> in { 1 .. (imax - 2) }*{ 1 .. (jmax - 1) }*KJ do
    4j[i,j+1,k,1] + 4j[i+1,j+1,k,2] + 4j[i+2,j+1,k,3] + 4j[i+2,j,k,4] == con4j[i,j,k] * 4;

subto con51i: forall <i,j,k> in { 1 .. (imax - 3) }*{ 1 .. jmax }*KI do
    1i[i,j,k,1] + 1i[i+1,j,k,2] + 1i[i+2,j,k,3] + 1i[i+3,j,k,4] == con1i[i,j,k] * 4;
subto con52i: forall <i,j,k> in { 1 .. imax }*{ 1 .. (jmax - 3) }*KI do
    2i[i,j,k,1] + 2i[i,j+1,k,2] + 2i[i,j+2,k,3] + 2i[i,j+3,k,4] == con2i[i,j,k] * 4;

subto con51o: forall <i,j,k> in { 1 .. (imax - 1) }*{ 1 .. (jmax - 1) }*KO do
    1o[i,j,k,1] + 1o[i+1,j,k,2] + 1o[i,j+1,k,3] + 1o[i+1,j+1,k,4] == con1o[i,j,k] * 4;

subto con51s: forall <i,j,k> in { 1 .. (imax - 2) }*{ 1 .. (jmax - 1) }*KS do
    1s[i,j,k,1] + 1s[i+1,j,k,2] + 1s[i+1,j+1,k,3] + 1s[i+2,j+1,k,4] == con1s[i,j,k] * 4;
subto con52s: forall <i,j,k> in { 1 .. (imax - 1) }*{ 1 .. (jmax - 2) }*KS do
    2s[i+1,j,k,1] + 2s[i+1,j+1,k,2] + 2s[i,j+1,k,3] + 2s[i,j+2,k,4] == con2s[i,j,k] * 4;

subto con51z: forall <i,j,k> in { 1 .. (imax - 2) }*{ 1 .. (jmax - 1) }*KZ do
    1z[i,j+1,k,1] + 1z[i+1,j+1,k,2] + 1z[i+1,j,k,3] + 1z[i+2,j,k,4] == con1z[i,j,k] * 4;
subto con52z: forall <i,j,k> in { 1 .. (imax - 1) }*{ 1 .. (jmax - 2) }*KZ do
    2z[i,j,k,1] + 2z[i,j+1,k,2] + 2z[i+1,j+1,k,3] + 2z[i+1,j+2,k,4] == con2z[i,j,k] * 4;

subto con51t: forall <i,j,k> in { 1 .. (imax - 2) }*{ 1 .. (jmax - 1) }*KT do
    1t[i,j+1,k,1] + 1t[i+1,j+1,k,2] + 1t[i+2,j+1,k,3] + 1t[i+1,j,k,4] == con1t[i,j,k] * 4;
subto con52t: forall <i,j,k> in { 1 .. (imax - 1) }*{ 1 .. (jmax - 2) }*KT do
    2t[i,j,k,1] + 2t[i,j+1,k,2] + 2t[i,j+2,k,3] + 2t[i+1,j+1,k,4] == con2t[i,j,k] * 4;
subto con53t: forall <i,j,k> in { 1 .. (imax - 2) }*{ 1 .. (jmax - 1) }*KT do
    3t[i,j,k,1] + 3t[i+1,j,k,2] + 3t[i+2,j,k,3] + 3t[i+1,j+1,k,4] == con3t[i,j,k] * 4;
subto con54t: forall <i,j,k> in { 1 .. (imax - 1) }*{ 1 .. (jmax - 2) }*KT do
    4t[i,j+1,k,1] + 4t[i+1,j,k,2] + 4t[i+1,j+1,k,3] + 4t[i+1,j+2,k,4] == con4t[i,j,k] * 4;

subto con51: forall <i,j,k> in { 1 .. (imax - 2) }*{ 2 .. (jmax - 1) }*KL1 do
    sum <k,l> in KL1*LL: (N1[i,j-1,k,l] + N1[i+1,j-1,k,l] + N1[i+2,j-1,k,l])
    + sum <k,l> in KL2*LL: (N2[i,j-1,k,l] + N2[i+1,j-1,k,l] + N2[i+2,j-1,k,l])  
    >= CON1[i,j,k];
subto con52: forall <i,j,k> in { 1 .. (imax - 1) }*{ 2 .. (jmax - 2) }*KL2 do
    sum <k,l> in KL1*LL: (N2[i,j-1,k,l] + N2[i+1,j-1,k,l]) 
    + sum <k,l> in KL2*LL: (N2[i,j-1,k,l] + N2[i+1,j-1,k,l]) 
    >= CON2[i,j,k];



subto con5: forall <j> in JL do
    sum <i,k,l> in IL*KL1*LL do (sum <icon,jcon> in { 1 .. (imax - 2) }*{ 1 .. (jmax - 1) }: CON1[icon,jcon,k] * N1[i,j,k,l]) + sum <i,k,l> in IL*KL2*LL do (sum <icon,jcon> in { 1 .. (imax - 1) }*{ 1 .. (jmax - 2) }: CON2[icon,jcon,k] * N2[i,j,k,l]) >= imax * M[j];
