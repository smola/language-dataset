/*****************************************************************************************
* PARTIAL AND POINT IDENTIFICATION RESULTS UNDER INSTRUMENTAL VARIABLE TYPE ASSUMPTIONS  *
* Authors: Sonja Swanson (s.swanson@erasmusmc.nl), Miguel Hernán, Matthew Miller,        *
* James Robins, and Thomas Richardson.                                                   *
*                                                                                        *
* This software provides bounds under various sets of assumptions, for Pr[Y(0)=1],       *
* Pr[Y(1)=1], and ATE as described in                                                    *
* Swanson SA, Hernán MA, Miller M, Robins JM, Richardson T. Partial identification of    *
* the average treatment effect using instrumental variables. JASA 2018; 113(522):933-947 *
*                                                                                        *
* Version 01/26/2018. Latest version in github.com/CausalInference and                   *
* www.hsph.harvard.edu/causal/software/                                                  *    
*                                                                                        *
* Macro inputs:                                                                          *
*      dat = dataset                                                                     *
*      z = proposed instrument (coded 0 vs. 1; drops missing)                            *
*      x = treatment (coded 0 vs. 1; drops missing)                                      *
*      y = outcome (coded 0 vs. 1; drops missing)                                        *
*      prob_DE = proportion of defiers (assuming well-defined)                           *
*      minAT0 = lower bound on outcome for always-treaters under no treatment            *
*      maxAT0 = upper bound on outcome for always-treaters under no treatment            *
*      minNT1 = lower bound on outcome for never-treaters under treatment                *
*      maxNT0 = upper bound on outcome for never-treaters under treatment                *
*                                                                                        *
* Note: nonsensical estimates (e.g., ATE>1 or LB>UB) may indicate the observed data are  *
* not consistent with the assumptions.                                                   *
******************************************************************************************/


/*
Copyright (c) 2014, 2018, The President and Fellows of Harvard College
Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated 
documentation files (the "Software"), to deal in the Software without restriction, including without limitation 
the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, 
and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
The above copyright notice and this permission notice shall be included in all copies or substantial 
portions of the Software.

This software is provided under the standard MIT License:
THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED 
TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL 
THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF 
CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER 
DEALINGS IN THE SOFTWARE.
*/



%macro iv_bounds(dat=,z=,x=,y=,
	prob_DE=0,minAT0=0,minNT1=0,maxAT0=1,maxNT1=1);

options nonotes nomprint errors=0;

data ivdat0;
    set &dat;
    if &z ne .; 
    if &x ne .; 
    if &y ne .;
	newid = _n_;
	keep &z &x &y newid;
    run;

proc sort data=ivdat0; by &z &x; run;

/* Pr[Z=z] */
proc freq data=ivdat0 noprint;
table &z /sparse list missing out=mpz;
run;
data mpz;
set mpz;
prob=0.01*percent;
drop count percent;
run;

/* Pr[Y=y,X=x,Z=z] */
proc freq data=ivdat0 noprint;
table &z*&x*&y /sparse list missing out=mpyxz;
run;
data mpyxz;
set mpyxz;
prob=0.01*percent;
drop count percent;
run;

/* Pr[Y=y,Z=z] */
proc freq data=ivdat0 noprint;
table &z*&y /sparse list missing out=mpyz;
run;
data mpyz;
set mpyz;
prob=0.01*percent;
drop count percent;
run;

/* Pr[X=x,Z=z] */
proc freq data=ivdat0 noprint;
table &z*&x /sparse list missing out=mpxz;
run;
data mpxz;
set mpxz;
prob=0.01*percent;
drop count percent;
run;

/* N of X=x,Z=z */
proc freq data=ivdat0 noprint;
table &z*&x /sparse list missing out=nxz;
run;
data nxz;
set nxz;
drop percent;
run;

proc sort data=ivdat0; by &x; run;

/* Pr[X=x] */
proc freq data=ivdat0 noprint;
table &x /sparse list missing out=mpx;
run;
data mpx;
set mpx;
prob=0.01*percent;
drop count percent;
run;

/* Pr[Y=y,X=x] */
proc freq data=ivdat0 noprint;
table &x*&y /sparse list missing out=mpyx;
run;
data mpyx;
set mpyx;
prob=0.01*percent;
drop count percent;
run;

proc iml;
/* matrix of Pr[Z=z] */
use mpz;
read all into pz;
/* matrix of Pr[X=x] */
use mpx;
read all into px;
/* matrix of Pr[Y=y,X=x,Z=z] */
use mpyxz;
read all into pyxz;
/* matrix of Pr[Y=y,X=x] */
use mpyx;
read all into pyx;
/* matrix of Pr[X=x,Z=z] */
use mpxz;
read all into pxz;
/* matrix of N of [X=x,Z=z] */
use nxz;
read all into nxz;
/* matrix of Pr[Y=y,Z=z] */
use mpyz;
read all into pyz;
/* matrix of Pr[Y=y,X=x|Z=z] */
pyx_z = j(8,4,9999);
pyx_z[1:8,1:3] = pyxz[1:8,1:3];
if sum(pyxz[1:4,4])>0 then pyx_z[1,4] = pyxz[1,4]/sum(pyxz[1:4,4]); else pyx_z[1,4] = 0;
if sum(pyxz[1:4,4])>0 then pyx_z[2,4] = pyxz[2,4]/sum(pyxz[1:4,4]); else pyx_z[2,4] = 0;
if sum(pyxz[1:4,4])>0 then pyx_z[3,4] = pyxz[3,4]/sum(pyxz[1:4,4]); else pyx_z[3,4] = 0;
if sum(pyxz[1:4,4])>0 then pyx_z[4,4] = pyxz[4,4]/sum(pyxz[1:4,4]); else pyx_z[4,4] = 0;
if sum(pyxz[5:8,4])>0 then pyx_z[5,4] = pyxz[5,4]/sum(pyxz[5:8,4]); else pyx_z[5,4] = 0;
if sum(pyxz[5:8,4])>0 then pyx_z[6,4] = pyxz[6,4]/sum(pyxz[5:8,4]); else pyx_z[6,4] = 0;
if sum(pyxz[5:8,4])>0 then pyx_z[7,4] = pyxz[7,4]/sum(pyxz[5:8,4]); else pyx_z[7,4] = 0;
if sum(pyxz[5:8,4])>0 then pyx_z[8,4] = pyxz[8,4]/sum(pyxz[5:8,4]); else pyx_z[8,4] = 0;
/* matrix of Pr[Y=y|X=x,Z=z] */
py_xz = j(8,4,9999);
py_xz[1:8,1:3] = pyxz[1:8,1:3];
if sum(pyxz[1:2,4])>0 then py_xz[1,4] = pyxz[1,4]/sum(pyxz[1:2,4]); else py_xz[1,4] = 0;
if sum(pyxz[1:2,4])>0 then py_xz[2,4] = pyxz[2,4]/sum(pyxz[1:2,4]); else py_xz[2,4] = 0;
if sum(pyxz[3:4,4])>0 then py_xz[3,4] = pyxz[3,4]/sum(pyxz[3:4,4]); else py_xz[3,4] = 0;
if sum(pyxz[3:4,4])>0 then py_xz[4,4] = pyxz[4,4]/sum(pyxz[3:4,4]); else py_xz[4,4] = 0;
if sum(pyxz[5:6,4])>0 then py_xz[5,4] = pyxz[5,4]/sum(pyxz[5:6,4]); else py_xz[5,4] = 0;
if sum(pyxz[5:6,4])>0 then py_xz[6,4] = pyxz[6,4]/sum(pyxz[5:6,4]); else py_xz[6,4] = 0;
if sum(pyxz[7:8,4])>0 then py_xz[7,4] = pyxz[7,4]/sum(pyxz[7:8,4]); else py_xz[7,4] = 0;
if sum(pyxz[7:8,4])>0 then py_xz[8,4] = pyxz[8,4]/sum(pyxz[7:8,4]); else py_xz[8,4] = 0;
/* matrix of Pr[Y=y|Z=z] */
py_z = j(4,3,9999);
py_z[1:4,1:2] = pyz[1:4,1:2];
if sum(pyz[1:2,3])>0 then py_z[1,3] = pyz[1,3]/sum(pyz[1:2,3]); else py_z[1,3] = 0;
if sum(pyz[1:2,3])>0 then py_z[2,3] = pyz[2,3]/sum(pyz[1:2,3]); else py_z[2,3] = 0;
if sum(pyz[3:4,3])>0 then py_z[3,3] = pyz[3,3]/sum(pyz[3:4,3]); else py_z[3,3] = 0;
if sum(pyz[3:4,3])>0 then py_z[4,3] = pyz[4,3]/sum(pyz[3:4,3]); else py_z[4,3] = 0;
/* matrix of Pr[X=x|Z=z] */
px_z = j(4,3,9999);
px_z[1:4,1:2] = pxz[1:4,1:2];
if sum(pxz[1:2,3])>0 then px_z[1,3] = pxz[1,3]/sum(pxz[1:2,3]); else px_z[1,3] = 0;
if sum(pxz[1:2,3])>0 then px_z[2,3] = pxz[2,3]/sum(pxz[1:2,3]); else px_z[2,3] = 0;
if sum(pxz[3:4,3])>0 then px_z[3,3] = pxz[3,3]/sum(pxz[3:4,3]); else px_z[3,3] = 0;
if sum(pxz[3:4,3])>0 then px_z[4,3] = pxz[4,3]/sum(pxz[3:4,3]); else px_z[4,3] = 0;
/* matrix of Pr[Y=y|X=x] */
py_x = j(4,3,9999);
py_x[1:4,1:2] = pyx[1:4,1:2];
if sum(pyx[1:2,3])>0 then py_x[1,3] = pyx[1,3]/sum(pyx[1:2,3]); else py_x[1,3] = 0;
if sum(pyx[1:2,3])>0 then py_x[2,3] = pyx[2,3]/sum(pyx[1:2,3]); else py_x[2,3] = 0;
if sum(pyx[3:4,3])>0 then py_x[3,3] = pyx[3,3]/sum(pyx[3:4,3]); else py_x[3,3] = 0;
if sum(pyx[3:4,3])>0 then py_x[4,3] = pyx[4,3]/sum(pyx[3:4,3]); else py_x[4,3] = 0;

/* create empty vectors for output */
LB_Y0=j(9,1,9999);
LB_Y1=j(9,1,9999);
UB_Y0=j(9,1,9999);
UB_Y1=j(9,1,9999);
LB_ATE=j(9,1,9999);
UB_ATE=j(9,1,9999);
LB_RR=j(9,1,9999);
UB_RR=j(9,1,9999);

/* 
No IV assumptions (finite bounding): 
	Robins (1989), Manski (1990)
*/
LB_Y0[1,1] = py_x[2,3]*px[1,2]; 
UB_Y0[1,1] = py_x[2,3]*px[1,2] + 1*px[2,2];

LB_Y1[1,1] = py_x[4,3]*px[2,2];
UB_Y1[1,1] = py_x[4,3]*px[2,2] + 1*px[1,2];

LB_ATE[1,1] = LB_Y1[1,1] - UB_Y0[1,1];
UB_ATE[1,1] = UB_Y1[1,1] - LB_Y0[1,1];

LB_RR[1,1] = LB_Y1[1,1]/UB_Y0[1,1];
UB_RR[1,1] = UB_Y1[1,1]/LB_Y0[1,1];

/* 
A1+A2
Assume exclusion restriction and marginal exchangeability:
	Robins (1989), Manski (1990)
*/

LB_Y0[2,1] = max(py_xz[2,4]*px_z[1,3],
				py_xz[6,4]*px_z[3,3]);
UB_Y0[2,1] = min(py_xz[2,4]*px_z[1,3] + px_z[2,3],
				py_xz[6,4]*px_z[3,3] + px_z[4,3]);

LB_Y1[2,1] = max(py_xz[4,4]*px_z[2,3],
				py_xz[8,4]*px_z[4,3]);
UB_Y1[2,1] = min(py_xz[4,4]*px_z[2,3] + px_z[1,3],
				py_xz[8,4]*px_z[4,3] + px_z[3,3]);

LB_ATE[2,1] = LB_Y1[2,1]-UB_Y0[2,1];
UB_ATE[2,1] = UB_Y1[2,1]-LB_Y0[2,1];

LB_RR[2,1] = LB_Y1[2,1]/UB_Y0[2,1];
UB_RR[2,1] = UB_Y1[2,1]/LB_Y0[2,1];

/*
A3+A4
Assume exclusion restriction and partial or full joint exchangeability:
	Balke and Pearl (1997)
	Richardson and Robins (2014)
*/

LB_Y0[3,1] = max(pyx_z[2,4]+pyx_z[4,4]-pyx_z[5,4]-pyx_z[8,4],
				pyx_z[6,4],
				pyx_z[2,4],
				pyx_z[3,4]+pyx_z[2,4]-pyx_z[5,4]-pyx_z[7,4]);
UB_Y0[3,1] = min(pyx_z[3,4]+pyx_z[2,4]+pyx_z[6,4]+pyx_z[8,4],
				1-pyx_z[5,4],
				1-pyx_z[1,4],
				pyx_z[2,4]+pyx_z[4,4]+pyx_z[7,4]+pyx_z[6,4]);

LB_Y1[3,1] = max(pyx_z[4,4],
				pyx_z[8,4],
				-pyx_z[1,4]-pyx_z[3,4]+pyx_z[5,4]+pyx_z[8,4],
				-pyx_z[3,4]-pyx_z[2,4]+pyx_z[6,4]+pyx_z[8,4]);
UB_Y1[3,1] = min(1-pyx_z[3,4],
				1-pyx_z[7,4],
				pyx_z[1,4]+pyx_z[4,4]+pyx_z[6,4]+pyx_z[8,4],
				pyx_z[2,4]+pyx_z[4,4]+pyx_z[5,4]+pyx_z[8,4]);

LB_ATE[3,1] = max(pyx_z[8,4]+pyx_z[1,4]-1,
				pyx_z[4,4]+pyx_z[5,4]-1,
				pyx_z[4,4]-pyx_z[8,4]-pyx_z[6,4]-pyx_z[3,4]-pyx_z[2,4],
				pyx_z[8,4]-pyx_z[4,4]-pyx_z[2,4]-pyx_z[7,4]-pyx_z[6,4],
				-pyx_z[7,4]-pyx_z[6,4],
				-pyx_z[3,4]-pyx_z[2,4],
				pyx_z[5,4]-pyx_z[7,4]-pyx_z[6,4]-pyx_z[3,4]-pyx_z[1,4],
				pyx_z[1,4]-pyx_z[3,4]-pyx_z[2,4]-pyx_z[7,4]-pyx_z[5,4]);
UB_ATE[3,1] = min(1-pyx_z[7,4]-pyx_z[2,4],
				1-pyx_z[3,4]-pyx_z[6,4],
				-pyx_z[3,4]+pyx_z[7,4]+pyx_z[5,4]+pyx_z[4,4]+pyx_z[1,4],
				-pyx_z[7,4]+pyx_z[3,4]+pyx_z[1,4]+pyx_z[8,4]+pyx_z[5,4],
				pyx_z[8,4]+pyx_z[5,4],
				pyx_z[4,4]+pyx_z[1,4],
				-pyx_z[6,4]+pyx_z[8,4]+pyx_z[5,4]+pyx_z[4,4]+pyx_z[2,4],
				-pyx_z[2,4]+pyx_z[4,4]+pyx_z[1,4]+pyx_z[8,4]+pyx_z[6,4]);

LB_RR[3,1] = LB_Y1[3,1]/UB_Y0[3,1];
UB_RR[3,1] = UB_Y1[3,1]/LB_Y0[3,1];

/* 
A5+A12+A13(+A14)
Assume exclusion restriction and joint independence for a specified compliance distribution:
		Richardson and Robins (2010)

	Note: compliance distribution must be compatible with the data
*/

*probability of each compliance type;
probDE = &prob_DE;
probAT = px_z[2,3] - probDE;
probCO = px_z[4,3] - probAT;
probNT = 1 - probDE - probCO - probAT;

*need the distribution to be compatible;
if probDE>=0 & probDE<=1 & probCO>=0 & probCO<=1 &
	probAT>=0 & probAT<=1 & probNT>=0 & probNT<=1 then do;

*bounds for counterfactuals and ATE for NT;
if probNT = 0 then do;
Y0_NT_LB = 0;
Y0_NT_UB = 1;
Y1_NT_LB = 0;
Y1_NT_UB = 1;
end;
else do;
Y0_NT_LB = max(0,
	(py_xz[2,4] - (probCO/(probCO+probNT)))/(probNT/(probCO+probNT)),
	(py_xz[6,4] - (probDE/(probDE+probNT)))/(probNT/(probDE+probNT)));
Y0_NT_UB = min(1,
	py_xz[2,4]/(probNT/(probNT+probCO)),
	py_xz[6,4]/(probNT/(probNT+probDE)));
Y1_NT_LB = 0;
Y1_NT_UB = 1;
end;
Y1_NT_LB_sens = &minNT1;
Y1_NT_UB_sens = &maxNT1;
ATE_NT_LB = Y1_NT_LB - Y0_NT_UB;
ATE_NT_UB = Y1_NT_UB - Y0_NT_LB;
ATE_NT_LB_sens = Y1_NT_LB_sens - Y0_NT_UB;
ATE_NT_UB_sens = Y1_NT_UB_sens - Y0_NT_LB;
RR_NT_LB = Y1_NT_LB / Y0_NT_UB;
RR_NT_UB = Y1_NT_UB / Y0_NT_LB;
RR_NT_LB_sens = Y1_NT_LB_sens / Y0_NT_UB;
RR_NT_UB_sens = Y1_NT_UB_sens / Y0_NT_LB;

*bounds for counterfactuals and ATE for AT;
if probAT = 0 then do;
Y0_AT_LB = 0;
Y0_AT_UB = 1;
Y1_AT_LB = 0;
Y1_AT_UB = 1;
end;
else do;
Y0_AT_LB = 0;
Y0_AT_UB = 1;
Y1_AT_LB = max(0,
	(py_xz[8,4]-(probCO/(probCO+probAT)))/(probAT/(probCO+probAT)),
	(py_xz[4,4]-(probDE/(probDE+probAT)))/(probAT/(probDE+probAT)));
Y1_AT_UB = min(1,
	py_xz[8,4]/(probAT/(probCO+probAT)),
	py_xz[4,4]/(probAT/(probDE+probAT)));
end;
Y0_AT_LB_sens = &minAT0;
Y0_AT_UB_sens = &maxAT0;
ATE_AT_LB = Y1_AT_LB - Y0_AT_UB;
ATE_AT_UB = Y1_AT_UB - Y0_AT_LB;
ATE_AT_LB_sens = Y1_AT_LB - Y0_AT_UB_sens;
ATE_AT_UB_sens = Y1_AT_UB - Y0_AT_LB_sens;
RR_AT_LB = Y1_AT_LB / Y0_AT_UB;
RR_AT_UB = Y1_AT_UB / Y0_AT_LB;
RR_AT_LB_sens = Y1_AT_LB / Y0_AT_UB_sens;
RR_AT_UB_sens = Y1_AT_UB / Y0_AT_LB_sens;

*bounds for counterfactuals and ATE for CO;
if probCO = 0 then do;
Y0_CO_LB = 0;
Y0_CO_UB = 1;
Y1_CO_LB = 0;
Y1_CO_UB = 1;
end;
else do;
Y0_CO_LB = (py_xz[2,4] - Y0_NT_UB*(probNT/(probNT+probCO)))/(probCO/(probNT+probCO));
Y0_CO_UB = (py_xz[2,4] - Y0_NT_LB*(probNT/(probNT+probCO)))/(probCO/(probNT+probCO));
Y1_CO_LB = (py_xz[8,4] - Y1_AT_UB*(probAT/(probAT+probCO)))/(probCO/(probAT+probCO));
Y1_CO_UB = (py_xz[8,4] - Y1_AT_LB*(probAT/(probAT+probCO)))/(probCO/(probAT+probCO));
end;
ATE_CO_LB = Y1_CO_LB - Y0_CO_UB;
ATE_CO_UB = Y1_CO_UB - Y0_CO_LB;
RR_CO_LB = Y1_CO_LB / Y0_CO_UB;
RR_CO_UB = Y1_CO_UB / Y0_CO_LB;

*bounds for counterfactuals and ATE for DE;
if probDE = 0 then do;
Y0_DE_LB = 0;
Y0_DE_UB = 1;
Y1_DE_LB = 0;
Y1_DE_UB = 1;
end;
else do;
Y0_DE_LB = (py_xz[6,4] - Y0_NT_UB*(probNT/(probNT+probDE)))/(probDE/(probNT+probDE));
Y0_DE_UB = (py_xz[6,4] - Y0_NT_LB*(probNT/(probNT+probDE)))/(probDE/(probNT+probDE));
Y1_DE_LB = (py_xz[4,4] - Y1_AT_UB*(probAT/(probAT+probDE)))/(probDE/(probAT+probDE));
Y1_DE_UB = (py_xz[4,4] - Y1_AT_LB*(probAT/(probAT+probDE)))/(probDE/(probAT+probDE));
end;
ATE_DE_LB = Y1_DE_LB - Y0_DE_UB;
ATE_DE_UB = Y1_DE_UB - Y0_DE_LB;
RR_DE_LB = Y1_DE_LB / Y0_DE_UB;
RR_DE_UB = Y1_DE_UB / Y0_DE_LB;

*global bounds under these assumptions;
LB_global_Y0 = Y0_DE_LB*probDE + Y0_AT_LB*probAT + pyx_z[2,4];
UB_global_Y0 = Y0_DE_UB*probDE + Y0_AT_UB*probAT + pyx_z[2,4];
LB_global_Y1 = Y1_DE_LB*probDE + Y1_NT_LB*probNT + pyx_z[8,4];
UB_global_Y1 = Y1_DE_UB*probDE + Y1_NT_UB*probNT + pyx_z[8,4];
ATE_global_LB = pyx_z[8,4] - pyx_z[2,4] + probDE*ATE_DE_LB - probAT;
ATE_global_UB = pyx_z[8,4] - pyx_z[2,4] + probDE*ATE_DE_UB + probNT;
RR_global_LB = LB_global_Y1 / UB_global_Y0;
RR_global_UB = UB_global_Y1 / LB_global_Y0;

LB_globalsens_Y0 = Y0_DE_LB*probDE + Y0_AT_LB_sens*probAT + pyx_z[2,4];
UB_globalsens_Y0 = Y0_DE_UB*probDE + Y0_AT_UB_sens*probAT + pyx_z[2,4];
LB_globalsens_Y1 = Y1_DE_LB*probDE + Y1_NT_LB_sens*probNT + pyx_z[8,4];
UB_globalsens_Y1 = Y1_DE_UB*probDE + Y1_NT_UB_sens*probNT + pyx_z[8,4];
ATE_globalsens_LB = pyx_z[8,4] - pyx_z[2,4] + probDE*ATE_DE_LB + probNT*Y1_NT_LB_sens - probAT*Y0_AT_UB_sens;
ATE_globalsens_UB = pyx_z[8,4] - pyx_z[2,4] + probDE*ATE_DE_LB + probNT*Y1_NT_UB_sens - probAT*Y0_AT_LB_sens;
RR_globalsens_LB = LB_globalsens_Y1 / UB_globalsens_Y0;
RR_globalsens_UB = UB_globalsens_Y1 / LB_globalsens_Y0;

*summary matrix of bounds for each compliance type;
Compl = J(6,9,9999);
Compl[1,] = probDE || Y0_DE_LB || Y0_DE_UB || Y1_DE_LB || Y1_DE_UB || ATE_DE_LB || ATE_DE_UB || RR_DE_LB || RR_DE_UB;
Compl[2,] = probCO || Y0_CO_LB || Y0_CO_UB || Y1_CO_LB || Y1_CO_UB || ATE_CO_LB || ATE_CO_UB || RR_CO_LB || RR_CO_UB;
Compl[3,] = probAT || Y0_AT_LB || Y0_AT_UB || Y1_AT_LB || Y1_AT_UB || ATE_AT_LB || ATE_AT_UB || RR_AT_LB || RR_AT_UB;
Compl[4,] = probNT || Y0_NT_LB || Y0_NT_UB || Y1_NT_LB || Y1_NT_UB || ATE_NT_LB || ATE_NT_UB || RR_NT_LB || RR_NT_UB;
Compl[5,] = 1 || LB_global_Y0 || UB_global_Y0 || LB_global_Y1 || UB_global_Y1 || ATE_global_LB || ATE_global_UB  || RR_global_LB || RR_global_UB;
Compl[6,] = 
	1 || LB_globalsens_Y0 || UB_globalsens_Y0 || LB_globalsens_Y1 || UB_globalsens_Y1
		|| ATE_globalsens_LB || ATE_globalsens_UB  || RR_globalsens_LB || RR_globalsens_UB;


end;
else do;
Compl = j(6,6,9999);
end;


/* 
A1+A2+A15
Assume exclusion restriction, marginal exchangeability, and treatment weakly decreases
    the outcome:
        Manski and Pepper (2000), Bhattacharya et al 2008
*/

LB_ATE[5,1] = LB_ATE[2,1];
	*max(pyx_z[4,4],pyx_z[8,4]) - min(pyx_z[2,4]+px_z[2,3],pyx_z[6,4]+px_z[4,3]);
UB_ATE[5,1] = -ABS(py_z[4,3] - py_z[2,3]);



/* A1+A2+A16 
Assume exclusion restriction, marginal exchangeability, and Roy model:
		Siddique (2013)
*/

LB_Y0[6,1] = max(py_xz[2,4]*px_z[1,3]+py_xz[4,4]*px_z[2,3],
				py_xz[6,4]*px_z[3,3]);
UB_Y0[6,1] = UB_Y0[2,1];

LB_Y1[6,1] = max(py_xz[8,4]*px_z[4,3]+py_xz[6,4]*px_z[3,3],
				py_xz[4,4]*px_z[2,3]);
UB_Y1[6,1] = UB_Y1[2,1];

LB_ATE[6,1] = LB_Y1[6,1]-UB_Y0[6,1];
UB_ATE[6,1] = UB_Y1[6,1]-LB_Y0[6,1];

LB_RR[6,1] = LB_Y1[6,1]/UB_Y0[6,1];
UB_RR[6,1] = UB_Y1[6,1]/LB_Y0[6,1];

/* 
A1+A2+A17
Assume exclusion restriction, marginal exchangeability, and no additive effect modication
	among treated/untreated by z:
		Robins (1989), Angrist, Imbens, and Rubin (1996)

	Note: 'standard IV estimator' also estimates the effect in the compliers under
    a monotonicity assumption, exclusion restriction, and marginal exchangeability
    
    Note: A7+A8+A9+(A19orA20) also leads to this expression 
*/

LB_ATE[8,1] = (py_z[4,3]-py_z[2,3])/(px_z[4,3]-px_z[2,3]);
UB_ATE[8,1] = LB_ATE[8,1];

/* 
A1+A2+A18
Assume exclusion restriction, marginal exchangeability, and no multiplicative effect modication
	among treated/untreated by z:
		Hernan and Robins (2006)
		
	Note: see erratum to HR 2006 paper for correct expression for the ATE	
*/

expnpsi = 1 - 
	(py_z[4,3]-py_z[2,3])/(pyx_z[8,4] - pyx_z[4,4]);
exppsi = 1/expnpsi;

LB_Y0[9,1] = py_x[2,3]*(1-px[2,2]) + py_x[4,3]*px[2,2]*expnpsi;
UB_Y0[9,1] = LB_Y0[9,1];

LB_Y1[9,1] = py_x[2,3]*(1-px[2,2])*exppsi + py_x[4,3]*px[2,2];
UB_Y1[9,1] = LB_Y1[9,1];


LB_ATE[9,1] = py_x[2,3]*(1-px[2,2])*(exppsi-1) + py_x[4,3]*px[2,2]*(1-expnpsi);
UB_ATE[9,1] = LB_ATE[9,1];

LB_RR[9,1] = exppsi;
UB_RR[9,1] = LB_RR[9,1];

/* create datasets with summary information */

*data distribution;
data_dist=j(4,4,9999);
data_dist[,1] = nxz[1:4,3];
data_dist[,2] = {0,0,1,1};
data_dist[,3] = {0,1,0,1};
data_dist[1:4,4] = py_xz[{2 4 6 8},4];
cname={'N','Z','X','Y'};
create datadist from data_dist[colname=cname];
append from data_dist;
*Y0;
Y0_est = LB_Y0 || UB_Y0;
cname={'LB','UB'};
create Y0est from Y0_est[colname=cname];
append from Y0_est;
*Y1;
Y1_est = LB_Y1 || UB_Y1;
cname={'LB','UB'};
create Y1est from Y1_est[colname=cname];
append from Y1_est;
*ATE;
ATE_est = LB_ATE || UB_ATE;
cname={'LB','UB'};
create ATEest from ATE_est[colname=cname];
append from ATE_est;
*RR;
RR_est = LB_RR || UB_RR;
cname={'LB','UB'};
create RRest from RR_est[colname=cname];
append from RR_est;
*by compliance types;
Compl_est = Compl;
cname={'Proportion','Y0_LB','Y0_UB','Y1_LB','Y1_UB','ATE_LB','ATE_UB','RR_LB','RR_UB'};
create Complest from Compl_est[colname=cname];
append from Compl_est;
*labels;
assume = {'None',
		'A1+A2 (Natural)',
		'A3+A4 (Balke-Pearl)','',
		'A1+A2+A15',
		'A1+A2+A16','',
		'A1+A2+A17',
		'A1+A2+A18'
		};
cname={'Assumptions'};
create Assumptions from assume[colname=cname];
append from assume;
CTypes = {'Defier','Complier','Always Taker','Never Taker','Study Population','Study Population with Bounded AT0 and NT1'};
cname={'Compliance Type'};
create Types from CTypes[colname=cname];
append from Ctypes;


quit;

/* Summary information */
data Y0summary;
merge Assumptions Y0est;
Parameter = 'Pr[Y(0)=1]';
if LB=8888 or UB=8888 then delete;
if LB=9999 or UB=9999 then delete;
run;
data Y1summary;
merge Assumptions Y1est;
Parameter = 'Pr[Y(1)=1]';
if LB=8888 or UB=8888 then delete;
if LB=9999 or UB=9999 then delete;
run;
data Yxsummary;
set Y0summary Y1summary;
run;
data ATEsummary;
merge Assumptions ATEest;
if LB=8888 or UB=8888 then delete;
if LB=9999 or UB=9999 then delete;
run;
data RRsummary;
merge Assumptions RRest;
if LB=8888 or UB=8888 then delete;
if LB=9999 or UB=9999 then delete;
run;
data Complsummary;
merge Types Complest;
run;

proc print data=datadist;
title 'Data Distribution';
footnote 'N and distribution of Pr[Y=1|X,Z] in the study with complete observation of Z, X, and Y';
run;

proc print data=Yxsummary;
var Assumptions Parameter LB UB;
title 'Partial Identification of Pr[Y(x)=1]';
footnote 'See paper for descriptions of assumptions';
run;

proc print data=ATEsummary;
var Assumptions LB UB;
title 'Partial Identification of ATE, Pr[Y(1)=1]-Pr[Y(0)=1]';
footnote 'See paper for descriptions of assumptions';
run;

proc print data=RRsummary;
var Assumptions LB UB;
title 'Partial Identification of RR, Pr[Y(1)=1]/Pr[Y(0)=1]';
footnote 'See paper for descriptions of assumptions';
run;

proc print data=Complsummary;
title 'Partial Identification of Pr[Y(0)=1], Pr[Y(1)=1], ATE, and RR by Compliance Type';
footnote 'Assumes A5+A12+A13(+A14).
		9999 indicates % defiers was incompatible with observed data and assumptions.';
run;

proc datasets library = work nolist ;
delete assumptions ivdat ivdat0
	mpx mpx_z mpyx_z mpy_x
	mpy_xz mpy_z mpz mpxz mpyx
	mpyxz mpyz
	Complest Complsummary Types
	Yxsummary
	Y0est Y0bootsummary Y0summary 
	Y1est Y1bootsummary Y1summary 
	ATEest ATEbootsummary ATEsummary ;
quit;

goptions reset=global;
%mend;
