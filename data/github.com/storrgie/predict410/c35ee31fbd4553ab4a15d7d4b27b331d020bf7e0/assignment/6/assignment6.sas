* William T. Mickelson ;
* 5-5-2015  ;
* portfolio_pca.sas;

libname mydata "/scs/wtm926/" access=readonly;

data temp;
  set mydata.stock_portfolio_data;
run;

proc print data=temp(obs=10); run; quit;
proc sort data=temp; by date; run; quit;

data temp;
  set temp;

  * Compute the log-returns - log of the ratio of today's price to yesterday's price;
  * Note that the data needs to be sorted in the correct 
    direction in order for us to compute the correct return;
  return_AA  = log(AA/lag1(AA));
  return_BAC = log(BAC/lag1(BAC));
  return_BHI = log(BHI/lag1(BHI));
  return_CVX = log(CVX/lag1(CVX));
  return_DD  = log(DD/lag1(DD));
  return_DOW = log(DOW/lag1(DOW));
  return_DPS = log(DPS/lag1(DPS));
  return_GS  = log(GS/lag1(GS));
  return_HAL = log(HAL/lag1(HAL));
  return_HES = log(HES/lag1(HES));
  return_HON = log(HON/lag1(HON));
  return_HUN = log(HUN/lag1(HUN));
  return_JPM = log(JPM/lag1(JPM));
  return_KO  = log(KO/lag1(KO));
  return_MMM = log(MMM/lag1(MMM));
  return_MPC = log(MPC/lag1(MPC));
  return_PEP = log(PEP/lag1(PEP));
  return_SLB = log(SLB/lag1(SLB));
  return_WFC = log(WFC/lag1(WFC));
  return_XOM = log(XOM/lag1(XOM));
  *return_VV  = log(VV/lag1(VV));
  response_VV = log(VV/lag1(VV));
run;

proc print data=temp(obs=10); run; quit;

* We can use ODS TRACE to print out all of the data sets available to ODS for a particular SAS procedure.;
* We can also look these data sets up in the SAS User's Guide in the chapter for the selected procedure.;
*ods trace on;
ods output PearsonCorr=portfolio_correlations;
proc corr data=temp;
*var return: with response_VV;
var return_:;
with response_VV;
run; quit;
*ods trace off;

proc print data=portfolio_correlations; run; quit;


data wide_correlations;
  set portfolio_correlations (keep=return_:);
run;


* Note that wide_correlations is a 'wide' data set and we need a 'long' data set;
* SAS has two 'standard' data formats - wide and long;
* We can use PROC TRANSPOSE to convert data from one format to the other;
proc transpose data=wide_correlations out=long_correlations;
run; quit;

data long_correlations;
  set long_correlations;
  tkr = substr(_NAME_,8,3);
  drop _NAME_;
  rename COL1=correlation;
run;

proc print data=long_correlations; run; quit;



* Merge on sector id and make a colored bar plot;
data sector;
input tkr $ 1-3 sector $ 4-35;
datalines;
AA  Industrial - Metals
BAC Banking
BHI Oil Field Services
CVX Oil Refining
DD  Industrial - Chemical
DOW Industrial - Chemical
DPS Soft Drinks
GS  Banking
HAL Oil Field Services
HES Oil Refining
HON Manufacturing
HUN Industrial - Chemical
JPM Banking
KO  Soft Drinks
MMM Manufacturing
MPC Oil Refining
PEP Soft Drinks
SLB Oil Field Services
WFC Banking
XOM Oil Refining
VV  Market Index
;
run;


proc print data=sector; run; quit;

proc sort data=sector; by tkr; run;
proc sort data=long_correlations; by tkr; run;

data long_correlations;
  merge long_correlations (in=a) sector (in=b);
  by tkr;
  if (a=1) and (b=1);
run;

proc print data=long_correlations; run; quit;


* Make Grouped Bar Plot;
* p. 48 Statistical Graphics Procedures By Example;
ods graphics on;
title 'Correlations with the Market Index';
proc sgplot data=long_correlations;
  format correlation 3.2;
  vbar tkr / response=correlation group=sector groupdisplay=cluster datalabel;
run; quit;
ods graphics off;


* Still not the correct graphic.  We want the tickers grouped and color coded by sector;
* We want ticker labels directly under the x-axis and sector labels under the ticker
  labels denoting each group.  Looks like we have an open SAS graphics project.;
ods graphics on;
title 'Correlations with the Market Index';
proc sgplot data=long_correlations;
  format correlation 3.2;
  vbar sector / response=correlation group=tkr groupdisplay=cluster datalabel;
run; quit;
ods graphics off;



* SAS can produce bar plots by sector of the mean correlation;
proc means data=long_correlations nway noprint;
class sector;
var correlation;
output out=mean_correlation mean(correlation)=mean_correlation;
run; quit;

proc print data=mean_correlation; run;


ods graphics on;
title 'Mean Correlations with the Market Index by Sector';
proc sgplot data=mean_correlation;
  format mean_correlation 3.2;
  vbar sector / response=mean_correlation datalabel;
run; quit;
ods graphics off;


* Note that we have been using PROC SGPLOT to display a data summary, and hence we have not
  been able to make the display that we want.  In reality PROC SGPLOT is designed to take an
  input data set, perform some routine data summaries, and display that output.  Unfortunately,
  routine data summaries are typically frequency counts for discrete data or averages for
  contiuous data.  Here is an example of the default use of PROC SGPLOT.;
ods graphics on;
title 'Mean Correlations with the Market Index by Sector - SGPLOT COMPUTES MEANS';
proc sgplot data=long_correlations;
  format correlation 3.2;
  vbar sector / response=correlation stat=mean datalabel;
run; quit;
ods graphics off;

* Reset title statement to blank;
title '';

************************************************************************************;
* Begin Modeling;
************************************************************************************;
* Note that we do not want the response variable in the data used to compute the
  principal components;

data return_data;
  set temp (keep= return_:);
  * What happens when I put this keep statement in the set statement?;
  * Look it up in The Little SAS Book;
run;

proc print data=return_data(obs=10); run;


************************************************************************************;
* Compute Principal Components;
************************************************************************************;
ods graphics on;
proc princomp data=return_data out=pca_output outstat=eigenvectors plots=scree(unpackpanel);
run; quit;
ods graphics off;
* Notice that PROC PRINCOMP produces a lot of output;
* How many principal components should we keep?;
* Do the principal components have any interpretability?;
* Can we display that interpretability using graphics?;

proc print data=pca_output(obs=10); run;

proc print data=eigenvectors(where=(_TYPE_='SCORE')); run;
* Display the two plots and the Eigenvalue table from the output;



* Plot the first two eigenvectors;
data pca2;
  set eigenvectors(where=(_NAME_ in ('Prin1','Prin2')));
  drop _TYPE_ ;
run;

proc print data=pca2; run;

proc transpose data=pca2 out=long_pca; run; quit;
proc print data=long_pca; run;

data long_pca;
  set long_pca;
  format tkr $3.;
  tkr = substr(_NAME_,8,3);
  drop _NAME_;
run;

proc print data=long_pca; run;

* Plot the first two principal components;
ods graphics on;
proc sgplot data=long_pca;
scatter x=Prin1 y=Prin2 / datalabel=tkr;
run; quit;
ods graphics off;



************************************************************************************;
* Create a training data set and a testing data set from the PCA output             ;
* Note that we will use a SAS shortcut to keep both of these 'datasets' in one      ;
* data set that we will call cv_data (cross-validation data).                       ;
************************************************************************************;
data cv_data;
  merge pca_output temp(keep=response_VV);
  * No BY statement needed here.  We are going to append a column in its current order;
  * generate a uniform(0,1) random variable with seed set to 123;
  u = uniform(123);
  if (u < 0.70) then train = 1;
  else train = 0;

  if (train=1) then train_response=response_VV;
  else train_response=.;

run;

proc print data=cv_data(obs=10); run;

* You can double check this merge by printing out the original data;
proc print data=temp(keep=response_VV obs=10); run; quit;



********************************************************************************************;
* Fit a regression model using the raw predictor variables;
********************************************************************************************;
* Fit a regression model using all of the raw predictor variables and VV as the response variable;

ods graphics on;
proc reg data=cv_data;
model train_response = return_: / vif ;
output out=model1_output predicted=Yhat ;
run; quit;
ods graphics off;
* Examine the Goodness-Of-Fit for this model.  How well does it fit?  Are there any problems?;


********************************************************************************************;
* Fit a regression model using the rotated predictor variables (Principal Component Scores) ;
********************************************************************************************;
* Now fit a regression model using your selected number of principal components and VV as
  the response variable;
* Examine the Goodness-Of-Fit for this model.  How well does it fit?  Are there any problems?;

ods graphics on;
proc reg data=cv_data;
model train_response = prin1-prin8 / vif ;
output out=model2_output predicted=Yhat  ;
run; quit;
ods graphics off;


********************************************************************************************;
* Compare fit and predictive accuracy of the two fitted models ;
********************************************************************************************;
* Use the Mean Square Error (MSE) and the Mean Absolute Error (MAE) metrics for your comparison.;

proc print data=model1_output(obs=10); run;

* Model 1;
data model1_output;
  set model1_output;
  square_error = (response_VV - Yhat)**2;
  absolute_error = abs(response_VV - Yhat);
run;

proc means data=model1_output nway noprint;
class train;
var square_error absolute_error;
output out=model1_error
  mean(square_error)=MSE_1
  mean(absolute_error)=MAE_1;
run; quit;

proc print data=model1_error; run;


* Model 2;
data model2_output;
  set model2_output;
  square_error = (response_VV - Yhat)**2;
  absolute_error = abs(response_VV - Yhat);
run;

proc means data=model2_output nway noprint;
class train;
var square_error absolute_error;
output out=model2_error
  mean(square_error)=MSE_2
  mean(absolute_error)=MAE_2;
run; quit;

proc print data=model2_error; run;


* Merge them together in one table;
data error_table;
  merge model1_error(drop= _TYPE_ _FREQ_) model2_error(drop= _TYPE_ _FREQ_);
  by train;
run;

proc print data=error_table; run;
* Which model fits better? Did we benefit from using PCA? ;
* In our analysis did we do anything that might not be 100% kosher? ;
* Model 2 has higher accuracy both in-sample and out-of-sample, hence we prefer Model 2;