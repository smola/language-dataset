/* Chapter 4 */

libname mozart "/courses/dc4508e5ba27fe300/c_629/saslib" access=readonly;

* effect of the length statement;

data test_scores;
   * length Z_ID $3 Name $15;
    input Z_ID Score1-Score3 Name $;
datalines;
1 90 95 98 larry
2 78 77 75 moe
3 88 91 92 curly
run;

data test_scores;
    * length Z_ID $3 Name $15;
    input Z_ID Score1-Score3 Name $;
datalines;
1 90 95 98 larry
2 78 77 75 moe
3 88 91 92 curly
3 88 91 92 Supercalifragilisticexpialidocious
;
run;

/* 4.4 Examining the Descriptor Portion of a SAS Data Set Using PROC CONTENTS */
*Program 4-2 Using PROC CONTENTS to examine the descriptor portion of a SAS data set - page 56;
title "The Descriptor Portion of Data Set TEST_SCORES";
proc contents data=Mozart.test_scores;
run;

*Program 4-3 Demonstrating the VARNUM option of PROC CONTENTS - page 58;
title "The Descriptor Portion of Data Set TEST_SCORES - with VARNUM option";
proc contents data=Mozart.test_scores varnum;
run;
title Adding the position option;
proc contents data=Mozart.test_scores position;
run;

*Program 4-4 Using a LIBNAME in a new SAS session - page 58;
libname proj99 "/courses/dc4508e5ba27fe300/c_629/saslib" access=readonly;

title "Descriptor Portion of Data Set TEST_SCORES";
proc contents data=proj99.test_scores varnum;
run;

/* 4.5 Listing All the SAS Data Sets in a SAS Library Using PROC CONTENTS */
*Program 4-5 Using PROC CONTENTS to list the names of all the SAS data sets in a SAS library - page 59;
title "Listing All the SAS Data Sets in a Library";
proc contents data=Mozart._all_;
run;

proc contents data=proj99._all_ nods;
run;

/* 4.7 Viewing the Data Portion of a SAS Data Set Using PROC PRINT */

*Program 4-6 Using PROC PRINT to list the data portion of a SAS data set - page 63;
title "Listing of TEST_SCORES";
proc print data=Mozart.test_scores;
run;

/* 4.9 Using a SAS Data Set as Input to a DATA Step */
*Using observations from a SAS data set as input to a new SAS data set - page 66;
* this is a different example than shown in book ;
data test_scores;
   set mozart.test_scores;
   totscore=score1 + score2 + score3;
run;

/* Working with permanent data sets */
* generate some statistics;

proc freq data=mozart.blood;
  tables gender bloodtype;
run;

proc means data=proj99.blood;
  var chol wbc rbc;
run;
