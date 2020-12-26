// --------------------------------------------------------------------------
// Licensed Materials - Property of IBM
//
// 5725-A06 5725-A29 5724-Y48 5724-Y49 5724-Y54 5724-Y55
// Copyright IBM Corporation 1998, 2013. All Rights Reserved.
//
// Note to U.S. Government Users Restricted Rights:
// Use, duplication or disclosure restricted by GSA ADP Schedule
// Contract with IBM Corp.
// --------------------------------------------------------------------------

using CP;

include "common.mod";

dvar int pattern[1..nbRounds] in Where;

tuple p
{
   int v[1..nbRounds];
}

{p} patterns=...;


execute{
	cp.param.Workers = 1;
}


int nbPatterns = card(patterns);
range rngPatterns= 0..nbPatterns-1;


int score[i in rngPatterns] = sum(j in 1..nbRounds-3)((item(patterns,i).v[j]!=away)*
          (item(patterns,i).v[j+1]!=away)*(item(patterns,i).v[j+2]!=away)*(item(patterns,i).v[j+3]!=away));

int lesspref[i in rngPatterns] = 
     prod(j in 1..2) (item(patterns,i).v[j]==away);


dvar int patind[rngPatterns] in 0..1;

dvar int patset[0..nbTeams] in rngPatterns;




subject to
 {
   sum (i in rngPatterns) patind[i] == nbTeams;
   forall (j in 1..nbRounds) {
      sum (i in rngPatterns:item(patterns,i).v[j] == home )1*patind[i] == 4;
     sum (i in rngPatterns) (item(patterns,i).v[j]== away)*patind[i] == 4;
     sum (i in rngPatterns )(item(patterns,i).v[j]==  bbye )*patind[i] == 1;
  };
   forall (i in rngPatterns) {
       patind[i] == 1  => patset[(sum(j in 0..i) patind[j])] == i;
   };
   patset[0] == 1; 
   sum (i in rngPatterns) (lesspref[i]*patind[i]) <= 1;
   forall (i in rngPatterns, j in rngPatterns: i != j) {
      (patind[i]*patind[j] == 1 ) =>
          (sum (r in rngRounds) ((item(patterns,i).v[r]==home)*(item(patterns,j).v[r]==away))) >= 1;
   };
/*
  Use following constraints with Henz's patterns to select just one specific
  pattern from NT's solution corresponding to UNC 

   sum (i in rngPatterns) ((item(patterns,i).v[1]==away) && (item(patterns,i).v[2]==away))  * patind[i] <= 1;
   sum (i in rngPatterns) ((item(patterns,i).v[17]==away) && (item(patterns,i).v[18]==away))  * patind[i] <= 1;
   sum (i in rngPatterns) (score[i]*patind[i]) <= 1;
    forall (i in rngPatterns) ((item(patterns,i).v[1]==away)*(item(patterns,i).v[2]==away)*patind[i]) == 1 =>
            ( (item(patterns,i).v[6]==bbye)*patind[i])==1;
*/

};

main
{
   var  n=0;
   thisOplModel.generate();
   cp.startNewSearch();
   while (cp.next()) { 
     n++;
      
     thisOplModel.postProcess();
     writeln(thisOplModel.patset); 
   }
   
   writeln(n," solutions "); 
   
 

}




   
