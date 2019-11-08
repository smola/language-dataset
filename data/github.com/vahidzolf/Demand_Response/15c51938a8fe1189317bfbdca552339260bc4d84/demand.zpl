#Minimum Hourly Demand
param minhd:=0;
#Maximum Hourly Demand
param maxhd:=3;
#Minimum daily Consumption 
param eday:=15;
#Ramping Up Limit
param rU:=1;
#Ramping Down Limit
param rD:=1;
#Consumer Utility
param cu:=41.5;
#Robustness percentage

param t:=23;
param ta:=0.1*(25-t);

set H:={1..24};
set H0_24_t:={0..24-t};
set H1_24_t:={1..24-t};
set H0_25_t:={0..25-t};
set H1_25_t:={1..25-t};
set HE:={t..24};
set NOW:={1..t-1};
set Ht_24:={t..24};
set Ht1_24:={t+1..24};
set Ht1_25:={t+1..25};
set dem:={1..t};

var e[Ht_24] real;
var ep[Ht1_24] real;
var y[Ht1_24] real;
var d[Ht1_25] real;

#B is betha 
var B real; 
#et_0 is energy consumption for following hour   
#var et_0 real;

param maxh:=max(H);
param actual_prices[H] := read "price.txt" as "<1n> 2n";
param received_price:=actual_prices[t];
param lmin[H] := read "price.txt" as "<1n> 3n";
param lmax[H] := read "price.txt" as "<1n> 4n";
param demands[dem]:= read "sofars.txt" as "<1n> 2n";
param sofar[dem] := read "sofars.txt" as "<1n> 3n";


#do print received_price; 
#do forall <h> in H : print actual_prices[h];
#do forall <h> in H : print lmin[h];
#do forall <h> in H : print lmax[h];
do forall <h> in NOW : print sofar[h];
#Objective Functions: 
minimize energy : received_price*e[t] - cu +
		  sum <h> in H1_24_t : (lmin[t+h]*e[t+h] - cu ) + 
		  B*ta +
		  sum <h> in H1_24_t : ep[t+h] ;
#subject to : 
subto b1 :  e[t] + 
		sum <n> in NOW : sofar[n] +
		sum <h> in H1_24_t : e[t+h] >= eday ; 
subto c1: forall <h> in H0_24_t:  if (h==0)
					then    e[t+h]==0.5*demands[t]+0.5*d[t+h+1] 
					else    e[t+h]==0.5*d[t+h]+0.5*d[t+h+1] 
				end;
subto d1: forall <h> in H0_24_t: if (h==0)
					then demands[t]-d[t+h+1] <= rD 
					else d[t+h]-d[t+h+1] <= rD				
				end;
subto e1: forall <h> in H0_24_t: if (h==0)
					then 	d[t+h+1]-demands[t] <= rU 
				        else    d[t+h+1]-d[t+h] <= rU				
				end;
subto f1_1: forall <h> in H0_24_t: d[t+h+1] <= maxhd;
subto f1_2: forall <h> in H0_24_t: d[t+h+1] >= minhd;

subto c2: forall <h> in H1_24_t: B + ep[t+h] >= ((lmax[t+h]-lmin[t+h])*y[t+h]);
subto d2: forall <h> in H1_24_t: ep[t+h]>=0;
subto e2: forall <h> in H1_24_t: y[t+h]>=0;
subto f2: B >= 0;
subto g2: forall <h> in H1_24_t: e[h+t] <= y[h+t];

