# Copywrite (C) 2015 Jan Vaillant <jan.vaillant@zalf.de>
# Licensed under the MIT licence

# Create cow feed rations for all periods and groups as such that the absolute (relative %) 
# difference between cow/group requirements (energy, protein) and diets is minimized.

set Period      := { 1, 2 };
set Group       := { 1, 2 };
set Cow         := { 1, 2, 3, 4 };
set Nutrient    := { "energy", "protein" };
set CowAttr     := Nutrient + { "intake", "no" };
set Feed        := { "grasssilage", "maizesilage", "wheat", "soy" };
set FeedAttr    := Nutrient + { "fill", "rnb" };
set Concentrate := { "wheat", "soy" };

param need[Period * Group * Cow  * CowAttr] :=        
        |"energy", "protein", "intake", "no"|    
  |1,1,1| 111    ,  2399    ,  15.25  ,  10 |
  |1,1,2| 110    ,  2330    ,  16.91  ,  10 | 
  |1,2,3|  99    ,  2135    ,  16.60  ,  10 |
  |1,2,4|  97    ,  2097    ,  16.23  ,  10 |
  |2,1,1| 108    ,  2274    ,  17.20  ,  10 |
  |2,1,2| 103    ,  2198    ,  16.95  ,  10 |
  |2,2,3|  95    ,  2070    ,  15.58  ,  10 |
  |2,2,4|  95    ,  2069    ,  15.42  ,  10 | default 0;
#         (NEL)     (uCP)      (LFU)     (no. cows)

param feed[Feed * FeedAttr] :=                
                |"energy", "protein", "fill", "rnb"|    
  |"grasssilage"| 5.9    ,  135    ,  0.914 ,  6   |
  |"maizesilage"| 6.9    ,  136    ,  0.962 ,  -9  |
  |      "wheat"| 8.5    ,  170    ,  0.400 ,  -5  |
  |        "soy"| 9.9    ,  250    ,  0.400 ,  24  |;
#                 (NEL)     (uCP)     (LFU)   (uCP)

param max_conc_pc := 0.4;
param cows_per_gr := 2;

var x_m[Period * Group * Cow * Nutrient];
var x_p[Period * Group * Cow * Nutrient];
var y[Period * Group * Feed];

maximize satisfaction: sum <t> in Period: sum <i> in Group: sum <l> in Cow: sum <k> in Nutrient 
  with need[t,i,l,"no"] > 0: - need[t,i,l,"no"] * (x_m[t,i,l,k] + x_p[t,i,l,k]);

subto nutrients: forall <t,i,l,k> in Period * Group * Cow * Nutrient
  with need[t,i,l,"no"] > 0: 
  need[t,i,l,"intake"] / ((sum <lx> in Cow: need[t,i,lx,"intake"]) / cows_per_gr) * 
  sum <j> in Feed: y[t,i,j] * feed[j,k] / need[t,i,l,k] + x_m[t,i,l,k] - x_p[t,i,l,k] == 1;

subto intake: forall <t,i> in Period * Group: 
  sum <j> in Feed: y[t,i,j] * feed[j,"fill"] == sum <l> in Cow: need[t,i,l,"intake"] / cows_per_gr;

subto rnb: forall <t,i,l> in Period * Group * Cow
  with need[t,i,l,"no"] > 0:
  0 <= need[t,i,l,"intake"] / ((sum <lx> in Cow: need[t,i,lx,"intake"]) / cows_per_gr) * sum <j> in Feed: y[t,i,j] * feed[j,"rnb"] <= 50;

subto concentrates: forall <t,i> in Period * Group:
  sum <j> in Concentrate: (1 - max_conc_pc) / max_conc_pc * y[t,i,j] - 
  sum <j> in Feed \ Concentrate: y[t,i,j] <= 0;

subto maizesilage: sum <t,i,l,j> in Period * Group * Cow * Feed 
  with j == "maizesilage" and need[t,i,l,"no"] > 0:
  need[t,i,l,"no"] * y[t,i,j] * need[t,i,l,"intake"] / ((sum <lx> in Cow: need[t,i,lx,"intake"]) / cows_per_gr)  <= 300;
  
