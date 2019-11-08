set Food      := { "F1", "F2", "F3" };
set NutritionalFeature := { "Energy", "N1", "N2", "N3" };
set Attr      := NutritionalFeature + { "Price" };

param needed[NutritionalFeature] := <"Energy"> 2000, <"N1"> 200, <"N2"> 50, <"N3"> 150; #Energy (kcal), Nn (g)

param data[Food * Attr] :=
      |      "Energy" , "N1" , "N2", "N3" , "Price" |
|"F1" |            14 ,  0.1 , 0.1 ,  0.8 ,       1 |
|"F2" |            25 ,  0.5 , 0.3 ,  0.2 ,       2 |
|"F3" |            20 ,  0.4 , 0.5 , 0.55 ,       3 |;
#            (kcal/g)    (%)   (%)    (%)  (euro/g)

var x[<f> in Food] real >= 0; #(g)

minimize cost: sum <f> in Food : data[f, "Price"] * x[f];

subto need :
  forall <n> in NutritionalFeature do
    sum <f> in Food : data[f, n] * x[f] >= needed[n];

