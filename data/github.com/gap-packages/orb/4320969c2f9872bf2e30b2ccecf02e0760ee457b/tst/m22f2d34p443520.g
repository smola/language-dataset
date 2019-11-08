LoadPackage("atlasrep");
LoadPackage("orb");
Print("M22 acting on F_2^{34}, first a regular orbit...\n");
Print("Use 500MB initial workspace, should show one garbage ",
      "collection per test\n");
Print("and one initial one (more for enumerations with stabiliser).\n");

gens := ShallowCopy(AtlasGenerators([ "M22", [ "M22G1-f2r34B0.m1", "M22G1-f2r34B0.m2" ], 1, 2 ]).generators);
pgens := ShallowCopy(AtlasGenerators([ "M22", [ "M22G1-p22B0.m1", "M22G1-p22B0.m2" ], 1, 22 ]).generators);
v := [ Z(2)^0, Z(2)^0, Z(2)^0, 0*Z(2), 0*Z(2), 0*Z(2), Z(2)^0, Z(2)^0, Z(2)^0, 
0*Z(2), 0*Z(2), 0*Z(2), 0*Z(2), 0*Z(2), Z(2)^0, Z(2)^0, Z(2)^0, 0*Z(2), Z(2)^0,
Z(2)^0, 0*Z(2), Z(2)^0, 0*Z(2), Z(2)^0, 0*Z(2), Z(2)^0, Z(2)^0, 0*Z(2), 
0*Z(2), Z(2)^0, Z(2)^0, Z(2)^0, 0*Z(2), 0*Z(2) ];
ConvertToVectorRep(v,2);
GASMAN("collect");

sometests := function(o)
  local l,wo;
  l := Length(o);
  if not(o[l] in o) then Error(1); fi;
  if o!.storenumbers then
      if Position(o,o[l]) <> l then Error(2); fi;
      if PositionCanonical(o,o[l]) <> l then Error(3); fi;
  else
      if Position(o,o[l]) <> l then Error(2); fi;
      if PositionCanonical(o,o[l]) <> l then Error(3); fi;
  fi;
  if not(IsPermOnIntOrbitRep(o)) then
      if 0*o[1] in o then Error(4); fi;
      if Position(o,0*o[1]) <> fail then Error(5); fi;
      if PositionCanonical(o,0*o[1]) <> fail then Error(6); fi;
  fi;
  if o!.schreier then
      wo := TraceSchreierTreeForward(o,l);
      if ActWithWord(o!.gens,wo,o!.op,o[1]) <> o[l] then Error(7); fi;
      wo := TraceSchreierTreeBack(o,l);
      if ActWithWord(List(o!.gens,x->x^-1),wo,o!.op,o[l]) <> o[1] then
          Error(8);
      fi;
  fi;
end;

o := Orb(gens,v,OnRight,1000000);
ti := Runtime();
Enumerate(o);
Print("Plain orbit: ",Runtime()-ti,"\n"); ti := Runtime();
sometests(o);
GASMAN("collect");

o := Orb(gens,v,OnRight,1000000,rec(orbsizebound := 443520));
ti := Runtime();
Enumerate(o);
Print("Plain orbit with orbit size bound: ",Runtime()-ti,"\n"); ti := Runtime();
sometests(o);
GASMAN("collect");

# Now with Schreier tree:
o := Orb(gens,v,OnRight,1000000,rec(schreier := true));
ti := Runtime();
Enumerate(o);
Print("Orbit with Schreier tree: ",
      Runtime()-ti,"\n"); ti := Runtime();
sometests(o);
GASMAN("collect");

# Now with storing numbers:
o := Orb(gens,v,OnRight,1000000,rec(schreier := true, storenumbers := true));
ti := Runtime();
Enumerate(o);
Print("Orbit with Schreier tree and storing numbers: ",
      Runtime()-ti,"\n"); ti := Runtime();
sometests(o);
GASMAN("collect");

# Now with stabiliser:
o := Orb(gens,v,OnRight,1000000,rec(permgens := pgens));
ti := Runtime();
Enumerate(o);
Print("Orbit with stabiliser: ",
      Runtime()-ti,"\n"); ti := Runtime();
sometests(o);
StabWords(o);
Stabilizer(o);
GASMAN("collect");

# Now with stabiliser, but with known group size:
o := Orb(gens,v,OnRight,1000000,
         rec(permgens := pgens, grpsizebound := 443520));
ti := Runtime();
Enumerate(o);
Print("Orbit with stabiliser and known group size bound: ",
      Runtime()-ti,"\n"); ti := Runtime();
sometests(o);
StabWords(o);
Stabilizer(o);
GASMAN("collect");

# Now with stabiliser and known stabiliser size:
o := Orb(gens,v,OnRight,1000000,
         rec(permgens := pgens, stabsizebound := 1));
ti := Runtime();
Enumerate(o);
Print("Orbit with stabiliser and known stab size bound: ",
      Runtime()-ti,"\n"); ti := Runtime();
sometests(o);
StabWords(o);
Stabilizer(o);
GASMAN("collect");

# Now with stabiliser and known orbit length and stabiliser size:
o := Orb(gens,v,OnRight,1000000,
         rec(permgens := pgens, stabsizebound := 1, orbsizebound := 443520));
ti := Runtime();
Enumerate(o);
Print("Orbit with stabiliser and known stab and orbit size bound: ",
      Runtime()-ti,"\n"); ti := Runtime();
sometests(o);
StabWords(o);
Stabilizer(o);
GASMAN("collect");

# Now with stabiliser and known group size bound only stab:
o := Orb(gens,v,OnRight,1000000,
         rec(permgens := pgens, grpsizebound := 443520,
             onlystab := true));
ti := Runtime();
Enumerate(o);
Print("Orbit with stabiliser, known group size bound and only stab: ",
      Runtime()-ti,"\n"); ti := Runtime();
sometests(o);
StabWords(o);
Stabilizer(o);
GASMAN("collect");




