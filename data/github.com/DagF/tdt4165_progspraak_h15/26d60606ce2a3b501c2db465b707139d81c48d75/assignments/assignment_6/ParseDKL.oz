
%% Skeleton file for Exercise:Parser.
%%
%% Fill in missing code marked: 'YOUR CODE HERE'
%%
%% You might want to use Ozcar while solving this exercise.

\insert 'TokenizeDKL.oz'

local
   fun {ParseDKL Tokens}
      local Rest Tree in
         Tree = {Prog Tokens Rest}
         if Rest == nil then ast(Tree)
         else raise unparsedTokens(Rest) end end
      end
   end
   fun {Prog Tokens Rest}
      {Stat Tokens Rest}
   end
   fun {Stat Tokens Rest}
      local Rest1 in
         {SeqRecord
          {SingleStat Tokens Rest1}
          {StatSeq Rest1 Rest}}
      end
   end
   fun {StatSeq Tokens Rest} % Another statement?
      local Rest1 in
         case Tokens
         of 'skip'|_ then {SeqRecord {Stat Tokens Rest1} {StatSeq Rest1 Rest}}
         [] 'local'|_ then {SeqRecord {Stat Tokens Rest1} {StatSeq Rest1 Rest}}
         [] ident(_)|_ then {SeqRecord {Stat Tokens Rest1} {StatSeq Rest1 Rest}}
         [] 'if'|_ then {SeqRecord {Stat Tokens Rest1} {StatSeq Rest1 Rest}}
         [] 'case'|_ then {SeqRecord {Stat Tokens Rest1} {StatSeq Rest1 Rest}}
         [] '{'|_ then {SeqRecord {Stat Tokens Rest1} {StatSeq Rest1 Rest}}
         else Rest = Tokens nil
         end
      end
   end
   fun {SingleStat Tokens Rest}
      try
         case Tokens
         of 'skip'  | R then Rest=R 'skip'
         [] 'local' | R then {Local R Rest}
         [] ident(Id) | T then {IdEqn ident(Id) T Rest}
         [] 'if' | R then {If R Rest}
         [] 'case' | R then {Case R Rest}
         [] '{' | R then {Application R Rest}
         else raise singleStatPE(Tokens) end
         end
      catch E then raise singleStatPE(E) end
      end
   end %% For producing correct stat_seq-records in the AST.
   fun {SeqRecord X Y}
      if Y==nil then X else stat_seq(X Y) end
   end
   fun {IdEqn Id1 '=' | Tokens Rest}
      try
         case Tokens
         of ident(Id2) | R then
            Rest = R
            varvar_bind(Id1 ident(Id2))
         else {VarValBind Id1 Tokens Rest}
         end
      catch E then raise statement2PE(E) end
      end
   end
   fun {VarValBind Id1 Tokens Rest}
      try
         val_cr(Id1 {Val Tokens Rest})
      catch E then raise assignPE(E) end
      end
   end
   fun {Val Tokens Rest}
      try
         case Tokens
         of int(N) | R then Rest = R int(N)
         [] float(N) | R then Rest = R float(N)
         [] atom(_) | _ then {Record Tokens Rest}
         [] 'true' | R then Rest = R recpat('true' nil)
         [] 'false' | R then Rest = R recpat('false' nil)
         [] 'proc' | R then {DefProc R Rest}
         else raise valuePE(Tokens) end
         end
      catch E then raise valuePE(E) end end
   end
   fun {Lit Tokens Rest}
      try
         case Tokens
         of atom(N) | R then Rest=R atom(N)
         [] 'true' | R then Rest = R 'true'
         [] 'false' | R then Rest = R 'false'
         end
      catch E then raise literalPE(E) end end
   end
   %% Task 1c
   fun {DefProc Tokens Rest}
      %% YOUR CODE HERE
      foo % remove this line
   end
   fun {Record Tokens Rest}
      local Rest1 in
         recpat( {Lit Tokens Rest1}
                 {RecPat Rest1 Rest} )
      end
   end
   fun {FeatureList Tokens Rest}
      local F Rest2 in
         F = {Feature Tokens Rest2}
         case F
         of nil then Rest=Rest2 nil
         else F | {FeatureList Rest2 Rest}
         end
      end
   end
   fun {Feature Tokens Rest}
      local F Rest1 Rest2 in
         case Tokens
         of atom(N) | R then Rest1 = R F = atom(N)
         [] 'true' | R then Rest1 = R F = 'true'
         [] 'false' | R then Rest1 = R F = 'false'
         [] int(N) | R then Rest1 = R F = int(N)
         else Rest=Tokens F = nil end
         if F == nil then nil else
            Rest1 = ':' | Rest2
            memb(F {Ident Rest2 Rest})
         end
      end
   end
   %% Task 1a
   fun {If Tokens Rest}
      %% YOUR CODE HERE
      foo % remove this line
   end
   fun {Local Tokens Rest}
      try
         local Rest1 in
            var_cr( {Ident Tokens 'in'|Rest1}
                    {Stat Rest1 'end'|Rest} )
         end
      catch E then raise localPE(E) end
      end
   end
   %% Task 1b
   fun {Case Tokens Rest}
      %% YOUR CODE HERE
      foo % remove this line
   end
   fun {Pat Tokens Rest}
      local Rest1 in
         recpat( {Lit Tokens Rest1}
                 {RecPat Rest1 Rest} )
      end
   end
   fun {RecPat Tokens Rest}
      case Tokens
      of '(' | Rest1 then {FeatureList Rest1 ')'|Rest}
      else Tokens = Rest nil
      end
   end
   %% Task 1d
   fun {Application Tokens Rest}
      %% YOUR CODE HERE
      foo % remove this line
   end
   fun {IdentList Tokens Rest}
      case Tokens
      of ident(Id) | R then ident(Id) | {IdentList R Rest}
      else Tokens = Rest nil
      end
   end
   fun {Ident Tokens Rest}
      try
         local Id in
            Tokens = ident(Id) | Rest
            ident(Id)
         end
      catch E then raise identPE(E) end
      end
   end
in
   try
      {Inspect {ParseDKL {TokenizeDKL {GetText "Search.dkl"}}}}
   catch E then {Inspect E} end
end
