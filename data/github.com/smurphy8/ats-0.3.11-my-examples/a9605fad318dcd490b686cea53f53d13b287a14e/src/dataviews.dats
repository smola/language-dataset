#include "share/atspre_staload.hats"



(* --------------------------------------------------
Optional Views
-------------------------------------------------- *)
dataview option_v (v:view+,bool) = 
  | Some_v (v,true) of (v) 
  | None_v (v,false) of () 




// TODO implement ptr_alloc_opt (don't trust unimplemented ATS!)
extern
fun {a:t@ype} 
  ptr_alloc_opt () : [l:addr] (option_v (a? @l, l > null) | ptr l)




(* --------------------------------------------------
Disjunctive Views
-------------------------------------------------- *)


dataview VOR (v0:view+, v1:view+, int) = 
  | VORLeft (v0,v1,0) 
  | VORRight (v0,v1,1)


(* Data view for linear arrays

Intuitively, the view for an array storing N elements of type T consists of N at-views: T@L0, T@L1, ..., and T@LN-1, 
where L0 is the starting address of the array and each subsequent L equals the previous one plus the size of T, 
that is, the number of bytes needed to store a value of the type T. The following declared dataview array_v precisely captures this intuituion: 
*)


(* dataview array_v(a:t@ype,addr,int) =  *)
(*   | {l:addr} *)
(*     array_v_nil(a,l,0) *)
(*   | {l:addr} {n:nat} *)
(*     array_v_cons(a,l,n+1) of (a@l, array_v(a, l+sizeof(a), n), n) *)
    
    
// TODO implement  Array Set and Array Get    