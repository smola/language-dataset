% helper material for check.g

Define str_symbol := "symbol".
Define str_pi := "pi-type".
Define str_lam := "lambda-term".
Define str_app := "application".

Define aequiv := 
  fun(nextid:var)(owned t1 t2:trm). 
    let a1 = (acanon nextid t1) in
    let a2 = (acanon nextid t2) in
    let r = (eqtrm a1 a2) in
      dec a1 dec a2 r.

Define aequiv_conv : Forall(nextid:var)(t1 t2:trm)
                           (u:{(aequiv nextid t1 t2) = tt}).
                      { (acanon nextid t1) = (acanon nextid t2)} :=
  foralli(nextid:var)(t1 t2:trm)
         (u:{(aequiv nextid t1 t2) = tt}).
  abbrev P = trans join (eqtrm (acanon nextid t1) (acanon nextid t2))
                         (aequiv nextid t1 t2) 
                    u in
  abbrev P1 = trans cong (eqtrm * (acanon nextid t2)) 
                     symm eval (acanon nextid t1)
                   P in
  abbrev P2 = trans cong (eqtrm (acanon nextid t1) *) 
                     symm eval (acanon nextid t2)
                   P in
  [eqtrm_eq
    terminates (acanon nextid t1) by cinv (acanon nextid t1) P1
    terminates (acanon nextid t2) by cinv (acanon nextid t2) P2
    P].     


