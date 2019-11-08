#define ATS_DYNLOADFLAG 0

staload "elvysh/partially-initialized-array.sats"

primplement partially_initialized_array_unmax_before { t } { before
                                                           , after
                                                           , total
                                                           } ( pf ) =
  sif before > total then pf else let
    prval partially_initialized_array ( init, noninit ) = pf
    prval splits = array_v_split ( init )
    prval noninit = array_v_unsplit{ t? } ( splits.1, noninit )
  in partially_initialized_array ( splits.0, noninit ) end

primplement partially_initialized_array_unmax_after { t } { before
                                                          , after
                                                          , total
                                                          } ( pf ) =
  sif after > total then pf else let
    prval partially_initialized_array ( init, noninit ) = pf
    prval splits = array_v_split ( init )
    prval noninit = array_v_unsplit{ t? } ( splits.1, noninit )
  in partially_initialized_array ( splits.0, noninit ) end
