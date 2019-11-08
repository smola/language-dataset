//genesis
// kkit Version 11 flat dumpfile

// Saved on 100
include kkit {argv 1}
FASTDT = 0.001
SIMDT = 0.001
CONTROLDT = 0.1
PLOTDT = 0.1
MAXTIME = 100
TRANSIENT_TIME = 2
VARIABLE_DT_FLAG = 0
DEFAULT_VOL = 1.15874591124e-15
VERSION = 11.0 
setfield /file/modpath value ~/scripts/modules
kparms

//genesis
initdump -version 3 -ignoreorphans 1
simobjdump table input output alloced step_mode stepsize x y z
simobjdump xtree path script namemode sizescale
simobjdump xcoredraw xmin xmax ymin ymax
simobjdump xtext editable
simobjdump xgraph xmin xmax ymin ymax overlay
simobjdump xplot pixflags script fg ysquish do_slope wy
simobjdump group xtree_fg_req xtree_textfg_req plotfield expanded movealone \
  link savename file version md5sum mod_save_flag x y z
simobjdump geometry size dim shape outside xtree_fg_req xtree_textfg_req x y z
simobjdump kpool DiffConst CoInit Co n nInit mwt nMin vol slave_enable \
  geomname xtree_fg_req xtree_textfg_req x y z
simobjdump kreac kf kb notes xtree_fg_req xtree_textfg_req x y z
simobjdump kenz CoComplexInit CoComplex nComplexInit nComplex vol k1 k2 k3 \
  keepconc usecomplex notes xtree_fg_req xtree_textfg_req link x y z
simobjdump stim level1 width1 delay1 level2 width2 delay2 baselevel trig_time \
  trig_mode notes xtree_fg_req xtree_textfg_req is_running x y z
simobjdump xtab input output alloced step_mode stepsize notes editfunc \
  xtree_fg_req xtree_textfg_req baselevel last_x last_y is_running x y z
simobjdump kchan perm gmax Vm is_active use_nernst notes xtree_fg_req \
  xtree_textfg_req x y z
simobjdump transport input output alloced step_mode stepsize dt delay clock \
  kf xtree_fg_req xtree_textfg_req x y z
simobjdump proto x y z
simundump geometry /kinetics/geometry 0 1.15874591124e-15 3 sphere  "" white black 4 2 0
simundump group /kinetics/barr2_signaling 0 blue green x 0 0 "" defaultfile \
  defaultfile.g 0 0 0 1 2 0
simundump group /kinetics/MAPK 0 blue green x 0 0 "" defaultfile \
  defaultfile.g 0 0 0 1 2 0
simundump group /kinetics/mGluR 0 blue green x 0 0 "" defaultfile \
  defaultfile.g 0 0 0 1 2 0
simundump group /kinetics/Phosphatase 0 blue green x 0 0 "" defaultfile \
  defaultfile.g 0 0 0 1 2 0
simundump kpool /kinetics/mGluR/mGluR 0 0.0 0 0 0 209343.955202 0 0 697813.184005 0 /kinetics/geometry 0 black -1185 -3314 0
simundump kpool /kinetics/mGluR/Rec_Glu 0 0.0 0 0 0 0.0 0 0 697813.184005 0 /kinetics/geometry 31 black 142 -3844 0
simundump kpool /kinetics/barr2_signaling/GRK 0 0.0 0 0 0 805171.742365 0 0 697813.184005 0 /kinetics/geometry 42 black 1434 -4215 0
simundump kpool /kinetics/barr2_signaling/L.mGluR_p 0 0.0 0 0 0 0.0 0 0 697813.184005 0 /kinetics/geometry 24 black 1057 -2794 0
simundump kpool /kinetics/barr2_signaling/barr2 0 0.0 0 0 0 218066.620002 0 0 697813.184005 0 /kinetics/geometry 19 black 29 -1207 0
simundump kpool /kinetics/barr2_signaling/Internal_mGluR_p.barr2 0 0.0 0 0 0 0.0 0 0 697813.184005 0 /kinetics/geometry 0 black -71 234 0
simundump kpool /kinetics/barr2_signaling/L.mGluR_p.barr2 0 0.0 0 0 0 0.0 0 0 697813.184005 0 /kinetics/geometry 53 black 1521 -1915 0
simundump kpool /kinetics/Phosphatase/PP2A 0 0.0 0 0 0 104671.977601 0 0 697813.184005 0 /kinetics/geometry 7 black -2872 -1057 0
simundump kpool /kinetics/barr2_signaling/Internal_mGluR 0 0.0 0 0 0 0.0 0 0 697813.184005 0 /kinetics/geometry 61 black -2192 -2080 0
simundump kpool /kinetics/barr2_signaling/Internal_mGluR_p 0 0.0 0 0 0 0.0 0 0 697813.184005 0 /kinetics/geometry 39 black -1381 -718 0
simundump kpool /kinetics/mGluR/Glutamate 0 0.0 0 0 0 6978131.84005 0 0 697813.184005 4 /kinetics/geometry 2 black 226 -2463 0
simundump kpool /kinetics/MAPK/craf_1 0 0.0 0 0 0 348906.592001 0 0 697813.184005 0 /kinetics/geometry 23 black 823 1214 0
simundump kpool /kinetics/MAPK/craf_1_p 0 0.0 0 0 0 0.0 0 0 697813.184005 0 /kinetics/geometry 59 black -193 1914 0
simundump kpool /kinetics/MAPK/MAPKK 0 0.0 0 0 0 348906.592001 0 0 697813.184005 0 /kinetics/geometry 48 black -1753 2538 0
simundump kpool /kinetics/MAPK/MAPKK_p 0 0.0 0 0 0 0.0 0 0 697813.184005 0 /kinetics/geometry 27 black -613 2989 0
simundump kpool /kinetics/MAPK/MAPKK_p_p 0 0.0 0 0 0 0.0 0 0 697813.184005 0 /kinetics/geometry 7 black 690 3373 0
simundump kpool /kinetics/MAPK/MAPK 0 0.0 0 0 0 2512127.46242 0 0 697813.184005 0 /kinetics/geometry 57 black 100 4894 0
simundump kpool /kinetics/MAPK/MAPK_p 0 0.0 0 0 0 0.0 0 0 697813.184005 0 /kinetics/geometry 27 black 1262 4511 0
simundump kpool /kinetics/MAPK/MAPK_p_p 0 0.0 0 0 0 0.0 0 0 697813.184005 0 /kinetics/geometry 7 black 2244 3880 0
simundump kpool /kinetics/barr2_signaling/mGluR_p.barr2 0 0.0 0 0 0 0.0 0 0 697813.184005 0 /kinetics/geometry 50 black 1132 -804 0
simundump kreac /kinetics/mGluR/RecLigandBinding 0 2.40752115106e-05 10.0 "" white black -334 -3183 0
simundump kreac /kinetics/barr2_signaling/mGluR_barr2_assoc 0 2.86609660847e-08 0.00578 "" white black 676 -1952 0
simundump kreac /kinetics/barr2_signaling/mGluR_internalize 0 0.005 0.0 "" white black 692 -115 0
simundump kreac /kinetics/barr2_signaling/barr2_dissoc 0 0.005 0.0 "" white black -529 -497 0
simundump kreac /kinetics/barr2_signaling/mGluR_recycling 0 0.01 0.0 "" white black -1888 -2859 0
simundump kreac /kinetics/barr2_signaling/ligand_dissoc 0 10.0 8.5266374101e-12 "" white black 943 -1646 0
simundump kreac /kinetics/MAPK/mGluR_barr2_Raf_scaffolding 0 2.14957245633e-09 0.001 "" white black 19 1086 0
simundump kenz /kinetics/barr2_signaling/GRK/GRK_binding 0 0 0 0.0 0 697813.184005 1.03544106504e-07 0.66 0.165 0 0 "" black 20 "" 922 -3618 0
simundump kenz /kinetics/Phosphatase/PP2A/mGluR_dephosph 0 0 0 0.0 0 697813.184005 8.19875517566e-08 0.5 0.125 0 0 "" black 23 "" -2074 -1229 0
simundump kenz /kinetics/MAPK/craf_1_p/MEK_phospho 0 0 0 0.0 0 697813.184005 4.35692826744e-06 0.42 0.105 0 0 "" black 0 "" -971 2276 0
simundump kenz /kinetics/MAPK/craf_1_p/MEKp_phospho 0 0 0 0.0 0 697813.184005 4.35692826744e-06 0.42 0.105 0 0 "" black 63 "" 177 2693 0
simundump kenz /kinetics/MAPK/MAPKK_p_p/MAPKKtyr 0 0 0 0.0 0 697813.184005 2.14086839604e-05 0.6 0.15 0 0 "" black 23 "" 508 4200 0
simundump kenz /kinetics/MAPK/MAPKK_p_p/MAPKKthr 0 0 0 0.0 0 697813.184005 2.14086839604e-05 0.6 0.15 0 0 "" black 25 "" 1459 3711 0
simundump xgraph /graphs/conc1 0 0 99 0.001 0.999 0
simundump xgraph /graphs/conc2 0 0 100 0 1 0
 simundump xplot /graphs/conc1/_kinetics_0__mGluR_0__mGluR_0_.conc 3 524288 \
"delete_plot.w <s> <d>; edit_plot.D <w>" black 0 0 1
simundump xplot /graphs/conc1/_kinetics_0__barr2_signaling_0__mGluR_p.barr2_0_.conc 3 524288 \
"delete_plot.w <s> <d>; edit_plot.D <w>" black 0 0 1
simundump xplot /graphs/conc1/_kinetics_0__barr2_signaling_0__L.mGluR_p.barr2_0_.conc 3 524288 \
"delete_plot.w <s> <d>; edit_plot.D <w>" black 0 0 1
simundump xplot /graphs/conc1/_kinetics_0__barr2_signaling_0__L.mGluR_p_0_.conc 3 524288 \
"delete_plot.w <s> <d>; edit_plot.D <w>" black 0 0 1
simundump xplot /graphs/conc1/_kinetics_0__mGluR_0__Rec_Glu_0_.conc 3 524288 \
"delete_plot.w <s> <d>; edit_plot.D <w>" black 0 0 1
simundump xplot /graphs/conc1/_kinetics_0__barr2_signaling_0__Internal_mGluR_p.barr2_0_.conc 3 524288 \
"delete_plot.w <s> <d>; edit_plot.D <w>" black 0 0 1
simundump xplot /graphs/conc1/_kinetics_0__barr2_signaling_0__Internal_mGluR_p_0_.conc 3 524288 \
"delete_plot.w <s> <d>; edit_plot.D <w>" black 0 0 1
simundump xplot /graphs/conc1/_kinetics_0__barr2_signaling_0__Internal_mGluR_0_.conc 3 524288 \
"delete_plot.w <s> <d>; edit_plot.D <w>" black 0 0 1
simundump xplot /graphs/conc1/model_0__kinetics_0__MAPK_0__craf_1_0_.Conc 3 524288 \
"delete_plot.w <s> <d>; edit_plot.D <w>" black 0 0 1
simundump xplot /graphs/conc1/model_0__kinetics_0__MAPK_0__MAPK_p_p_0_.Conc 3 524288 \
"delete_plot.w <s> <d>; edit_plot.D <w>" black 0 0 1
simundump xplot /graphs/conc1/model_0__kinetics_0__MAPK_0__MAPK_p_0_.Conc 3 524288 \
"delete_plot.w <s> <d>; edit_plot.D <w>" black 0 0 1
simundump xplot /graphs/conc1/model_0__kinetics_0__MAPK_0__craf_1_p_0_.Conc 3 524288 \
"delete_plot.w <s> <d>; edit_plot.D <w>" black 0 0 1
simundump xgraph /moregraphs/conc3 0 0 100 0 1 0
simundump xgraph /moregraphs/conc4 0 0 100 0 1 0
 simundump xcoredraw /edit/draw 0 -6 4 -2 6
simundump xtree /edit/draw/tree 0 \
  /kinetics/#[],/kinetics/#[]/#[],/kinetics/#[]/#[]/#[][TYPE!=proto],/kinetics/#[]/#[]/#[][TYPE!=linkinfo]/##[] "edit_elm.D <v>; drag_from_edit.w <d> <S> <x> <y> <z>" auto 0.6
simundump xtext /file/notes 0 1
addmsg /kinetics/mGluR/mGluR /kinetics/mGluR/RecLigandBinding SUBSTRATE n 
addmsg /kinetics/mGluR/RecLigandBinding /kinetics/mGluR/mGluR REAC A B 
addmsg /kinetics/mGluR/Glutamate /kinetics/mGluR/RecLigandBinding SUBSTRATE n 
addmsg /kinetics/mGluR/RecLigandBinding /kinetics/mGluR/Glutamate REAC A B 
addmsg /kinetics/mGluR/Rec_Glu /kinetics/mGluR/RecLigandBinding PRODUCT n 
addmsg /kinetics/mGluR/RecLigandBinding /kinetics/mGluR/Rec_Glu REAC B A
addmsg /kinetics/barr2_signaling/barr2 /kinetics/barr2_signaling/mGluR_barr2_assoc SUBSTRATE n 
addmsg /kinetics/barr2_signaling/mGluR_barr2_assoc /kinetics/barr2_signaling/barr2 REAC A B 
addmsg /kinetics/barr2_signaling/L.mGluR_p /kinetics/barr2_signaling/mGluR_barr2_assoc SUBSTRATE n 
addmsg /kinetics/barr2_signaling/mGluR_barr2_assoc /kinetics/barr2_signaling/L.mGluR_p REAC A B 
addmsg /kinetics/barr2_signaling/L.mGluR_p.barr2 /kinetics/barr2_signaling/mGluR_barr2_assoc PRODUCT n 
addmsg /kinetics/barr2_signaling/mGluR_barr2_assoc /kinetics/barr2_signaling/L.mGluR_p.barr2 REAC B A
addmsg /kinetics/barr2_signaling/mGluR_p.barr2 /kinetics/barr2_signaling/mGluR_internalize SUBSTRATE n 
addmsg /kinetics/barr2_signaling/mGluR_internalize /kinetics/barr2_signaling/mGluR_p.barr2 REAC A B 
addmsg /kinetics/barr2_signaling/Internal_mGluR_p.barr2 /kinetics/barr2_signaling/mGluR_internalize PRODUCT n 
addmsg /kinetics/barr2_signaling/mGluR_internalize /kinetics/barr2_signaling/Internal_mGluR_p.barr2 REAC B A
addmsg /kinetics/barr2_signaling/Internal_mGluR_p.barr2 /kinetics/barr2_signaling/barr2_dissoc SUBSTRATE n 
addmsg /kinetics/barr2_signaling/barr2_dissoc /kinetics/barr2_signaling/Internal_mGluR_p.barr2 REAC A B 
addmsg /kinetics/barr2_signaling/Internal_mGluR_p /kinetics/barr2_signaling/barr2_dissoc PRODUCT n 
addmsg /kinetics/barr2_signaling/barr2_dissoc /kinetics/barr2_signaling/Internal_mGluR_p REAC B A
addmsg /kinetics/barr2_signaling/barr2 /kinetics/barr2_signaling/barr2_dissoc PRODUCT n 
addmsg /kinetics/barr2_signaling/barr2_dissoc /kinetics/barr2_signaling/barr2 REAC B A
addmsg /kinetics/barr2_signaling/Internal_mGluR /kinetics/barr2_signaling/mGluR_recycling SUBSTRATE n 
addmsg /kinetics/barr2_signaling/mGluR_recycling /kinetics/barr2_signaling/Internal_mGluR REAC A B 
addmsg /kinetics/mGluR/mGluR /kinetics/barr2_signaling/mGluR_recycling PRODUCT n 
addmsg /kinetics/barr2_signaling/mGluR_recycling /kinetics/mGluR/mGluR REAC B A
addmsg /kinetics/barr2_signaling/L.mGluR_p.barr2 /kinetics/barr2_signaling/ligand_dissoc SUBSTRATE n 
addmsg /kinetics/barr2_signaling/ligand_dissoc /kinetics/barr2_signaling/L.mGluR_p.barr2 REAC A B 
addmsg /kinetics/barr2_signaling/mGluR_p.barr2 /kinetics/barr2_signaling/ligand_dissoc PRODUCT n 
addmsg /kinetics/barr2_signaling/ligand_dissoc /kinetics/barr2_signaling/mGluR_p.barr2 REAC B A
addmsg /kinetics/mGluR/Glutamate /kinetics/barr2_signaling/ligand_dissoc PRODUCT n 
addmsg /kinetics/barr2_signaling/ligand_dissoc /kinetics/mGluR/Glutamate REAC B A
addmsg /kinetics/MAPK/craf_1 /kinetics/MAPK/mGluR_barr2_Raf_scaffolding SUBSTRATE n 
addmsg /kinetics/MAPK/mGluR_barr2_Raf_scaffolding /kinetics/MAPK/craf_1 REAC A B 
addmsg /kinetics/barr2_signaling/Internal_mGluR_p.barr2 /kinetics/MAPK/mGluR_barr2_Raf_scaffolding SUBSTRATE n 
addmsg /kinetics/MAPK/mGluR_barr2_Raf_scaffolding /kinetics/barr2_signaling/Internal_mGluR_p.barr2 REAC A B 
addmsg /kinetics/MAPK/craf_1_p /kinetics/MAPK/mGluR_barr2_Raf_scaffolding PRODUCT n 
addmsg /kinetics/MAPK/mGluR_barr2_Raf_scaffolding /kinetics/MAPK/craf_1_p REAC B A
addmsg /kinetics/mGluR/Rec_Glu /kinetics/barr2_signaling/GRK/GRK_binding SUBSTRATE n 
addmsg /kinetics/barr2_signaling/GRK/GRK_binding /kinetics/mGluR/Rec_Glu REAC sA B 
addmsg /kinetics/barr2_signaling/GRK/GRK_binding /kinetics/barr2_signaling/L.mGluR_p MM_PRD pA
addmsg /kinetics/barr2_signaling/GRK /kinetics/barr2_signaling/GRK/GRK_binding ENZYME n
addmsg /kinetics/barr2_signaling/GRK/GRK_binding /kinetics/barr2_signaling/GRK REAC eA B
addmsg /kinetics/barr2_signaling/Internal_mGluR_p /kinetics/Phosphatase/PP2A/mGluR_dephosph SUBSTRATE n 
addmsg /kinetics/Phosphatase/PP2A/mGluR_dephosph /kinetics/barr2_signaling/Internal_mGluR_p REAC sA B 
addmsg /kinetics/Phosphatase/PP2A/mGluR_dephosph /kinetics/barr2_signaling/Internal_mGluR MM_PRD pA
addmsg /kinetics/Phosphatase/PP2A /kinetics/Phosphatase/PP2A/mGluR_dephosph ENZYME n
addmsg /kinetics/Phosphatase/PP2A/mGluR_dephosph /kinetics/Phosphatase/PP2A REAC eA B
addmsg /kinetics/MAPK/MAPKK /kinetics/MAPK/craf_1_p/MEK_phospho SUBSTRATE n 
addmsg /kinetics/MAPK/craf_1_p/MEK_phospho /kinetics/MAPK/MAPKK REAC sA B 
addmsg /kinetics/MAPK/craf_1_p/MEK_phospho /kinetics/MAPK/MAPKK_p MM_PRD pA
addmsg /kinetics/MAPK/craf_1_p /kinetics/MAPK/craf_1_p/MEK_phospho ENZYME n
addmsg /kinetics/MAPK/craf_1_p/MEK_phospho /kinetics/MAPK/craf_1_p REAC eA B
addmsg /kinetics/MAPK/MAPKK_p /kinetics/MAPK/craf_1_p/MEKp_phospho SUBSTRATE n 
addmsg /kinetics/MAPK/craf_1_p/MEKp_phospho /kinetics/MAPK/MAPKK_p REAC sA B 
addmsg /kinetics/MAPK/craf_1_p/MEKp_phospho /kinetics/MAPK/MAPKK_p_p MM_PRD pA
addmsg /kinetics/MAPK/craf_1_p /kinetics/MAPK/craf_1_p/MEKp_phospho ENZYME n
addmsg /kinetics/MAPK/craf_1_p/MEKp_phospho /kinetics/MAPK/craf_1_p REAC eA B
addmsg /kinetics/MAPK/MAPK /kinetics/MAPK/MAPKK_p_p/MAPKKtyr SUBSTRATE n 
addmsg /kinetics/MAPK/MAPKK_p_p/MAPKKtyr /kinetics/MAPK/MAPK REAC sA B 
addmsg /kinetics/MAPK/MAPKK_p_p/MAPKKtyr /kinetics/MAPK/MAPK_p MM_PRD pA
addmsg /kinetics/MAPK/MAPKK_p_p /kinetics/MAPK/MAPKK_p_p/MAPKKtyr ENZYME n
addmsg /kinetics/MAPK/MAPKK_p_p/MAPKKtyr /kinetics/MAPK/MAPKK_p_p REAC eA B
addmsg /kinetics/MAPK/MAPK_p /kinetics/MAPK/MAPKK_p_p/MAPKKthr SUBSTRATE n 
addmsg /kinetics/MAPK/MAPKK_p_p/MAPKKthr /kinetics/MAPK/MAPK_p REAC sA B 
addmsg /kinetics/MAPK/MAPKK_p_p/MAPKKthr /kinetics/MAPK/MAPK_p_p MM_PRD pA
addmsg /kinetics/MAPK/MAPKK_p_p /kinetics/MAPK/MAPKK_p_p/MAPKKthr ENZYME n
addmsg /kinetics/MAPK/MAPKK_p_p/MAPKKthr /kinetics/MAPK/MAPKK_p_p REAC eA B
addmsg /kinetics/mGluR/mGluR /graphs/conc1/_kinetics_0__mGluR_0__mGluR_0_.conc PLOT Co *mGluR *0
addmsg /kinetics/barr2_signaling/mGluR_p.barr2 /graphs/conc1/_kinetics_0__barr2_signaling_0__mGluR_p.barr2_0_.conc PLOT Co *mGluR_p.barr2 *50
addmsg /kinetics/barr2_signaling/L.mGluR_p.barr2 /graphs/conc1/_kinetics_0__barr2_signaling_0__L.mGluR_p.barr2_0_.conc PLOT Co *L.mGluR_p.barr2 *53
addmsg /kinetics/barr2_signaling/L.mGluR_p /graphs/conc1/_kinetics_0__barr2_signaling_0__L.mGluR_p_0_.conc PLOT Co *L.mGluR_p *24
addmsg /kinetics/mGluR/Rec_Glu /graphs/conc1/_kinetics_0__mGluR_0__Rec_Glu_0_.conc PLOT Co *Rec_Glu *31
addmsg /kinetics/barr2_signaling/Internal_mGluR_p.barr2 /graphs/conc1/_kinetics_0__barr2_signaling_0__Internal_mGluR_p.barr2_0_.conc PLOT Co *Internal_mGluR_p.barr2 *0
addmsg /kinetics/barr2_signaling/Internal_mGluR_p /graphs/conc1/_kinetics_0__barr2_signaling_0__Internal_mGluR_p_0_.conc PLOT Co *Internal_mGluR_p *39
addmsg /kinetics/barr2_signaling/Internal_mGluR /graphs/conc1/_kinetics_0__barr2_signaling_0__Internal_mGluR_0_.conc PLOT Co *Internal_mGluR *61
addmsg /kinetics/MAPK/craf_1 /graphs/conc1/model_0__kinetics_0__MAPK_0__craf_1_0_.Conc PLOT Co *craf_1 *23
addmsg /kinetics/MAPK/MAPK_p_p /graphs/conc1/model_0__kinetics_0__MAPK_0__MAPK_p_p_0_.Conc PLOT Co *MAPK_p_p *7
addmsg /kinetics/MAPK/MAPK_p /graphs/conc1/model_0__kinetics_0__MAPK_0__MAPK_p_0_.Conc PLOT Co *MAPK_p *27
addmsg /kinetics/MAPK/craf_1_p /graphs/conc1/model_0__kinetics_0__MAPK_0__craf_1_p_0_.Conc PLOT Co *craf_1_p *59

enddump
 // End of dump
call /kinetics/barr2_signaling/GRK/notes LOAD \ 
"Bychkov et al., 2011. 150ng/mg *0.5mg/ml /65000g/mol = 1.155385nM (which is too low, therefore increasing the concentration arbitrarily by 1000 fold)"
call /kinetics/barr2_signaling/barr2/notes LOAD \ 
"Bychkov et al., 2011. 30ng/mg *0.5mg/ml /40000g/mol = 0.375nM (which is too low, therefore increasing the concentration arbitrarily by 1000 fold)"
call /kinetics/barr2_signaling/mGluR_barr2_assoc/notes LOAD \ 
"Heitzler,2012"
call /kinetics/barr2_signaling/mGluR_internalize/notes LOAD \ 
"Rate based on internalization time after beta-arrestin binding. According to Mundell SJ et al., 2001, t(1/2) ~ 1.9(+/-)0.4 min. Therefore, kf =0.005 to 0.0077/s"
call /kinetics/barr2_signaling/ligand_dissoc/notes LOAD \ 
"Navarro DL et al., Amino Acids (2005). Kd for glutamte-mGluR for rat fetus is 599 (+/-) 89.7 nM and for mothers is 534.3 (+/-) 89.7 nM. Therefore, assuming Kd = 595nM"
complete_loading
