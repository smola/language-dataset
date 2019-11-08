*ECON TERM PAPER FINAL COMMANDS 

*summary statistics for replication
sum hg phg gun pgun pgs majorcrime murder rape robbery assault burglary prison metpct ampct military employ unrate rpci p1524 p2544 p4564 pop if year < 1999
*same, with the new variabels we added
sum hg phg gun pgun pgs gunsamm amhms amrmms majorcrime murder rape robbery assault burglary prison metpct ampct military employ unrate rpci p1524 p2544 p4564 pop


*gun suicide relationship w/ gun ownership
reg gun pgs
eststo
reg hg pgs
eststo
esttab
eststo clear
corr gun hg pgs
corr pgun phg pgs 
*compared w/ magazines: (note: imputed b/c data is sparse for this)
corr pgun phg gunsamm amrmms amhms

*regressions: up until 1999 for closest replication
reg majorcrime hg prison metpct ampct employ unrate p1524 p2544 p4564 rpci if year < 1999, robust
eststo
reg majorcrime phg prison metpct ampct employ unrate p1524 p2544 p4564 rpci if year < 1999, robust
eststo

reg mc_noassault hg prison metpct ampct employ unrate p1524 p2544 p4564 rpci  if year < 1999, robust
eststo
reg mc_noassault phg prison metpct ampct employ unrate p1524 p2544 p4564 rpci if year < 1999, robust
eststo

reg murder hg prison metpct ampct employ unrate p1524 p2544 p4564 rpci if year < 1999, robust
eststo
reg murder phg prison metpct ampct employ unrate p1524 p2544 p4564 rpci if year < 1999, robust
eststo

*show first set of regressions w/ 15, 10, 5% significance levels
esttab, compress nogaps r2 ar2 star(* 0.15 ** 0.10 *** 0.05)
eststo clear


reg rape hg prison metpct ampct employ unrate p1524 p2544 p4564 rpci if year < 1999, robust
eststo
reg rape phg prison metpct ampct employ unrate p1524 p2544 p4564 rpci if year < 1999, robust
eststo

reg robbery hg prison metpct ampct employ unrate p1524 p2544 p4564 rpci if year < 1999, robust
eststo
reg robbery phg prison metpct ampct employ unrate p1524 p2544 p4564 rpci if year < 1999, robust
eststo

reg assault hg prison metpct ampct employ unrate p1524 p2544 p4564 rpci if year < 1999, robust
eststo
reg assault phg prison metpct ampct employ unrate p1524 p2544 p4564 rpci if year < 1999, robust
eststo

reg burglary hg prison metpct ampct employ unrate p1524 p2544 p4564 rpci if year < 1999, robust
eststo
reg burglary phg prison metpct ampct employ unrate p1524 p2544 p4564 rpci if year < 1999, robust
eststo

*show second set of regressions w/ 15, 10, 5% significance levels
esttab, compress nogaps r2 ar2 star(* 0.15 ** 0.10 *** 0.05)
eststo clear

/*
coefficient magnitudes are quite different than the original Marvell  Moody Study.
The study does not list how many of its variables are scaled, so we were liberal
with how we did the scaling ourselves. Because of this, sign and significance are 
much more important than the coefficients themselves to what we are examining.
*/






/*
Regressions with our model improvements:
-new years added in ('99-'13)
-gun production variable added
-gallup polls tested but data was sparse.. could potentially try imputing this later
*/

*1: all years (more data), non-imputed:
reg majorcrime hg prison metpct ampct employ unrate p1524 p2544 p4564 rpci, robust
eststo
reg mc_noassault hg prison metpct ampct employ unrate p1524 p2544 p4564 rpci, robust
eststo
reg murder hg prison metpct ampct employ unrate p1524 p2544 p4564 rpci, robust
eststo
reg rape hg prison metpct ampct employ unrate p1524 p2544 p4564 rpci, robust
eststo
reg robbery hg prison metpct ampct employ unrate p1524 p2544 p4564 rpci, robust
eststo
reg assault hg prison metpct ampct employ unrate p1524 p2544 p4564 rpci, robust
eststo
reg burglary hg prison metpct ampct employ unrate p1524 p2544 p4564 rpci, robust
eststo

*table 1 w/ new years:
esttab, compress nogaps r2 ar2 star(* 0.15 ** 0.10 *** 0.05)
eststo clear


reg majorcrime phg prison metpct ampct employ unrate p1524 p2544 p4564 rpci, robust
eststo
reg mc_noassault phg prison metpct ampct employ unrate p1524 p2544 p4564 rpci, robust
eststo
reg murder phg prison metpct ampct employ unrate p1524 p2544 p4564 rpci, robust
eststo
reg rape phg prison metpct ampct employ unrate p1524 p2544 p4564 rpci, robust
eststo
reg robbery phg prison metpct ampct employ unrate p1524 p2544 p4564 rpci, robust
eststo
reg assault phg prison metpct ampct employ unrate p1524 p2544 p4564 rpci, robust
eststo
reg burglary phg prison metpct ampct employ unrate p1524 p2544 p4564 rpci, robust
eststo

*table 2 w/ new years, imputed:
esttab, compress nogaps r2 ar2 star(* 0.15 ** 0.10 *** 0.05)
eststo clear


*now testing with magazine circulation variables added '99-'13:
reg majorcrime phg prison metpct ampct employ unrate gunsamm amhms amrmms p1524 p2544 p4564 rpci, robust
eststo
reg mc_noassault phg prison metpct ampct employ unrate gunsamm amhms amrmms p1524 p2544 p4564 rpci, robust
eststo
reg murder phg prison metpct ampct employ unrate gunsamm amhms amrmms p1524 p2544 p4564 rpci, robust
eststo
reg rape phg prison metpct ampct employ unrate gunsamm amhms amrmms p1524 p2544 p4564 rpci, robust
eststo
reg robbery phg prison metpct ampct employ unrate gunsamm amhms amrmms p1524 p2544 p4564 rpci, robust
eststo
reg assault phg prison metpct ampct employ unrate gunsamm amhms amrmms p1524 p2544 p4564 rpci, robust
eststo
reg burglary phg prison metpct ampct employ unrate gunsamm amhms amrmms p1524 p2544 p4564 rpci, robust
eststo

esttab, compress nogaps r2 ar2 star(* 0.15 ** 0.10 *** 0.05)
eststo clear



*using all guns instead of just handguns
reg majorcrime pgun prison metpct ampct employ unrate p1524 p2544 p4564 rpci, robust
eststo
reg mc_noassault pgun prison metpct ampct employ unrate p1524 p2544 p4564 rpci, robust
eststo
reg murder pgun prison metpct ampct employ unrate p1524 p2544 p4564 rpci, robust
eststo
reg rape pgun prison metpct ampct employ unrate p1524 p2544 p4564 rpci, robust
eststo
reg robbery pgun prison metpct ampct employ unrate p1524 p2544 p4564 rpci, robust
eststo
reg assault pgun prison metpct ampct employ unrate p1524 p2544 p4564 rpci, robust
eststo
reg burglary pgun prison metpct ampct employ unrate p1524 p2544 p4564 rpci, robust
eststo

*gun production added
reg majorcrime phg prison metpct ampct employ unrate gunprod p1524 p2544 p4564 rpci, robust
eststo
reg mc_noassault phg prison metpct ampct employ unrate gunprod p1524 p2544 p4564 rpci, robust
eststo
reg murder phg prison metpct ampct employ unrate gunprod p1524 p2544 p4564 rpci, robust
eststo
reg rape phg prison metpct ampct employ unrate gunprod p1524 p2544 p4564 rpci, robust
eststo
reg robbery phg prison metpct ampct employ unrate gunprod p1524 p2544 p4564 rpci, robust
eststo
reg assault phg prison metpct ampct employ unrate gunprod p1524 p2544 p4564 rpci, robust
eststo
reg burglary phg prison metpct ampct employ unrate gunprod p1524 p2544 p4564 rpci, robust
eststo




*magazines: corr w/ guns 
corr hg pgs gun gunsamm amrmms amhms phg pgun






