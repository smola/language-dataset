* cert script for pdslasso/ivlasso/rlasso/CHS 1.0.03 MS 10feb2018
* uses 4 external datasets:
* 1. Levitt dataset from Belloni et al. J. of Econ. Persp. 28:2.
*    Ascii file levitt_ex.dat available online from journal publisher.
*    Replicates using pdslasso and rlasso.
* 2. BLP dataset from Belloni et al. Econometrica 2012, 80:6.
*    Original data available online from journal publisher.  Also used in
*    Chernozhukov et al., American Economic Review 2015, 105:5.
*    Assembled into Stata dataset separately and used here as BLP.dta.
*    Replicates using ivlasso and rlasso.
* 3. AJR dataset from Acemoglu et al. American Economic Review 2001, 91:5.
*    Original data available from economics.mit.edu; see ivlasso help file.
* 4. Stata online panel dataset abdata.
* Replicated results are inserted in this do file.
* #1 replicates results using original Stata code (modified slightly here).
* #2 replicates results from original Matlab code.
* #3 and #4 used for checking options etc.

cscript "rlasso/pdslasso/ivlasso" adofile rlasso pdslasso ivlasso lassoutils
clear all
capture log close
set more off
set rmsg on
program drop _all
log using cs_ivlasso,replace
about
which pdslasso
which ivlasso
which rlasso
which lassoutils


// ***************** LEVITT DATASET ************************* //

// Based on BCH JEP replication do file.
// Differences:
//   changed locals to globals (easier to check contents)
//   fixed typo in constructing initial differences
//   all generated vars are doubles
//   instead of dropping if trend==0, added "if trend>0" to original commands
//   use tweaked lassoShootingCBH instead of original lassoShooting
//   because of different varnames (FVs vs created by hand), coef vectors
//     are sorted before comparing

insheet using levitt_ex.dat, clear

* Drop DC, Alaska, and Hawaii
*drop if statenum == 9 | statenum == 2 | statenum == 12
* Drop DC
drop if statenum == 9

* Drop years not used
drop if year < 85 | year > 97

* Normalized trend variable
qui gen double trend = (year - 85)/12

// added
qui gen double trendsq=trend^2

tsset statenum year

xi i.year

* Generate variables for LASSO
replace xxincome = xxincome/100
replace xxpover = xxpover/100
replace xxafdc15 = xxafdc15/10000
replace xxbeer = xxbeer/100

global tdums	_Iyear_87 _Iyear_88 _Iyear_89 _Iyear_90 _Iyear_91 _Iyear_92 _Iyear_93 _Iyear_94 _Iyear_95 _Iyear_96 _Iyear_97
global xx		xxprison xxpolice xxunemp xxincome xxpover xxafdc15 xxgunlaw xxbeer

// Time dummies
sum $tdums
sum i.year

* Differences
global Dxx
foreach x of global xx {
	qui gen double D`x' = D.`x'
	local tempname = "D`x'"
	global Dxx : list global(Dxx) | tempname
}

sum $Dxx
sum D.($xx)

* Squared Differences
global Dxx2
foreach x of global Dxx {
	qui gen double `x'2 = `x'^2
	local tempname = "`x'2"
	global Dxx2 : list global(Dxx2) | tempname
}

* Difference Interactions
global DxxInt
local nxx : word count $Dxx
forvalues ii = 1/`nxx' {
	local start = `ii'+1
	forvalues jj = `start'/`nxx' {
		local temp1 : word `ii' of $Dxx
		local temp2 : word `jj' of $Dxx
		qui gen double `temp1'X`temp2' = `temp1'*`temp2'
		local tempname = "`temp1'X`temp2'"
		global DxxInt : list global(DxxInt) | tempname
	}
}		

* Lags
global Lxx
foreach x of global xx {
	qui gen double L`x' = L.`x'
	local tempname = "L`x'"
	global Lxx : list global(Lxx) | tempname
}

* Squared Lags
global Lxx2
foreach x of global Lxx {
	qui gen double `x'2 = `x'^2
	local tempname = "`x'2"
	global Lxx2 : list global(Lxx2) | tempname
}

* Means
global Mxx
foreach x of global xx {
	qui by statenum: egen double M`x' = mean(`x')
	local tempname = "M`x'"
	global Mxx : list global(Mxx) | tempname
}

* Squared Means
global Mxx2
foreach x of global Mxx {
	qui gen double `x'2 = `x'^2
	local tempname = "`x'2"
	global Mxx2 : list global(Mxx2) | tempname
}

* Initial Levels
local xx0
foreach x of global xx {
	qui by statenum: gen double `x'0 = `x'[1]
	local tempname = "`x'0"
	global xx0 : list global(xx0) | tempname
}

* Squared Initial Levels
global xx02
foreach x of global xx0 {
	qui gen double `x'2 = `x'^2
	local tempname = "`x'2"
	global xx02 : list global(xx02) | tempname
}

* Initial Differences
global Dxx0
foreach x of global Dxx {
	qui by statenum: gen double `x'0 = `x'[2]
	local tempname = "`x'0"
// typo in original code?
//	local xx0 : list local xx0 | tempname
	global Dxx0 : list global(Dxx0) | tempname
}

* Squared Initial Differences
global Dxx02
foreach x of global Dxx0 {
	qui gen double `x'2 = `x'^2
	local tempname = "`x'2"
	global Dxx02 : list global(Dxx02) | tempname
}

* Interactions with trends
global biglist : list global(Dxx) | global(Dxx2)
global biglist : list global(biglist) | global(DxxInt)
global biglist : list global(biglist) | global(Lxx)
global biglist : list global(biglist) | global(Lxx2)
global biglist : list global(biglist) | global(Mxx)
global biglist : list global(biglist) | global(Mxx2)
global biglist : list global(biglist) | global(xx0)
global biglist : list global(biglist) | global(xx02)
global biglist : list global(biglist) | global(Dxx0)
global biglist : list global(biglist) | global(Dxx02)

global IntT
local nxx : word count $biglist
foreach x of global biglist {
	qui gen double `x'Xt = `x'*trend
	qui gen double `x'Xt2 = `x'*(trend^2)
	local tempname = "`x'Xt `x'Xt2"
	global IntT : list global(IntT) | tempname
}
	
global shared : list global(biglist) | global(IntT)

* Violence specific controls
qui gen double Dviol = D.efaviol
qui by statenum: gen double viol0 = efaviol[1]
qui by statenum: gen double Dviol0 = Dviol[2]
qui gen double viol02 = viol0^2
qui gen double Dviol02 = Dviol0^2
qui gen double viol0Xt = viol0*trend
qui gen double viol0Xt2 = viol0*(trend^2)
qui gen double viol02Xt = viol02*trend
qui gen double viol02Xt2 = viol02*(trend^2)
qui gen double Dviol0Xt = Dviol0*trend
qui gen double Dviol0Xt2 = Dviol0*(trend^2)
qui gen double Dviol02Xt = Dviol02*trend
qui gen double Dviol02Xt2 = Dviol02*(trend^2)

global contviol		viol0 viol0Xt viol0Xt2 viol02 viol02Xt viol02Xt2 Dviol0 Dviol0Xt Dviol0Xt2 Dviol02 Dviol02Xt Dviol02Xt2

global AllViol : list global(contviol) | global(shared)
			
* Property specifc controls
qui gen double Dprop = D.efaprop
qui by statenum: gen double prop0 = efaprop[1]
qui by statenum: gen double Dprop0 = Dprop[2]
qui gen double prop02 = prop0^2
qui gen double Dprop02 = Dprop0^2
qui gen double prop0Xt = prop0*trend
qui gen double prop0Xt2 = prop0*(trend^2)
qui gen double prop02Xt = prop02*trend
qui gen double prop02Xt2 = prop02*(trend^2)
qui gen double Dprop0Xt = Dprop0*trend
qui gen double Dprop0Xt2 = Dprop0*(trend^2)
qui gen double Dprop02Xt = Dprop02*trend
qui gen double Dprop02Xt2 = Dprop02*(trend^2)

global contprop		prop0 prop0Xt prop0Xt2 prop02 prop02Xt prop02Xt2 Dprop0 Dprop0Xt Dprop0Xt2 Dprop02 Dprop02Xt Dprop02Xt2

global AllProp : list global(contprop) | global(shared)

* Murder specific controls
qui gen double Dmurd = D.efamurd
qui by statenum: gen double murd0 = efamurd[1]
qui by statenum: gen double Dmurd0 = Dmurd[2]
qui gen double murd02 = murd0^2
qui gen double Dmurd02 = Dmurd0^2
qui gen double murd0Xt = murd0*trend
qui gen double murd0Xt2 = murd0*(trend^2)
qui gen double murd02Xt = murd02*trend
qui gen double murd02Xt2 = murd02*(trend^2)
qui gen double Dmurd0Xt = Dmurd0*trend
qui gen double Dmurd0Xt2 = Dmurd0*(trend^2)
qui gen double Dmurd02Xt = Dmurd02*trend
qui gen double Dmurd02Xt2 = Dmurd02*(trend^2)

global contmurd		murd0 murd0Xt murd0Xt2 murd02 murd02Xt murd02Xt2 Dmurd0 Dmurd0Xt Dmurd0Xt2 Dmurd02 Dmurd02Xt Dmurd02Xt2

global AllMurd : list global(contmurd) | global(shared)

* Differenced outcomes
qui gen double Dyviol = D.lpc_viol
qui gen double Dyprop = D.lpc_prop
qui gen double Dymurd = D.lpc_murd			

// replaced with "if trend>0" below
// drop if trend == 0

*********************** Violence ************************************

* Variable selection

* Violence Outcome
rlasso D.lpc_viol													///
																	/// no trend
		c.viol0##c.viol0											///
		c.Dviol0##c.Dviol0											///
		c.(D.($xx))##c.(D.($xx))									/// note macro
		c.(L.xxprison)##c.(L.xxprison)								///
		c.(L.xxpolice)##c.(L.xxpolice)								///
		c.(L.xxunemp)##c.(L.xxunemp)								///
		c.(L.xxincome)##c.(L.xxincome)								///
		c.(L.xxpover)##c.(L.xxpover)								///
		c.(L.xxafdc15)##c.(L.xxafdc15)								///
		c.(L.xxgunlaw)##c.(L.xxgunlaw)								///
		c.(L.xxbeer)##c.(L.xxbeer)									///
		c.Mxxprison##c.Mxxprison									///
		c.Mxxpolice##c.Mxxpolice									///
		c.Mxxunemp##c.Mxxunemp										///
		c.Mxxincome##c.Mxxincome									///
		c.Mxxpover##c.Mxxpover										///
		c.Mxxafdc15##c.Mxxafdc15									///
		c.Mxxgunlaw##c.Mxxgunlaw									///
		c.Mxxbeer##c.Mxxbeer										///
		c.xxprison0##c.xxprison0									///
		c.xxpolice0##c.xxpolice0									///
		c.xxunemp0##c.xxunemp0										///
		c.xxincome0##c.xxincome0									///
		c.xxpover0##c.xxpover0										///
		c.xxafdc150##c.xxafdc150									///
		c.xxgunlaw0##c.xxgunlaw0									///
		c.xxbeer0##c.xxbeer0										///
		c.Dxxprison0##c.Dxxprison0									///
		c.Dxxpolice0##c.Dxxpolice0									///
		c.Dxxunemp0##c.Dxxunemp0									///
		c.Dxxincome0##c.Dxxincome0									///
		c.Dxxpover0##c.Dxxpover0									///
		c.Dxxafdc150##c.Dxxafdc150									///
		c.Dxxgunlaw0##c.Dxxgunlaw0									///
		c.Dxxbeer0##c.Dxxbeer0										///
																	///
		(c.viol0##c.viol0)#(c.trend##c.trend)						/// now interacted with trend
		(c.Dviol0##c.Dviol0)#(c.trend##c.trend)						///
		(c.(D.($xx))##c.(D.($xx)))#(c.trend##c.trend)				/// note macro
		(c.(L.xxprison)##c.(L.xxprison))#(c.trend##c.trend)			///
		(c.(L.xxpolice)##c.(L.xxpolice))#(c.trend##c.trend)			///
		(c.(L.xxunemp)##c.(L.xxunemp))#(c.trend##c.trend)			///
		(c.(L.xxincome)##c.(L.xxincome))#(c.trend##c.trend)			///
		(c.(L.xxpover)##c.(L.xxpover))#(c.trend##c.trend)			///
		(c.(L.xxafdc15)##c.(L.xxafdc15))#(c.trend##c.trend)			///
		(c.(L.xxgunlaw)##c.(L.xxgunlaw))#(c.trend##c.trend)			///
		(c.(L.xxbeer)##c.(L.xxbeer))#(c.trend##c.trend)				///
		(c.Mxxprison##c.Mxxprison)#(c.trend##c.trend)				///
		(c.Mxxpolice##c.Mxxpolice)#(c.trend##c.trend)				///
		(c.Mxxunemp##c.Mxxunemp	)#(c.trend##c.trend)				///
		(c.Mxxincome##c.Mxxincome)#(c.trend##c.trend)				///
		(c.Mxxpover##c.Mxxpover	)#(c.trend##c.trend)				///
		(c.Mxxafdc15##c.Mxxafdc15)#(c.trend##c.trend)				///
		(c.Mxxgunlaw##c.Mxxgunlaw)#(c.trend##c.trend)				///
		(c.Mxxbeer##c.Mxxbeer)#(c.trend##c.trend)					///
		(c.xxprison0##c.xxprison0)#(c.trend##c.trend)				///
		(c.xxpolice0##c.xxpolice0)#(c.trend##c.trend)				///
		(c.xxunemp0##c.xxunemp0)#(c.trend##c.trend)					///
		(c.xxincome0##c.xxincome0)#(c.trend##c.trend)				///
		(c.xxpover0##c.xxpover0)#(c.trend##c.trend)					///
		(c.xxafdc150##c.xxafdc150)#(c.trend##c.trend)				///
		(c.xxgunlaw0##c.xxgunlaw0)#(c.trend##c.trend)				///
		(c.xxbeer0##c.xxbeer0)#(c.trend##c.trend)					///
		(c.Dxxprison0##c.Dxxprison0)#(c.trend##c.trend)				///
		(c.Dxxpolice0##c.Dxxpolice0)#(c.trend##c.trend)				///
		(c.Dxxunemp0##c.Dxxunemp0)#(c.trend##c.trend)				///
		(c.Dxxincome0##c.Dxxincome0)#(c.trend##c.trend)				///
		(c.Dxxpover0##c.Dxxpover0)#(c.trend##c.trend)				///
		(c.Dxxafdc150##c.Dxxafdc150)#(c.trend##c.trend)				///
		(c.Dxxgunlaw0##c.Dxxgunlaw0)#(c.trend##c.trend)				///
		(c.Dxxbeer0##c.Dxxbeer0	)#(c.trend##c.trend)				///
		i.year, partial(i.year) robust lalt corrnum(0) maxupsiter(100)
di "p=" e(p)
global yvSelrlasso `e(selected)'
di "$yvSelrlasso"
mat b_rlasso=e(b)
if `e(s)' {
	mat b_rlasso=b_rlasso[1,1..`e(s)']
}
else {
	mat b_rlasso=.
}

/*
* Violence Outcome
lassoShootingCBH Dyviol $AllViol if trend>0, controls($tdums) lasiter(100) verbose(0) fdisplay(1)
// no vars selected
di "p=" r(p)
global yvSel `r(selected)'
di "$yvSel"
mat b_cbh=r(betaL)
mat b_cbh=b_cbh'
*/
global yvSel
mat b_cbh=.

// sort cols and compare
mata: st_matrix("b_rlasso",sort(st_matrix("b_rlasso")',1)')
mata: st_matrix("b_cbh",sort(st_matrix("b_cbh")',1)')
assert mreldif(b_rlasso,b_cbh)<1e-4

* Violence Abortion
rlasso D.efaviol													///
																	/// no trend
		c.viol0##c.viol0											///
		c.Dviol0##c.Dviol0											///
		c.(D.($xx))##c.(D.($xx))									/// note macro
		c.(L.xxprison)##c.(L.xxprison)								///
		c.(L.xxpolice)##c.(L.xxpolice)								///
		c.(L.xxunemp)##c.(L.xxunemp)								///
		c.(L.xxincome)##c.(L.xxincome)								///
		c.(L.xxpover)##c.(L.xxpover)								///
		c.(L.xxafdc15)##c.(L.xxafdc15)								///
		c.(L.xxgunlaw)##c.(L.xxgunlaw)								///
		c.(L.xxbeer)##c.(L.xxbeer)									///
		c.Mxxprison##c.Mxxprison									///
		c.Mxxpolice##c.Mxxpolice									///
		c.Mxxunemp##c.Mxxunemp										///
		c.Mxxincome##c.Mxxincome									///
		c.Mxxpover##c.Mxxpover										///
		c.Mxxafdc15##c.Mxxafdc15									///
		c.Mxxgunlaw##c.Mxxgunlaw									///
		c.Mxxbeer##c.Mxxbeer										///
		c.xxprison0##c.xxprison0									///
		c.xxpolice0##c.xxpolice0									///
		c.xxunemp0##c.xxunemp0										///
		c.xxincome0##c.xxincome0									///
		c.xxpover0##c.xxpover0										///
		c.xxafdc150##c.xxafdc150									///
		c.xxgunlaw0##c.xxgunlaw0									///
		c.xxbeer0##c.xxbeer0										///
		c.Dxxprison0##c.Dxxprison0									///
		c.Dxxpolice0##c.Dxxpolice0									///
		c.Dxxunemp0##c.Dxxunemp0									///
		c.Dxxincome0##c.Dxxincome0									///
		c.Dxxpover0##c.Dxxpover0									///
		c.Dxxafdc150##c.Dxxafdc150									///
		c.Dxxgunlaw0##c.Dxxgunlaw0									///
		c.Dxxbeer0##c.Dxxbeer0										///
																	///
		(c.viol0##c.viol0)#(c.trend##c.trend)						/// now interacted with trend
		(c.Dviol0##c.Dviol0)#(c.trend##c.trend)						///
		(c.(D.($xx))##c.(D.($xx)))#(c.trend##c.trend)				/// note macro
		(c.(L.xxprison)##c.(L.xxprison))#(c.trend##c.trend)			///
		(c.(L.xxpolice)##c.(L.xxpolice))#(c.trend##c.trend)			///
		(c.(L.xxunemp)##c.(L.xxunemp))#(c.trend##c.trend)			///
		(c.(L.xxincome)##c.(L.xxincome))#(c.trend##c.trend)			///
		(c.(L.xxpover)##c.(L.xxpover))#(c.trend##c.trend)			///
		(c.(L.xxafdc15)##c.(L.xxafdc15))#(c.trend##c.trend)			///
		(c.(L.xxgunlaw)##c.(L.xxgunlaw))#(c.trend##c.trend)			///
		(c.(L.xxbeer)##c.(L.xxbeer))#(c.trend##c.trend)				///
		(c.Mxxprison##c.Mxxprison)#(c.trend##c.trend)				///
		(c.Mxxpolice##c.Mxxpolice)#(c.trend##c.trend)				///
		(c.Mxxunemp##c.Mxxunemp	)#(c.trend##c.trend)				///
		(c.Mxxincome##c.Mxxincome)#(c.trend##c.trend)				///
		(c.Mxxpover##c.Mxxpover	)#(c.trend##c.trend)				///
		(c.Mxxafdc15##c.Mxxafdc15)#(c.trend##c.trend)				///
		(c.Mxxgunlaw##c.Mxxgunlaw)#(c.trend##c.trend)				///
		(c.Mxxbeer##c.Mxxbeer)#(c.trend##c.trend)					///
		(c.xxprison0##c.xxprison0)#(c.trend##c.trend)				///
		(c.xxpolice0##c.xxpolice0)#(c.trend##c.trend)				///
		(c.xxunemp0##c.xxunemp0)#(c.trend##c.trend)					///
		(c.xxincome0##c.xxincome0)#(c.trend##c.trend)				///
		(c.xxpover0##c.xxpover0)#(c.trend##c.trend)					///
		(c.xxafdc150##c.xxafdc150)#(c.trend##c.trend)				///
		(c.xxgunlaw0##c.xxgunlaw0)#(c.trend##c.trend)				///
		(c.xxbeer0##c.xxbeer0)#(c.trend##c.trend)					///
		(c.Dxxprison0##c.Dxxprison0)#(c.trend##c.trend)				///
		(c.Dxxpolice0##c.Dxxpolice0)#(c.trend##c.trend)				///
		(c.Dxxunemp0##c.Dxxunemp0)#(c.trend##c.trend)				///
		(c.Dxxincome0##c.Dxxincome0)#(c.trend##c.trend)				///
		(c.Dxxpover0##c.Dxxpover0)#(c.trend##c.trend)				///
		(c.Dxxafdc150##c.Dxxafdc150)#(c.trend##c.trend)				///
		(c.Dxxgunlaw0##c.Dxxgunlaw0)#(c.trend##c.trend)				///
		(c.Dxxbeer0##c.Dxxbeer0	)#(c.trend##c.trend)				///
		i.year, partial(i.year) robust lalt corrnum(0) maxupsiter(100)
di "p=" e(p)
global xvSelrlasso `e(selected)'
di "$xvSelrlasso"
mat b_rlasso=e(b)
if `e(s)' {
	mat b_rlasso=b_rlasso[1,1..`e(s)']
}
else {
	mat b_rlasso=.
}

/*
* Violence Abortion
lassoShootingCBH Dviol $AllViol if trend>0, controls($tdums) lasiter(100) verbose(0) fdisplay(1)
di "p=" r(p)
global xvSel `r(selected)'
di "$xvSel"
mat b_cbh=r(betaL)
mat b_cbh=b_cbh'
*/
global xvSel viol0 Lxxprison Lxxpolice Mxxincome Dxxincome0 LxxpoliceXt MxxincomeXt Dxxincome0Xt Dxxbeer0Xt
mat b_cbh = .65842111 , .00346113 , .01813694 , 3.5867181 , 5.6965391 , .00434867 , 23.924844 , 30.420403 , .5716038

// sort cols and compare
mata: st_matrix("b_rlasso",sort(st_matrix("b_rlasso")',1)')
mata: st_matrix("b_cbh",sort(st_matrix("b_cbh")',1)')
assert mreldif(b_rlasso,b_cbh)<1e-4


* Violence equation with selected controls
pdslasso D.lpc_viol D.efaviol										///
		(															/// no trend
		c.viol0##c.viol0											///
		c.Dviol0##c.Dviol0											///
		c.(D.($xx))##c.(D.($xx))									/// note macro
		c.(L.xxprison)##c.(L.xxprison)								///
		c.(L.xxpolice)##c.(L.xxpolice)								///
		c.(L.xxunemp)##c.(L.xxunemp)								///
		c.(L.xxincome)##c.(L.xxincome)								///
		c.(L.xxpover)##c.(L.xxpover)								///
		c.(L.xxafdc15)##c.(L.xxafdc15)								///
		c.(L.xxgunlaw)##c.(L.xxgunlaw)								///
		c.(L.xxbeer)##c.(L.xxbeer)									///
		c.Mxxprison##c.Mxxprison									///
		c.Mxxpolice##c.Mxxpolice									///
		c.Mxxunemp##c.Mxxunemp										///
		c.Mxxincome##c.Mxxincome									///
		c.Mxxpover##c.Mxxpover										///
		c.Mxxafdc15##c.Mxxafdc15									///
		c.Mxxgunlaw##c.Mxxgunlaw									///
		c.Mxxbeer##c.Mxxbeer										///
		c.xxprison0##c.xxprison0									///
		c.xxpolice0##c.xxpolice0									///
		c.xxunemp0##c.xxunemp0										///
		c.xxincome0##c.xxincome0									///
		c.xxpover0##c.xxpover0										///
		c.xxafdc150##c.xxafdc150									///
		c.xxgunlaw0##c.xxgunlaw0									///
		c.xxbeer0##c.xxbeer0										///
		c.Dxxprison0##c.Dxxprison0									///
		c.Dxxpolice0##c.Dxxpolice0									///
		c.Dxxunemp0##c.Dxxunemp0									///
		c.Dxxincome0##c.Dxxincome0									///
		c.Dxxpover0##c.Dxxpover0									///
		c.Dxxafdc150##c.Dxxafdc150									///
		c.Dxxgunlaw0##c.Dxxgunlaw0									///
		c.Dxxbeer0##c.Dxxbeer0										///
																	///
		(c.viol0##c.viol0)#(c.trend##c.trend)						/// now interacted with trend
		(c.Dviol0##c.Dviol0)#(c.trend##c.trend)						///
		(c.(D.($xx))##c.(D.($xx)))#(c.trend##c.trend)				/// note macro
		(c.(L.xxprison)##c.(L.xxprison))#(c.trend##c.trend)			///
		(c.(L.xxpolice)##c.(L.xxpolice))#(c.trend##c.trend)			///
		(c.(L.xxunemp)##c.(L.xxunemp))#(c.trend##c.trend)			///
		(c.(L.xxincome)##c.(L.xxincome))#(c.trend##c.trend)			///
		(c.(L.xxpover)##c.(L.xxpover))#(c.trend##c.trend)			///
		(c.(L.xxafdc15)##c.(L.xxafdc15))#(c.trend##c.trend)			///
		(c.(L.xxgunlaw)##c.(L.xxgunlaw))#(c.trend##c.trend)			///
		(c.(L.xxbeer)##c.(L.xxbeer))#(c.trend##c.trend)				///
		(c.Mxxprison##c.Mxxprison)#(c.trend##c.trend)				///
		(c.Mxxpolice##c.Mxxpolice)#(c.trend##c.trend)				///
		(c.Mxxunemp##c.Mxxunemp	)#(c.trend##c.trend)				///
		(c.Mxxincome##c.Mxxincome)#(c.trend##c.trend)				///
		(c.Mxxpover##c.Mxxpover	)#(c.trend##c.trend)				///
		(c.Mxxafdc15##c.Mxxafdc15)#(c.trend##c.trend)				///
		(c.Mxxgunlaw##c.Mxxgunlaw)#(c.trend##c.trend)				///
		(c.Mxxbeer##c.Mxxbeer)#(c.trend##c.trend)					///
		(c.xxprison0##c.xxprison0)#(c.trend##c.trend)				///
		(c.xxpolice0##c.xxpolice0)#(c.trend##c.trend)				///
		(c.xxunemp0##c.xxunemp0)#(c.trend##c.trend)					///
		(c.xxincome0##c.xxincome0)#(c.trend##c.trend)				///
		(c.xxpover0##c.xxpover0)#(c.trend##c.trend)					///
		(c.xxafdc150##c.xxafdc150)#(c.trend##c.trend)				///
		(c.xxgunlaw0##c.xxgunlaw0)#(c.trend##c.trend)				///
		(c.xxbeer0##c.xxbeer0)#(c.trend##c.trend)					///
		(c.Dxxprison0##c.Dxxprison0)#(c.trend##c.trend)				///
		(c.Dxxpolice0##c.Dxxpolice0)#(c.trend##c.trend)				///
		(c.Dxxunemp0##c.Dxxunemp0)#(c.trend##c.trend)				///
		(c.Dxxincome0##c.Dxxincome0)#(c.trend##c.trend)				///
		(c.Dxxpover0##c.Dxxpover0)#(c.trend##c.trend)				///
		(c.Dxxafdc150##c.Dxxafdc150)#(c.trend##c.trend)				///
		(c.Dxxgunlaw0##c.Dxxgunlaw0)#(c.trend##c.trend)				///
		(c.Dxxbeer0##c.Dxxbeer0	)#(c.trend##c.trend)				///
		i.year														///
		)															///
		, partial(i.year) robust lopt(lalt corrnum(0) maxupsiter(100))
mat b_pdslasso=e(b)

* Get union of selected instruments
global vDS : list global(yvSel) | global(xvSel)

* Violence equation with selected controls
// Original - clustered SEs (but not cluster lasso)
// reg Dyviol Dviol $vDS $tdums if trend>0, cluster(statenum)
// robust, no cluster
reg Dyviol Dviol $vDS $tdums if trend>0, robust
mat b_regress=e(b)

// sort cols and compare
// may be diff lengths and include 0s (for dropped base vars)
// so compare abs values > 0
mata: st_matrix("b_pdslasso",abs(sort(-abs(st_matrix("b_pdslasso"))',1))')
mata: st_matrix("b_regress",abs(sort(-abs(st_matrix("b_regress"))',1))')
local k1 = colsof(b_pdslasso)
local k2 = colsof(b_regress)
mat b_pdslasso = b_pdslasso[1,1..min(`k1',`k2')]
mat b_regress = b_regress[1,1..min(`k1',`k2')]
assert mreldif(b_pdslasso,b_regress)<1e-6

******************************** Property ******************************

* Property Outcome
// need low tol settings in order to match
rlasso D.lpc_prop													///
																	/// no trend
		c.prop0##c.prop0											///
		c.Dprop0##c.Dprop0											///
		c.(D.($xx))##c.(D.($xx))									/// note macro
		c.(L.xxprison)##c.(L.xxprison)								///
		c.(L.xxpolice)##c.(L.xxpolice)								///
		c.(L.xxunemp)##c.(L.xxunemp)								///
		c.(L.xxincome)##c.(L.xxincome)								///
		c.(L.xxpover)##c.(L.xxpover)								///
		c.(L.xxafdc15)##c.(L.xxafdc15)								///
		c.(L.xxgunlaw)##c.(L.xxgunlaw)								///
		c.(L.xxbeer)##c.(L.xxbeer)									///
		c.Mxxprison##c.Mxxprison									///
		c.Mxxpolice##c.Mxxpolice									///
		c.Mxxunemp##c.Mxxunemp										///
		c.Mxxincome##c.Mxxincome									///
		c.Mxxpover##c.Mxxpover										///
		c.Mxxafdc15##c.Mxxafdc15									///
		c.Mxxgunlaw##c.Mxxgunlaw									///
		c.Mxxbeer##c.Mxxbeer										///
		c.xxprison0##c.xxprison0									///
		c.xxpolice0##c.xxpolice0									///
		c.xxunemp0##c.xxunemp0										///
		c.xxincome0##c.xxincome0									///
		c.xxpover0##c.xxpover0										///
		c.xxafdc150##c.xxafdc150									///
		c.xxgunlaw0##c.xxgunlaw0									///
		c.xxbeer0##c.xxbeer0										///
		c.Dxxprison0##c.Dxxprison0									///
		c.Dxxpolice0##c.Dxxpolice0									///
		c.Dxxunemp0##c.Dxxunemp0									///
		c.Dxxincome0##c.Dxxincome0									///
		c.Dxxpover0##c.Dxxpover0									///
		c.Dxxafdc150##c.Dxxafdc150									///
		c.Dxxgunlaw0##c.Dxxgunlaw0									///
		c.Dxxbeer0##c.Dxxbeer0										///
																	///
		(c.prop0##c.prop0)#(c.trend##c.trend)						/// now interacted with trend
		(c.Dprop0##c.Dprop0)#(c.trend##c.trend)						///
		(c.(D.($xx))##c.(D.($xx)))#(c.trend##c.trend)				/// note macro
		(c.(L.xxprison)##c.(L.xxprison))#(c.trend##c.trend)			///
		(c.(L.xxpolice)##c.(L.xxpolice))#(c.trend##c.trend)			///
		(c.(L.xxunemp)##c.(L.xxunemp))#(c.trend##c.trend)			///
		(c.(L.xxincome)##c.(L.xxincome))#(c.trend##c.trend)			///
		(c.(L.xxpover)##c.(L.xxpover))#(c.trend##c.trend)			///
		(c.(L.xxafdc15)##c.(L.xxafdc15))#(c.trend##c.trend)			///
		(c.(L.xxgunlaw)##c.(L.xxgunlaw))#(c.trend##c.trend)			///
		(c.(L.xxbeer)##c.(L.xxbeer))#(c.trend##c.trend)				///
		(c.Mxxprison##c.Mxxprison)#(c.trend##c.trend)				///
		(c.Mxxpolice##c.Mxxpolice)#(c.trend##c.trend)				///
		(c.Mxxunemp##c.Mxxunemp	)#(c.trend##c.trend)				///
		(c.Mxxincome##c.Mxxincome)#(c.trend##c.trend)				///
		(c.Mxxpover##c.Mxxpover	)#(c.trend##c.trend)				///
		(c.Mxxafdc15##c.Mxxafdc15)#(c.trend##c.trend)				///
		(c.Mxxgunlaw##c.Mxxgunlaw)#(c.trend##c.trend)				///
		(c.Mxxbeer##c.Mxxbeer)#(c.trend##c.trend)					///
		(c.xxprison0##c.xxprison0)#(c.trend##c.trend)				///
		(c.xxpolice0##c.xxpolice0)#(c.trend##c.trend)				///
		(c.xxunemp0##c.xxunemp0)#(c.trend##c.trend)					///
		(c.xxincome0##c.xxincome0)#(c.trend##c.trend)				///
		(c.xxpover0##c.xxpover0)#(c.trend##c.trend)					///
		(c.xxafdc150##c.xxafdc150)#(c.trend##c.trend)				///
		(c.xxgunlaw0##c.xxgunlaw0)#(c.trend##c.trend)				///
		(c.xxbeer0##c.xxbeer0)#(c.trend##c.trend)					///
		(c.Dxxprison0##c.Dxxprison0)#(c.trend##c.trend)				///
		(c.Dxxpolice0##c.Dxxpolice0)#(c.trend##c.trend)				///
		(c.Dxxunemp0##c.Dxxunemp0)#(c.trend##c.trend)				///
		(c.Dxxincome0##c.Dxxincome0)#(c.trend##c.trend)				///
		(c.Dxxpover0##c.Dxxpover0)#(c.trend##c.trend)				///
		(c.Dxxafdc150##c.Dxxafdc150)#(c.trend##c.trend)				///
		(c.Dxxgunlaw0##c.Dxxgunlaw0)#(c.trend##c.trend)				///
		(c.Dxxbeer0##c.Dxxbeer0	)#(c.trend##c.trend)				///
		i.year, partial(i.year) robust lalt corrnum(0) maxupsiter(100) tolopt(1e-10) tolzero(1e-10) tolups(1e-10)
di "p=" e(p)
global ypSelrlasso `e(selected)'
di "$ypSelrlasso"
mat b_rlasso=e(b)
if `e(s)' {
	mat b_rlasso=b_rlasso[1,1..`e(s)']
}
else {
	mat b_rlasso=.
}

* Property Outcome
/*
lassoShootingCBH Dyprop $AllProp if trend>0, controls($tdums) lasiter(100) verbose(0) fdisplay(1) ltol(1e-10) tolups(1e-10) tolzero(1e-10)
di "p=" r(p)
global ypSel `r(selected)'
di "$ypSel"
mat b_cbh=r(betaL)
mat b_cbh=b_cbh'
*/
global ypSel Mxxincome2Xt2 xxincome02Xt2
mat b_cbh = -11.179967 , -.91841177

// sort cols and compare
mata: st_matrix("b_rlasso",sort(st_matrix("b_rlasso")',1)')
mata: st_matrix("b_cbh",sort(st_matrix("b_cbh")',1)')
assert mreldif(b_rlasso,b_cbh)<1e-4

* Property Abortion
rlasso D.efaprop													///
																	/// no trend
		c.prop0##c.prop0											///
		c.Dprop0##c.Dprop0											///
		c.(D.($xx))##c.(D.($xx))									/// note macro
		c.(L.xxprison)##c.(L.xxprison)								///
		c.(L.xxpolice)##c.(L.xxpolice)								///
		c.(L.xxunemp)##c.(L.xxunemp)								///
		c.(L.xxincome)##c.(L.xxincome)								///
		c.(L.xxpover)##c.(L.xxpover)								///
		c.(L.xxafdc15)##c.(L.xxafdc15)								///
		c.(L.xxgunlaw)##c.(L.xxgunlaw)								///
		c.(L.xxbeer)##c.(L.xxbeer)									///
		c.Mxxprison##c.Mxxprison									///
		c.Mxxpolice##c.Mxxpolice									///
		c.Mxxunemp##c.Mxxunemp										///
		c.Mxxincome##c.Mxxincome									///
		c.Mxxpover##c.Mxxpover										///
		c.Mxxafdc15##c.Mxxafdc15									///
		c.Mxxgunlaw##c.Mxxgunlaw									///
		c.Mxxbeer##c.Mxxbeer										///
		c.xxprison0##c.xxprison0									///
		c.xxpolice0##c.xxpolice0									///
		c.xxunemp0##c.xxunemp0										///
		c.xxincome0##c.xxincome0									///
		c.xxpover0##c.xxpover0										///
		c.xxafdc150##c.xxafdc150									///
		c.xxgunlaw0##c.xxgunlaw0									///
		c.xxbeer0##c.xxbeer0										///
		c.Dxxprison0##c.Dxxprison0									///
		c.Dxxpolice0##c.Dxxpolice0									///
		c.Dxxunemp0##c.Dxxunemp0									///
		c.Dxxincome0##c.Dxxincome0									///
		c.Dxxpover0##c.Dxxpover0									///
		c.Dxxafdc150##c.Dxxafdc150									///
		c.Dxxgunlaw0##c.Dxxgunlaw0									///
		c.Dxxbeer0##c.Dxxbeer0										///
																	///
		(c.prop0##c.prop0)#(c.trend##c.trend)						/// now interacted with trend
		(c.Dprop0##c.Dprop0)#(c.trend##c.trend)						///
		(c.(D.($xx))##c.(D.($xx)))#(c.trend##c.trend)				/// note macro
		(c.(L.xxprison)##c.(L.xxprison))#(c.trend##c.trend)			///
		(c.(L.xxpolice)##c.(L.xxpolice))#(c.trend##c.trend)			///
		(c.(L.xxunemp)##c.(L.xxunemp))#(c.trend##c.trend)			///
		(c.(L.xxincome)##c.(L.xxincome))#(c.trend##c.trend)			///
		(c.(L.xxpover)##c.(L.xxpover))#(c.trend##c.trend)			///
		(c.(L.xxafdc15)##c.(L.xxafdc15))#(c.trend##c.trend)			///
		(c.(L.xxgunlaw)##c.(L.xxgunlaw))#(c.trend##c.trend)			///
		(c.(L.xxbeer)##c.(L.xxbeer))#(c.trend##c.trend)				///
		(c.Mxxprison##c.Mxxprison)#(c.trend##c.trend)				///
		(c.Mxxpolice##c.Mxxpolice)#(c.trend##c.trend)				///
		(c.Mxxunemp##c.Mxxunemp	)#(c.trend##c.trend)				///
		(c.Mxxincome##c.Mxxincome)#(c.trend##c.trend)				///
		(c.Mxxpover##c.Mxxpover	)#(c.trend##c.trend)				///
		(c.Mxxafdc15##c.Mxxafdc15)#(c.trend##c.trend)				///
		(c.Mxxgunlaw##c.Mxxgunlaw)#(c.trend##c.trend)				///
		(c.Mxxbeer##c.Mxxbeer)#(c.trend##c.trend)					///
		(c.xxprison0##c.xxprison0)#(c.trend##c.trend)				///
		(c.xxpolice0##c.xxpolice0)#(c.trend##c.trend)				///
		(c.xxunemp0##c.xxunemp0)#(c.trend##c.trend)					///
		(c.xxincome0##c.xxincome0)#(c.trend##c.trend)				///
		(c.xxpover0##c.xxpover0)#(c.trend##c.trend)					///
		(c.xxafdc150##c.xxafdc150)#(c.trend##c.trend)				///
		(c.xxgunlaw0##c.xxgunlaw0)#(c.trend##c.trend)				///
		(c.xxbeer0##c.xxbeer0)#(c.trend##c.trend)					///
		(c.Dxxprison0##c.Dxxprison0)#(c.trend##c.trend)				///
		(c.Dxxpolice0##c.Dxxpolice0)#(c.trend##c.trend)				///
		(c.Dxxunemp0##c.Dxxunemp0)#(c.trend##c.trend)				///
		(c.Dxxincome0##c.Dxxincome0)#(c.trend##c.trend)				///
		(c.Dxxpover0##c.Dxxpover0)#(c.trend##c.trend)				///
		(c.Dxxafdc150##c.Dxxafdc150)#(c.trend##c.trend)				///
		(c.Dxxgunlaw0##c.Dxxgunlaw0)#(c.trend##c.trend)				///
		(c.Dxxbeer0##c.Dxxbeer0	)#(c.trend##c.trend)				///
		i.year, partial(i.year) robust lalt corrnum(0) maxupsiter(100)
di "p=" e(p)
global ypSelrlasso `e(selected)'
di "$xpSelrlasso"
mat b_rlasso=e(b)
if `e(s)' {
	mat b_rlasso=b_rlasso[1,1..`e(s)']
}
else {
	mat b_rlasso=.
}

* Property Abortion
/*
lassoShootingCBH Dprop $AllProp if trend>0, controls($tdums) lasiter(100) verbose(0) fdisplay(1)
global xpSel `r(selected)'
di "$xpSel"
mat b_cbh=r(betaL)
mat b_cbh=b_cbh'
*/
global xpSel prop0 Lxxprison Lxxpolice Lxxincome Mxxincome Dxxincome0 Dxxincome0Xt
mat b_cbh = .09225667 , .00802776 , .02081152 , 7.955133 , 16.997832 , 24.938141 , 28.140729

// sort cols and compare
mata: st_matrix("b_rlasso",sort(st_matrix("b_rlasso")',1)')
mata: st_matrix("b_cbh",sort(st_matrix("b_cbh")',1)')
assert mreldif(b_rlasso,b_cbh)<1e-4

* Property equation with selected controls
pdslasso D.lpc_prop D.efaprop										///
		(															/// no trend
		c.prop0##c.prop0											///
		c.Dprop0##c.Dprop0											///
		c.(D.($xx))##c.(D.($xx))									/// note macro
		c.(L.xxprison)##c.(L.xxprison)								///
		c.(L.xxpolice)##c.(L.xxpolice)								///
		c.(L.xxunemp)##c.(L.xxunemp)								///
		c.(L.xxincome)##c.(L.xxincome)								///
		c.(L.xxpover)##c.(L.xxpover)								///
		c.(L.xxafdc15)##c.(L.xxafdc15)								///
		c.(L.xxgunlaw)##c.(L.xxgunlaw)								///
		c.(L.xxbeer)##c.(L.xxbeer)									///
		c.Mxxprison##c.Mxxprison									///
		c.Mxxpolice##c.Mxxpolice									///
		c.Mxxunemp##c.Mxxunemp										///
		c.Mxxincome##c.Mxxincome									///
		c.Mxxpover##c.Mxxpover										///
		c.Mxxafdc15##c.Mxxafdc15									///
		c.Mxxgunlaw##c.Mxxgunlaw									///
		c.Mxxbeer##c.Mxxbeer										///
		c.xxprison0##c.xxprison0									///
		c.xxpolice0##c.xxpolice0									///
		c.xxunemp0##c.xxunemp0										///
		c.xxincome0##c.xxincome0									///
		c.xxpover0##c.xxpover0										///
		c.xxafdc150##c.xxafdc150									///
		c.xxgunlaw0##c.xxgunlaw0									///
		c.xxbeer0##c.xxbeer0										///
		c.Dxxprison0##c.Dxxprison0									///
		c.Dxxpolice0##c.Dxxpolice0									///
		c.Dxxunemp0##c.Dxxunemp0									///
		c.Dxxincome0##c.Dxxincome0									///
		c.Dxxpover0##c.Dxxpover0									///
		c.Dxxafdc150##c.Dxxafdc150									///
		c.Dxxgunlaw0##c.Dxxgunlaw0									///
		c.Dxxbeer0##c.Dxxbeer0										///
																	///
		(c.prop0##c.prop0)#(c.trend##c.trend)						/// now interacted with trend
		(c.Dprop0##c.Dprop0)#(c.trend##c.trend)						///
		(c.(D.($xx))##c.(D.($xx)))#(c.trend##c.trend)				/// note macro
		(c.(L.xxprison)##c.(L.xxprison))#(c.trend##c.trend)			///
		(c.(L.xxpolice)##c.(L.xxpolice))#(c.trend##c.trend)			///
		(c.(L.xxunemp)##c.(L.xxunemp))#(c.trend##c.trend)			///
		(c.(L.xxincome)##c.(L.xxincome))#(c.trend##c.trend)			///
		(c.(L.xxpover)##c.(L.xxpover))#(c.trend##c.trend)			///
		(c.(L.xxafdc15)##c.(L.xxafdc15))#(c.trend##c.trend)			///
		(c.(L.xxgunlaw)##c.(L.xxgunlaw))#(c.trend##c.trend)			///
		(c.(L.xxbeer)##c.(L.xxbeer))#(c.trend##c.trend)				///
		(c.Mxxprison##c.Mxxprison)#(c.trend##c.trend)				///
		(c.Mxxpolice##c.Mxxpolice)#(c.trend##c.trend)				///
		(c.Mxxunemp##c.Mxxunemp	)#(c.trend##c.trend)				///
		(c.Mxxincome##c.Mxxincome)#(c.trend##c.trend)				///
		(c.Mxxpover##c.Mxxpover	)#(c.trend##c.trend)				///
		(c.Mxxafdc15##c.Mxxafdc15)#(c.trend##c.trend)				///
		(c.Mxxgunlaw##c.Mxxgunlaw)#(c.trend##c.trend)				///
		(c.Mxxbeer##c.Mxxbeer)#(c.trend##c.trend)					///
		(c.xxprison0##c.xxprison0)#(c.trend##c.trend)				///
		(c.xxpolice0##c.xxpolice0)#(c.trend##c.trend)				///
		(c.xxunemp0##c.xxunemp0)#(c.trend##c.trend)					///
		(c.xxincome0##c.xxincome0)#(c.trend##c.trend)				///
		(c.xxpover0##c.xxpover0)#(c.trend##c.trend)					///
		(c.xxafdc150##c.xxafdc150)#(c.trend##c.trend)				///
		(c.xxgunlaw0##c.xxgunlaw0)#(c.trend##c.trend)				///
		(c.xxbeer0##c.xxbeer0)#(c.trend##c.trend)					///
		(c.Dxxprison0##c.Dxxprison0)#(c.trend##c.trend)				///
		(c.Dxxpolice0##c.Dxxpolice0)#(c.trend##c.trend)				///
		(c.Dxxunemp0##c.Dxxunemp0)#(c.trend##c.trend)				///
		(c.Dxxincome0##c.Dxxincome0)#(c.trend##c.trend)				///
		(c.Dxxpover0##c.Dxxpover0)#(c.trend##c.trend)				///
		(c.Dxxafdc150##c.Dxxafdc150)#(c.trend##c.trend)				///
		(c.Dxxgunlaw0##c.Dxxgunlaw0)#(c.trend##c.trend)				///
		(c.Dxxbeer0##c.Dxxbeer0	)#(c.trend##c.trend)				///
		i.year														///
		)															///
		, partial(i.year) robust lopt(lalt corrnum(0) maxupsiter(100))
mat b_pdslasso=e(b)

* Get union of selected instruments
global pDS : list global(ypSel) | global(xpSel)

* Property equation with selected controls
// Original - clustered SEs (but not cluster lasso)
// reg Dyprop Dprop $pDS $tdums if trend>0, cluster(statenum)
// robust, no cluster
reg Dyprop Dprop $pDS $tdums if trend>0, robust
mat b_regress=e(b)

// sort cols and compare
// may be diff lengths and include 0s (for dropped base vars)
// so compare abs values > 0
mata: st_matrix("b_pdslasso",abs(sort(-abs(st_matrix("b_pdslasso"))',1))')
mata: st_matrix("b_regress",abs(sort(-abs(st_matrix("b_regress"))',1))')
local k1 = colsof(b_pdslasso)
local k2 = colsof(b_regress)
mat b_pdslasso = b_pdslasso[1,1..min(`k1',`k2')]
mat b_regress = b_regress[1,1..min(`k1',`k2')]
assert mreldif(b_pdslasso,b_regress)<1e-6

********************************* Murder ***********************************

* Violence Outcome
rlasso D.lpc_murd													///
																	/// no trend
		c.murd0##c.murd0											///
		c.Dmurd0##c.Dmurd0											///
		c.(D.($xx))##c.(D.($xx))									/// note macro
		c.(L.xxprison)##c.(L.xxprison)								///
		c.(L.xxpolice)##c.(L.xxpolice)								///
		c.(L.xxunemp)##c.(L.xxunemp)								///
		c.(L.xxincome)##c.(L.xxincome)								///
		c.(L.xxpover)##c.(L.xxpover)								///
		c.(L.xxafdc15)##c.(L.xxafdc15)								///
		c.(L.xxgunlaw)##c.(L.xxgunlaw)								///
		c.(L.xxbeer)##c.(L.xxbeer)									///
		c.Mxxprison##c.Mxxprison									///
		c.Mxxpolice##c.Mxxpolice									///
		c.Mxxunemp##c.Mxxunemp										///
		c.Mxxincome##c.Mxxincome									///
		c.Mxxpover##c.Mxxpover										///
		c.Mxxafdc15##c.Mxxafdc15									///
		c.Mxxgunlaw##c.Mxxgunlaw									///
		c.Mxxbeer##c.Mxxbeer										///
		c.xxprison0##c.xxprison0									///
		c.xxpolice0##c.xxpolice0									///
		c.xxunemp0##c.xxunemp0										///
		c.xxincome0##c.xxincome0									///
		c.xxpover0##c.xxpover0										///
		c.xxafdc150##c.xxafdc150									///
		c.xxgunlaw0##c.xxgunlaw0									///
		c.xxbeer0##c.xxbeer0										///
		c.Dxxprison0##c.Dxxprison0									///
		c.Dxxpolice0##c.Dxxpolice0									///
		c.Dxxunemp0##c.Dxxunemp0									///
		c.Dxxincome0##c.Dxxincome0									///
		c.Dxxpover0##c.Dxxpover0									///
		c.Dxxafdc150##c.Dxxafdc150									///
		c.Dxxgunlaw0##c.Dxxgunlaw0									///
		c.Dxxbeer0##c.Dxxbeer0										///
																	///
		(c.murd0##c.murd0)#(c.trend##c.trend)						/// now interacted with trend
		(c.Dmurd0##c.Dmurd0)#(c.trend##c.trend)						///
		(c.(D.($xx))##c.(D.($xx)))#(c.trend##c.trend)				/// note macro
		(c.(L.xxprison)##c.(L.xxprison))#(c.trend##c.trend)			///
		(c.(L.xxpolice)##c.(L.xxpolice))#(c.trend##c.trend)			///
		(c.(L.xxunemp)##c.(L.xxunemp))#(c.trend##c.trend)			///
		(c.(L.xxincome)##c.(L.xxincome))#(c.trend##c.trend)			///
		(c.(L.xxpover)##c.(L.xxpover))#(c.trend##c.trend)			///
		(c.(L.xxafdc15)##c.(L.xxafdc15))#(c.trend##c.trend)			///
		(c.(L.xxgunlaw)##c.(L.xxgunlaw))#(c.trend##c.trend)			///
		(c.(L.xxbeer)##c.(L.xxbeer))#(c.trend##c.trend)				///
		(c.Mxxprison##c.Mxxprison)#(c.trend##c.trend)				///
		(c.Mxxpolice##c.Mxxpolice)#(c.trend##c.trend)				///
		(c.Mxxunemp##c.Mxxunemp	)#(c.trend##c.trend)				///
		(c.Mxxincome##c.Mxxincome)#(c.trend##c.trend)				///
		(c.Mxxpover##c.Mxxpover	)#(c.trend##c.trend)				///
		(c.Mxxafdc15##c.Mxxafdc15)#(c.trend##c.trend)				///
		(c.Mxxgunlaw##c.Mxxgunlaw)#(c.trend##c.trend)				///
		(c.Mxxbeer##c.Mxxbeer)#(c.trend##c.trend)					///
		(c.xxprison0##c.xxprison0)#(c.trend##c.trend)				///
		(c.xxpolice0##c.xxpolice0)#(c.trend##c.trend)				///
		(c.xxunemp0##c.xxunemp0)#(c.trend##c.trend)					///
		(c.xxincome0##c.xxincome0)#(c.trend##c.trend)				///
		(c.xxpover0##c.xxpover0)#(c.trend##c.trend)					///
		(c.xxafdc150##c.xxafdc150)#(c.trend##c.trend)				///
		(c.xxgunlaw0##c.xxgunlaw0)#(c.trend##c.trend)				///
		(c.xxbeer0##c.xxbeer0)#(c.trend##c.trend)					///
		(c.Dxxprison0##c.Dxxprison0)#(c.trend##c.trend)				///
		(c.Dxxpolice0##c.Dxxpolice0)#(c.trend##c.trend)				///
		(c.Dxxunemp0##c.Dxxunemp0)#(c.trend##c.trend)				///
		(c.Dxxincome0##c.Dxxincome0)#(c.trend##c.trend)				///
		(c.Dxxpover0##c.Dxxpover0)#(c.trend##c.trend)				///
		(c.Dxxafdc150##c.Dxxafdc150)#(c.trend##c.trend)				///
		(c.Dxxgunlaw0##c.Dxxgunlaw0)#(c.trend##c.trend)				///
		(c.Dxxbeer0##c.Dxxbeer0	)#(c.trend##c.trend)				///
		i.year, partial(i.year) robust lalt corrnum(0) maxupsiter(100)
di "p=" e(p)
global ymSelrlasso `e(selected)'
di "$ymSelrlasso"
mat b_rlasso=e(b)
if `e(s)' {
	mat b_rlasso=b_rlasso[1,1..`e(s)']
}
else {
	mat b_rlasso=.
}

* Murder Outcome
/*
// no variables selected
lassoShootingCBH Dymurd $AllMurd if trend>0, controls($tdums) lasiter(100) verbose(0) fdisplay(1)
global ymSel `r(selected)'
di "`$mSel"
mat b_cbh=r(betaL)
mat b_cbh=b_cbh'
*/
global ymSel
mat b_cbh=.

// sort cols and compare
mata: st_matrix("b_rlasso",sort(st_matrix("b_rlasso")',1)')
mata: st_matrix("b_cbh",sort(st_matrix("b_cbh")',1)')
assert mreldif(b_rlasso,b_cbh)<1e-4

* Murder Abortion
rlasso D.efamurd													///
																	/// no trend
		c.murd0##c.murd0											///
		c.Dmurd0##c.Dmurd0											///
		c.(D.($xx))##c.(D.($xx))									/// note macro
		c.(L.xxprison)##c.(L.xxprison)								///
		c.(L.xxpolice)##c.(L.xxpolice)								///
		c.(L.xxunemp)##c.(L.xxunemp)								///
		c.(L.xxincome)##c.(L.xxincome)								///
		c.(L.xxpover)##c.(L.xxpover)								///
		c.(L.xxafdc15)##c.(L.xxafdc15)								///
		c.(L.xxgunlaw)##c.(L.xxgunlaw)								///
		c.(L.xxbeer)##c.(L.xxbeer)									///
		c.Mxxprison##c.Mxxprison									///
		c.Mxxpolice##c.Mxxpolice									///
		c.Mxxunemp##c.Mxxunemp										///
		c.Mxxincome##c.Mxxincome									///
		c.Mxxpover##c.Mxxpover										///
		c.Mxxafdc15##c.Mxxafdc15									///
		c.Mxxgunlaw##c.Mxxgunlaw									///
		c.Mxxbeer##c.Mxxbeer										///
		c.xxprison0##c.xxprison0									///
		c.xxpolice0##c.xxpolice0									///
		c.xxunemp0##c.xxunemp0										///
		c.xxincome0##c.xxincome0									///
		c.xxpover0##c.xxpover0										///
		c.xxafdc150##c.xxafdc150									///
		c.xxgunlaw0##c.xxgunlaw0									///
		c.xxbeer0##c.xxbeer0										///
		c.Dxxprison0##c.Dxxprison0									///
		c.Dxxpolice0##c.Dxxpolice0									///
		c.Dxxunemp0##c.Dxxunemp0									///
		c.Dxxincome0##c.Dxxincome0									///
		c.Dxxpover0##c.Dxxpover0									///
		c.Dxxafdc150##c.Dxxafdc150									///
		c.Dxxgunlaw0##c.Dxxgunlaw0									///
		c.Dxxbeer0##c.Dxxbeer0										///
																	///
		(c.murd0##c.murd0)#(c.trend##c.trend)						/// now interacted with trend
		(c.Dmurd0##c.Dmurd0)#(c.trend##c.trend)						///
		(c.(D.($xx))##c.(D.($xx)))#(c.trend##c.trend)				/// note macro
		(c.(L.xxprison)##c.(L.xxprison))#(c.trend##c.trend)			///
		(c.(L.xxpolice)##c.(L.xxpolice))#(c.trend##c.trend)			///
		(c.(L.xxunemp)##c.(L.xxunemp))#(c.trend##c.trend)			///
		(c.(L.xxincome)##c.(L.xxincome))#(c.trend##c.trend)			///
		(c.(L.xxpover)##c.(L.xxpover))#(c.trend##c.trend)			///
		(c.(L.xxafdc15)##c.(L.xxafdc15))#(c.trend##c.trend)			///
		(c.(L.xxgunlaw)##c.(L.xxgunlaw))#(c.trend##c.trend)			///
		(c.(L.xxbeer)##c.(L.xxbeer))#(c.trend##c.trend)				///
		(c.Mxxprison##c.Mxxprison)#(c.trend##c.trend)				///
		(c.Mxxpolice##c.Mxxpolice)#(c.trend##c.trend)				///
		(c.Mxxunemp##c.Mxxunemp	)#(c.trend##c.trend)				///
		(c.Mxxincome##c.Mxxincome)#(c.trend##c.trend)				///
		(c.Mxxpover##c.Mxxpover	)#(c.trend##c.trend)				///
		(c.Mxxafdc15##c.Mxxafdc15)#(c.trend##c.trend)				///
		(c.Mxxgunlaw##c.Mxxgunlaw)#(c.trend##c.trend)				///
		(c.Mxxbeer##c.Mxxbeer)#(c.trend##c.trend)					///
		(c.xxprison0##c.xxprison0)#(c.trend##c.trend)				///
		(c.xxpolice0##c.xxpolice0)#(c.trend##c.trend)				///
		(c.xxunemp0##c.xxunemp0)#(c.trend##c.trend)					///
		(c.xxincome0##c.xxincome0)#(c.trend##c.trend)				///
		(c.xxpover0##c.xxpover0)#(c.trend##c.trend)					///
		(c.xxafdc150##c.xxafdc150)#(c.trend##c.trend)				///
		(c.xxgunlaw0##c.xxgunlaw0)#(c.trend##c.trend)				///
		(c.xxbeer0##c.xxbeer0)#(c.trend##c.trend)					///
		(c.Dxxprison0##c.Dxxprison0)#(c.trend##c.trend)				///
		(c.Dxxpolice0##c.Dxxpolice0)#(c.trend##c.trend)				///
		(c.Dxxunemp0##c.Dxxunemp0)#(c.trend##c.trend)				///
		(c.Dxxincome0##c.Dxxincome0)#(c.trend##c.trend)				///
		(c.Dxxpover0##c.Dxxpover0)#(c.trend##c.trend)				///
		(c.Dxxafdc150##c.Dxxafdc150)#(c.trend##c.trend)				///
		(c.Dxxgunlaw0##c.Dxxgunlaw0)#(c.trend##c.trend)				///
		(c.Dxxbeer0##c.Dxxbeer0	)#(c.trend##c.trend)				///
		i.year, partial(i.year) robust lalt corrnum(0) maxupsiter(100)
di "p=" e(p)
global xmSelrlasso `e(selected)'
di "$xmSelrlasso"
mat b_rlasso=e(b)
if `e(s)' {
	mat b_rlasso=b_rlasso[1,1..`e(s)']
}
else {
	mat b_rlasso=.
}

* Murder Abortion
/*
lassoShootingCBH Dmurd $AllMurd if trend>0, controls($tdums) lasiter(100) verbose(0) fdisplay(1)
global xmSel `r(selected)'
di "$xmSel"
mat b_cbh=r(betaL)
mat b_cbh=b_cbh'
*/
global xmSel murd0 murd0Xt Lxxprison LxxprisonXt LxxpoliceXt MxxincomeXt Dxxincome0Xt
mat b_cbh = 2.3966347 , .55232244 , .00096424 , .0020873 , .02429935 , 28.977584 , 26.183775

// sort cols and compare
mata: st_matrix("b_rlasso",sort(st_matrix("b_rlasso")',1)')
mata: st_matrix("b_cbh",sort(st_matrix("b_cbh")',1)')
assert mreldif(b_rlasso,b_cbh)<1e-4

* Murder equation with selected controls
pdslasso D.lpc_murd D.efamurd										///
		(															/// no trend
		c.murd0##c.murd0											///
		c.Dmurd0##c.Dmurd0											///
		c.(D.($xx))##c.(D.($xx))									/// note macro
		c.(L.xxprison)##c.(L.xxprison)								///
		c.(L.xxpolice)##c.(L.xxpolice)								///
		c.(L.xxunemp)##c.(L.xxunemp)								///
		c.(L.xxincome)##c.(L.xxincome)								///
		c.(L.xxpover)##c.(L.xxpover)								///
		c.(L.xxafdc15)##c.(L.xxafdc15)								///
		c.(L.xxgunlaw)##c.(L.xxgunlaw)								///
		c.(L.xxbeer)##c.(L.xxbeer)									///
		c.Mxxprison##c.Mxxprison									///
		c.Mxxpolice##c.Mxxpolice									///
		c.Mxxunemp##c.Mxxunemp										///
		c.Mxxincome##c.Mxxincome									///
		c.Mxxpover##c.Mxxpover										///
		c.Mxxafdc15##c.Mxxafdc15									///
		c.Mxxgunlaw##c.Mxxgunlaw									///
		c.Mxxbeer##c.Mxxbeer										///
		c.xxprison0##c.xxprison0									///
		c.xxpolice0##c.xxpolice0									///
		c.xxunemp0##c.xxunemp0										///
		c.xxincome0##c.xxincome0									///
		c.xxpover0##c.xxpover0										///
		c.xxafdc150##c.xxafdc150									///
		c.xxgunlaw0##c.xxgunlaw0									///
		c.xxbeer0##c.xxbeer0										///
		c.Dxxprison0##c.Dxxprison0									///
		c.Dxxpolice0##c.Dxxpolice0									///
		c.Dxxunemp0##c.Dxxunemp0									///
		c.Dxxincome0##c.Dxxincome0									///
		c.Dxxpover0##c.Dxxpover0									///
		c.Dxxafdc150##c.Dxxafdc150									///
		c.Dxxgunlaw0##c.Dxxgunlaw0									///
		c.Dxxbeer0##c.Dxxbeer0										///
																	///
		(c.murd0##c.murd0)#(c.trend##c.trend)						/// now interacted with trend
		(c.Dmurd0##c.Dmurd0)#(c.trend##c.trend)						///
		(c.(D.($xx))##c.(D.($xx)))#(c.trend##c.trend)				/// note macro
		(c.(L.xxprison)##c.(L.xxprison))#(c.trend##c.trend)			///
		(c.(L.xxpolice)##c.(L.xxpolice))#(c.trend##c.trend)			///
		(c.(L.xxunemp)##c.(L.xxunemp))#(c.trend##c.trend)			///
		(c.(L.xxincome)##c.(L.xxincome))#(c.trend##c.trend)			///
		(c.(L.xxpover)##c.(L.xxpover))#(c.trend##c.trend)			///
		(c.(L.xxafdc15)##c.(L.xxafdc15))#(c.trend##c.trend)			///
		(c.(L.xxgunlaw)##c.(L.xxgunlaw))#(c.trend##c.trend)			///
		(c.(L.xxbeer)##c.(L.xxbeer))#(c.trend##c.trend)				///
		(c.Mxxprison##c.Mxxprison)#(c.trend##c.trend)				///
		(c.Mxxpolice##c.Mxxpolice)#(c.trend##c.trend)				///
		(c.Mxxunemp##c.Mxxunemp	)#(c.trend##c.trend)				///
		(c.Mxxincome##c.Mxxincome)#(c.trend##c.trend)				///
		(c.Mxxpover##c.Mxxpover	)#(c.trend##c.trend)				///
		(c.Mxxafdc15##c.Mxxafdc15)#(c.trend##c.trend)				///
		(c.Mxxgunlaw##c.Mxxgunlaw)#(c.trend##c.trend)				///
		(c.Mxxbeer##c.Mxxbeer)#(c.trend##c.trend)					///
		(c.xxprison0##c.xxprison0)#(c.trend##c.trend)				///
		(c.xxpolice0##c.xxpolice0)#(c.trend##c.trend)				///
		(c.xxunemp0##c.xxunemp0)#(c.trend##c.trend)					///
		(c.xxincome0##c.xxincome0)#(c.trend##c.trend)				///
		(c.xxpover0##c.xxpover0)#(c.trend##c.trend)					///
		(c.xxafdc150##c.xxafdc150)#(c.trend##c.trend)				///
		(c.xxgunlaw0##c.xxgunlaw0)#(c.trend##c.trend)				///
		(c.xxbeer0##c.xxbeer0)#(c.trend##c.trend)					///
		(c.Dxxprison0##c.Dxxprison0)#(c.trend##c.trend)				///
		(c.Dxxpolice0##c.Dxxpolice0)#(c.trend##c.trend)				///
		(c.Dxxunemp0##c.Dxxunemp0)#(c.trend##c.trend)				///
		(c.Dxxincome0##c.Dxxincome0)#(c.trend##c.trend)				///
		(c.Dxxpover0##c.Dxxpover0)#(c.trend##c.trend)				///
		(c.Dxxafdc150##c.Dxxafdc150)#(c.trend##c.trend)				///
		(c.Dxxgunlaw0##c.Dxxgunlaw0)#(c.trend##c.trend)				///
		(c.Dxxbeer0##c.Dxxbeer0	)#(c.trend##c.trend)				///
		i.year														///
		)															///
		, partial(i.year) robust lopt(lalt corrnum(0) maxupsiter(100))
mat b_pdslasso=e(b)

* Get union of selected instruments
global mDS : list global(ymSel) | global(xmSel)

* Murder equation with selected controls
// Original - clustered SEs (but not cluster lasso)
// reg Dymurd Dmurd $mDS $tdums if trend>0, cluster(statenum)
// robust, no cluster
reg Dymurd Dmurd $mDS $tdums if trend>0, robust
mat b_regress=e(b)

// sort cols and compare
// may be diff lengths and include 0s (for dropped base vars)
// so compare abs values > 0
mata: st_matrix("b_pdslasso",abs(sort(-abs(st_matrix("b_pdslasso"))',1))')
mata: st_matrix("b_regress",abs(sort(-abs(st_matrix("b_regress"))',1))')
local k1 = colsof(b_pdslasso)
local k2 = colsof(b_regress)
mat b_pdslasso = b_pdslasso[1,1..min(`k1',`k2')]
mat b_regress = b_regress[1,1..min(`k1',`k2')]
assert mreldif(b_pdslasso,b_regress)<1e-6

// End replication.

// Run misc options and combinations.
foreach option1 in " " partial(i.year) pnotpen(i.year) aset(i.year)		{
foreach option2 in " " robust cluster(statenum)							{
foreach option3 in " " fe nocons										{
	di "Options: `option1' `option2' `option3'"
	pdslasso D.lpc_prop D.efaprop										///
			(															/// no trend
			c.prop0##c.prop0											///
			c.Dprop0##c.Dprop0											///
			c.(D.($xx))##c.(D.($xx))									/// note macro
			c.(L.xxprison)##c.(L.xxprison)								///
			c.(L.xxpolice)##c.(L.xxpolice)								///
			c.(L.xxunemp)##c.(L.xxunemp)								///
			c.(L.xxincome)##c.(L.xxincome)								///
			c.(L.xxpover)##c.(L.xxpover)								///
			c.(L.xxafdc15)##c.(L.xxafdc15)								///
			c.(L.xxgunlaw)##c.(L.xxgunlaw)								///
			c.(L.xxbeer)##c.(L.xxbeer)									///
			c.Mxxprison##c.Mxxprison									///
			c.Mxxpolice##c.Mxxpolice									///
			c.Mxxunemp##c.Mxxunemp										///
			c.Mxxincome##c.Mxxincome									///
			c.Mxxpover##c.Mxxpover										///
			c.Mxxafdc15##c.Mxxafdc15									///
			c.Mxxgunlaw##c.Mxxgunlaw									///
			c.Mxxbeer##c.Mxxbeer										///
			c.xxprison0##c.xxprison0									///
			c.xxpolice0##c.xxpolice0									///
			c.xxunemp0##c.xxunemp0										///
			c.xxincome0##c.xxincome0									///
			c.xxpover0##c.xxpover0										///
			c.xxafdc150##c.xxafdc150									///
			c.xxgunlaw0##c.xxgunlaw0									///
			c.xxbeer0##c.xxbeer0										///
			c.Dxxprison0##c.Dxxprison0									///
			c.Dxxpolice0##c.Dxxpolice0									///
			c.Dxxunemp0##c.Dxxunemp0									///
			c.Dxxincome0##c.Dxxincome0									///
			c.Dxxpover0##c.Dxxpover0									///
			c.Dxxafdc150##c.Dxxafdc150									///
			c.Dxxgunlaw0##c.Dxxgunlaw0									///
			c.Dxxbeer0##c.Dxxbeer0										///
																		///
			(c.prop0##c.prop0)#(c.trend##c.trend)						/// now interacted with trend
			(c.Dprop0##c.Dprop0)#(c.trend##c.trend)						///
			(c.(D.($xx))##c.(D.($xx)))#(c.trend##c.trend)				/// note macro
			(c.(L.xxprison)##c.(L.xxprison))#(c.trend##c.trend)			///
			(c.(L.xxpolice)##c.(L.xxpolice))#(c.trend##c.trend)			///
			(c.(L.xxunemp)##c.(L.xxunemp))#(c.trend##c.trend)			///
			(c.(L.xxincome)##c.(L.xxincome))#(c.trend##c.trend)			///
			(c.(L.xxpover)##c.(L.xxpover))#(c.trend##c.trend)			///
			(c.(L.xxafdc15)##c.(L.xxafdc15))#(c.trend##c.trend)			///
			(c.(L.xxgunlaw)##c.(L.xxgunlaw))#(c.trend##c.trend)			///
			(c.(L.xxbeer)##c.(L.xxbeer))#(c.trend##c.trend)				///
			(c.Mxxprison##c.Mxxprison)#(c.trend##c.trend)				///
			(c.Mxxpolice##c.Mxxpolice)#(c.trend##c.trend)				///
			(c.Mxxunemp##c.Mxxunemp	)#(c.trend##c.trend)				///
			(c.Mxxincome##c.Mxxincome)#(c.trend##c.trend)				///
			(c.Mxxpover##c.Mxxpover	)#(c.trend##c.trend)				///
			(c.Mxxafdc15##c.Mxxafdc15)#(c.trend##c.trend)				///
			(c.Mxxgunlaw##c.Mxxgunlaw)#(c.trend##c.trend)				///
			(c.Mxxbeer##c.Mxxbeer)#(c.trend##c.trend)					///
			(c.xxprison0##c.xxprison0)#(c.trend##c.trend)				///
			(c.xxpolice0##c.xxpolice0)#(c.trend##c.trend)				///
			(c.xxunemp0##c.xxunemp0)#(c.trend##c.trend)					///
			(c.xxincome0##c.xxincome0)#(c.trend##c.trend)				///
			(c.xxpover0##c.xxpover0)#(c.trend##c.trend)					///
			(c.xxafdc150##c.xxafdc150)#(c.trend##c.trend)				///
			(c.xxgunlaw0##c.xxgunlaw0)#(c.trend##c.trend)				///
			(c.xxbeer0##c.xxbeer0)#(c.trend##c.trend)					///
			(c.Dxxprison0##c.Dxxprison0)#(c.trend##c.trend)				///
			(c.Dxxpolice0##c.Dxxpolice0)#(c.trend##c.trend)				///
			(c.Dxxunemp0##c.Dxxunemp0)#(c.trend##c.trend)				///
			(c.Dxxincome0##c.Dxxincome0)#(c.trend##c.trend)				///
			(c.Dxxpover0##c.Dxxpover0)#(c.trend##c.trend)				///
			(c.Dxxafdc150##c.Dxxafdc150)#(c.trend##c.trend)				///
			(c.Dxxgunlaw0##c.Dxxgunlaw0)#(c.trend##c.trend)				///
			(c.Dxxbeer0##c.Dxxbeer0	)#(c.trend##c.trend)				///
			i.year														///
			)															///
			, `option1' `option2' `option3'
	}
	}
	}


// FE and no ftools
pdslasso D.lpc_prop D.efaprop										///
		(															/// no trend
		c.prop0##c.prop0											///
		c.Dprop0##c.Dprop0											///
		c.(D.($xx))##c.(D.($xx))									/// note macro
		c.(L.xxprison)##c.(L.xxprison)								///
		c.(L.xxpolice)##c.(L.xxpolice)								///
		c.(L.xxunemp)##c.(L.xxunemp)								///
		c.(L.xxincome)##c.(L.xxincome)								///
		c.(L.xxpover)##c.(L.xxpover)								///
		c.(L.xxafdc15)##c.(L.xxafdc15)								///
		c.(L.xxgunlaw)##c.(L.xxgunlaw)								///
		c.(L.xxbeer)##c.(L.xxbeer)									///
		c.Mxxprison##c.Mxxprison									///
		c.Mxxpolice##c.Mxxpolice									///
		c.Mxxunemp##c.Mxxunemp										///
		c.Mxxincome##c.Mxxincome									///
		c.Mxxpover##c.Mxxpover										///
		c.Mxxafdc15##c.Mxxafdc15									///
		c.Mxxgunlaw##c.Mxxgunlaw									///
		c.Mxxbeer##c.Mxxbeer										///
		c.xxprison0##c.xxprison0									///
		c.xxpolice0##c.xxpolice0									///
		c.xxunemp0##c.xxunemp0										///
		c.xxincome0##c.xxincome0									///
		c.xxpover0##c.xxpover0										///
		c.xxafdc150##c.xxafdc150									///
		c.xxgunlaw0##c.xxgunlaw0									///
		c.xxbeer0##c.xxbeer0										///
		c.Dxxprison0##c.Dxxprison0									///
		c.Dxxpolice0##c.Dxxpolice0									///
		c.Dxxunemp0##c.Dxxunemp0									///
		c.Dxxincome0##c.Dxxincome0									///
		c.Dxxpover0##c.Dxxpover0									///
		c.Dxxafdc150##c.Dxxafdc150									///
		c.Dxxgunlaw0##c.Dxxgunlaw0									///
		c.Dxxbeer0##c.Dxxbeer0										///
																	///
		(c.prop0##c.prop0)#(c.trend##c.trend)						/// now interacted with trend
		(c.Dprop0##c.Dprop0)#(c.trend##c.trend)						///
		(c.(D.($xx))##c.(D.($xx)))#(c.trend##c.trend)				/// note macro
		(c.(L.xxprison)##c.(L.xxprison))#(c.trend##c.trend)			///
		(c.(L.xxpolice)##c.(L.xxpolice))#(c.trend##c.trend)			///
		(c.(L.xxunemp)##c.(L.xxunemp))#(c.trend##c.trend)			///
		(c.(L.xxincome)##c.(L.xxincome))#(c.trend##c.trend)			///
		(c.(L.xxpover)##c.(L.xxpover))#(c.trend##c.trend)			///
		(c.(L.xxafdc15)##c.(L.xxafdc15))#(c.trend##c.trend)			///
		(c.(L.xxgunlaw)##c.(L.xxgunlaw))#(c.trend##c.trend)			///
		(c.(L.xxbeer)##c.(L.xxbeer))#(c.trend##c.trend)				///
		(c.Mxxprison##c.Mxxprison)#(c.trend##c.trend)				///
		(c.Mxxpolice##c.Mxxpolice)#(c.trend##c.trend)				///
		(c.Mxxunemp##c.Mxxunemp	)#(c.trend##c.trend)				///
		(c.Mxxincome##c.Mxxincome)#(c.trend##c.trend)				///
		(c.Mxxpover##c.Mxxpover	)#(c.trend##c.trend)				///
		(c.Mxxafdc15##c.Mxxafdc15)#(c.trend##c.trend)				///
		(c.Mxxgunlaw##c.Mxxgunlaw)#(c.trend##c.trend)				///
		(c.Mxxbeer##c.Mxxbeer)#(c.trend##c.trend)					///
		(c.xxprison0##c.xxprison0)#(c.trend##c.trend)				///
		(c.xxpolice0##c.xxpolice0)#(c.trend##c.trend)				///
		(c.xxunemp0##c.xxunemp0)#(c.trend##c.trend)					///
		(c.xxincome0##c.xxincome0)#(c.trend##c.trend)				///
		(c.xxpover0##c.xxpover0)#(c.trend##c.trend)					///
		(c.xxafdc150##c.xxafdc150)#(c.trend##c.trend)				///
		(c.xxgunlaw0##c.xxgunlaw0)#(c.trend##c.trend)				///
		(c.xxbeer0##c.xxbeer0)#(c.trend##c.trend)					///
		(c.Dxxprison0##c.Dxxprison0)#(c.trend##c.trend)				///
		(c.Dxxpolice0##c.Dxxpolice0)#(c.trend##c.trend)				///
		(c.Dxxunemp0##c.Dxxunemp0)#(c.trend##c.trend)				///
		(c.Dxxincome0##c.Dxxincome0)#(c.trend##c.trend)				///
		(c.Dxxpover0##c.Dxxpover0)#(c.trend##c.trend)				///
		(c.Dxxafdc150##c.Dxxafdc150)#(c.trend##c.trend)				///
		(c.Dxxgunlaw0##c.Dxxgunlaw0)#(c.trend##c.trend)				///
		(c.Dxxbeer0##c.Dxxbeer0	)#(c.trend##c.trend)				///
		i.year														///
		)															///
		, fe
savedresults save ftools e()
cap noi assert "`e(noftools)'"==""  // will be error if ftools not installed
pdslasso D.lpc_prop D.efaprop										///
		(															/// no trend
		c.prop0##c.prop0											///
		c.Dprop0##c.Dprop0											///
		c.(D.($xx))##c.(D.($xx))									/// note macro
		c.(L.xxprison)##c.(L.xxprison)								///
		c.(L.xxpolice)##c.(L.xxpolice)								///
		c.(L.xxunemp)##c.(L.xxunemp)								///
		c.(L.xxincome)##c.(L.xxincome)								///
		c.(L.xxpover)##c.(L.xxpover)								///
		c.(L.xxafdc15)##c.(L.xxafdc15)								///
		c.(L.xxgunlaw)##c.(L.xxgunlaw)								///
		c.(L.xxbeer)##c.(L.xxbeer)									///
		c.Mxxprison##c.Mxxprison									///
		c.Mxxpolice##c.Mxxpolice									///
		c.Mxxunemp##c.Mxxunemp										///
		c.Mxxincome##c.Mxxincome									///
		c.Mxxpover##c.Mxxpover										///
		c.Mxxafdc15##c.Mxxafdc15									///
		c.Mxxgunlaw##c.Mxxgunlaw									///
		c.Mxxbeer##c.Mxxbeer										///
		c.xxprison0##c.xxprison0									///
		c.xxpolice0##c.xxpolice0									///
		c.xxunemp0##c.xxunemp0										///
		c.xxincome0##c.xxincome0									///
		c.xxpover0##c.xxpover0										///
		c.xxafdc150##c.xxafdc150									///
		c.xxgunlaw0##c.xxgunlaw0									///
		c.xxbeer0##c.xxbeer0										///
		c.Dxxprison0##c.Dxxprison0									///
		c.Dxxpolice0##c.Dxxpolice0									///
		c.Dxxunemp0##c.Dxxunemp0									///
		c.Dxxincome0##c.Dxxincome0									///
		c.Dxxpover0##c.Dxxpover0									///
		c.Dxxafdc150##c.Dxxafdc150									///
		c.Dxxgunlaw0##c.Dxxgunlaw0									///
		c.Dxxbeer0##c.Dxxbeer0										///
																	///
		(c.prop0##c.prop0)#(c.trend##c.trend)						/// now interacted with trend
		(c.Dprop0##c.Dprop0)#(c.trend##c.trend)						///
		(c.(D.($xx))##c.(D.($xx)))#(c.trend##c.trend)				/// note macro
		(c.(L.xxprison)##c.(L.xxprison))#(c.trend##c.trend)			///
		(c.(L.xxpolice)##c.(L.xxpolice))#(c.trend##c.trend)			///
		(c.(L.xxunemp)##c.(L.xxunemp))#(c.trend##c.trend)			///
		(c.(L.xxincome)##c.(L.xxincome))#(c.trend##c.trend)			///
		(c.(L.xxpover)##c.(L.xxpover))#(c.trend##c.trend)			///
		(c.(L.xxafdc15)##c.(L.xxafdc15))#(c.trend##c.trend)			///
		(c.(L.xxgunlaw)##c.(L.xxgunlaw))#(c.trend##c.trend)			///
		(c.(L.xxbeer)##c.(L.xxbeer))#(c.trend##c.trend)				///
		(c.Mxxprison##c.Mxxprison)#(c.trend##c.trend)				///
		(c.Mxxpolice##c.Mxxpolice)#(c.trend##c.trend)				///
		(c.Mxxunemp##c.Mxxunemp	)#(c.trend##c.trend)				///
		(c.Mxxincome##c.Mxxincome)#(c.trend##c.trend)				///
		(c.Mxxpover##c.Mxxpover	)#(c.trend##c.trend)				///
		(c.Mxxafdc15##c.Mxxafdc15)#(c.trend##c.trend)				///
		(c.Mxxgunlaw##c.Mxxgunlaw)#(c.trend##c.trend)				///
		(c.Mxxbeer##c.Mxxbeer)#(c.trend##c.trend)					///
		(c.xxprison0##c.xxprison0)#(c.trend##c.trend)				///
		(c.xxpolice0##c.xxpolice0)#(c.trend##c.trend)				///
		(c.xxunemp0##c.xxunemp0)#(c.trend##c.trend)					///
		(c.xxincome0##c.xxincome0)#(c.trend##c.trend)				///
		(c.xxpover0##c.xxpover0)#(c.trend##c.trend)					///
		(c.xxafdc150##c.xxafdc150)#(c.trend##c.trend)				///
		(c.xxgunlaw0##c.xxgunlaw0)#(c.trend##c.trend)				///
		(c.xxbeer0##c.xxbeer0)#(c.trend##c.trend)					///
		(c.Dxxprison0##c.Dxxprison0)#(c.trend##c.trend)				///
		(c.Dxxpolice0##c.Dxxpolice0)#(c.trend##c.trend)				///
		(c.Dxxunemp0##c.Dxxunemp0)#(c.trend##c.trend)				///
		(c.Dxxincome0##c.Dxxincome0)#(c.trend##c.trend)				///
		(c.Dxxpover0##c.Dxxpover0)#(c.trend##c.trend)				///
		(c.Dxxafdc150##c.Dxxafdc150)#(c.trend##c.trend)				///
		(c.Dxxgunlaw0##c.Dxxgunlaw0)#(c.trend##c.trend)				///
		(c.Dxxbeer0##c.Dxxbeer0	)#(c.trend##c.trend)				///
		i.year														///
		)															///
		, fe noftools
assert "`e(noftools)'"=="noftools"
savedresults comp ftools e()

// ***************** END LEVITT DATASET ************************* //


// ***************** BLP DATASET ************************* //

* follows/replicates Matlab code/results from Belloni et. al 2012.

// dataset separately constructed from Matalab data and code
use BLP, clear

gen double y = ln(share) - ln(outshr)

// Matlab feasiblePostLasso code description:
// default lambda is 2.2*sqrt(n)*norminv(1-(1/log(n))/(2*p))
// i.e. "gamma" is 1 not 0.1 as in rlasso.
// default is het-rob loadings
// default max ups iter = 15
// default ups tol = 1e-6
// default zero tol = 1e-4 (not settable in feasiblePostLasso)
// default opt tol = 1e-5 (not settable in feasilblePostLasso)
// 6 results replicated, 3 with tol 1e-3, 3 with tol 1e-5
// Would possibly replicate more closely if Matlab tolerances were tighter.

******************************
******** Small set ***********
******************************

// lasso, price (line 72)
// Matlab output:
// bDzx =
//  -0.027187307080839
//                   0
//   0.140573386182651
//                   0
//  -0.040297610145709
//   0.043091980749192
//                   0
//                   0
//                   0
//                   0
//  22.361611212285307
//   8.864499676282328
//  -2.336920687857595
//   1.571888537853250
mat b_matlab = 				///
	(						///
  -0.027187307080839,		///
                   0,		///
   0.140573386182651,		///
                   0,		///
  -0.040297610145709,		///
   0.043091980749192,		///
                   0,		///
                   0,		///
                   0,		///
                   0,		///
  22.361611212285307,		///
   8.864499676282328,		///
  -2.336920687857595,		///
   1.571888537853250		///
   )

rlasso price																///
		sum_othbig_1 sum_othbig_2 sum_othbig_3 sum_othbig_4 sum_othbig_5	///
		sum_rivbig_1 sum_rivbig_2 sum_rivbig_3 sum_rivbig_4 sum_rivbig_5	///
		hpwt air mpd space													///
		, displayall gamma(1) rob maxupsiter(15)							///
		tolups(1e-6) tolzero(1e-4) tolopt(1e-5)

mat b_rlasso = e(betaAll)
mat b_rlasso = b_rlasso[1,1..colsof(b_rlasso)-1]
// note looser tolerance
assert mreldif(b_rlasso,b_matlab)<1e-3


// lasso, dep var (line 83)
// Matlab output:
// bYx =
//  -2.226431503473688
//  -0.894181896404704
//   0.234706642184791
//   1.782524104280113
mat b_matlab = 				///
	(						///
  -2.226431503473688,		///
  -0.894181896404704,		///
   0.234706642184791,		///
   1.782524104280113		///
   )

rlasso y																	///
		hpwt air mpd space 													///
		, displayall gamma(1) rob maxupsiter(15)							///
		tolups(1e-6) tolzero(1e-4) tolopt(1e-5)

mat b_rlasso = e(betaAll)
mat b_rlasso = b_rlasso[1,1..colsof(b_rlasso)-1]
// note tougher tolerance
assert mreldif(b_rlasso,b_matlab)<1e-5


// post-regularization IV (line 94)
// post-lasso-orthogonalized replicates
// Matlab output:
// aL =
//   -0.184934034947562
ivlasso y (price=															///
		sum_othbig_1 sum_othbig_2 sum_othbig_3 sum_othbig_4 sum_othbig_5	///
		sum_rivbig_1 sum_rivbig_2 sum_rivbig_3 sum_rivbig_4 sum_rivbig_5	///
		)																	///
		(hpwt air mpd space)												///
		, rob lopt(gamma(1) maxupsiter(15) tolups(1e-6))
// note tougher tolerance
assert reldif(el(e(beta_plasso),1,1), -0.184934034947562)<1e-5

******************************
******** Big set *************
******************************

// lasso, price (line 135)
// Matlab output:
/*
bDzx =

  -0.026309868626536
                   0
   0.064576639402394
                   0
                   0
                   0
                   0
                   0
                   0
                   0
  -0.061816175757360
                   0
                   0
                   0
                   0
                   0
                   0
                   0
                   0
                   0
                   0
                   0
                   0
                   0
                   0
                   0
                   0
                   0
   0.057797411950973
                   0
                   0
                   0
                   0
                   0
                   0
                   0
                   0
                   0
                   0
                   0
                   0
                   0
                   0
                   0
                   0
                   0
                   0
                   0
                   0
                   0
 -14.890463278505399
                   0
                   0
                   0
  20.239078392116543
                   0
                   0
                   0
                   0
                   0
                   0
  15.468957465755846
                   0
   3.499130491002932
                   0
                   0
   6.172115587542651
                   0
                   0
                   0
   1.525359161441577
 */
mat b_matlab = 				///
	(						///
  -0.026309868626536,		///
                   0,		///
   0.064576639402394,		///
                   0,		///
                   0,		///
                   0,		///
                   0,		///
                   0,		///
                   0,		///
                   0,		///
  -0.061816175757360,		///
                   0,		///
                   0,		///
                   0,		///
                   0,		///
                   0,		///
                   0,		///
                   0,		///
                   0,		///
                   0,		///
                   0,		///
                   0,		///
                   0,		///
                   0,		///
                   0,		///
                   0,		///
                   0,		///
                   0,		///
   0.057797411950973,		///
                   0,		///
                   0,		///
                   0,		///
                   0,		///
                   0,		///
                   0,		///
                   0,		///
                   0,		///
                   0,		///
                   0,		///
                   0,		///
                   0,		///
                   0,		///
                   0,		///
                   0,		///
                   0,		///
                   0,		///
                   0,		///
                   0,		///
                   0,		///
                   0,		///
 -14.890463278505399,		///
                   0,		///
                   0,		///
                   0,		///
  20.239078392116543,		///
                   0,		///
                   0,		///
                   0,		///
                   0,		///
                   0,		///
                   0,		///
  15.468957465755846,		///
                   0,		///
   3.499130491002932,		///
                   0,		///
                   0,		///
   6.172115587542651,		///
                   0,		///
                   0,		///
                   0,		///
   1.525359161441577		///
   )

rlasso price																///
		Z*																	///
		hpwt air mpdu spaceu tu												///
		hpwt_sq-spaceu_tu													///
		, displayall gamma(1) rob maxupsiter(15)							///
		tolups(1e-6) tolzero(1e-4) tolopt(1e-5)

mat b_rlasso = e(betaAll)
mat b_rlasso = b_rlasso[1,1..colsof(b_rlasso)-1]
// note looser tolerance
assert mreldif(b_rlasso,b_matlab)<1e-3

// lasso, dep var (line 146)
// Matlab output:
/*
bYx =
                   0
  -0.448213818098585
                   0
                   0
                   0
                   0
  -2.798818364136941
                   0
                   0
   0.849364554746214
   1.079486139873257
                   0
                   0
  -0.748743293530742
  -0.289563827442536
                   0
                   0
                   0
                   0
                   0
   3.512233383389987
                   0
                   0
*/

mat b_matlab = 				///
	(						///
                   0,		///
  -0.448213818098585,		///
                   0,		///
                   0,		///
                   0,		///
                   0,		///
  -2.798818364136941,		///
                   0,		///
                   0,		///
   0.849364554746214,		///
   1.079486139873257,		///
                   0,		///
                   0,		///
  -0.748743293530742,		///
  -0.289563827442536,		///
                   0,		///
                   0,		///
                   0,		///
                   0,		///
                   0,		///
   3.512233383389987,		///
                   0,		///
                   0		///
	)

rlasso y																	///
		hpwt air mpdu spaceu tu												///
		hpwt_sq-spaceu_tu													///
		, displayall gamma(1) rob maxupsiter(15)							///
		tolups(1e-6) tolzero(1e-4) tolopt(1e-5)

mat b_rlasso = e(betaAll)
mat b_rlasso = b_rlasso[1,1..colsof(b_rlasso)-1]
// note looser tolerance
assert mreldif(b_rlasso,b_matlab)<1e-3

// post-regularization IV (line 157)
// post-lasso-orthogonalized replicates
// Matlab output:
// aB =
//  -0.221208122513718
ivlasso y (price=															///
		Z*																	///
		)																	///
		(hpwt air mpdu spaceu tu											///
		hpwt_sq-spaceu_tu)													///
		, rob lopt(gamma(1) maxupsiter(15) tolups(1e-6))
// note tougher tolerance
assert reldif(el(e(beta_plasso),1,1), -0.221208122513718)<1e-5


// now results from Chernoznukov et al. AER 2015.
// 6 results replicated, 3 with tol 1e-3, 3 with tol 1e-5
// Would possibly replicate more closely if Matlab tolerances were tighter.

******************************
******** Small set ***********
******************************

// lasso, price (line 72)
// Matlab output:
// bDzx =
//  -0.027187307080839
//                   0
//   0.140573386182651
//                   0
//  -0.040297610145709
//   0.043091980749192
//                   0
//                   0
//                   0
//                   0
//  22.361611212285307
//   8.864499676282328
//  -2.336920687857595
//   1.571888537853250
mat b_matlab = 				///
	(						///
  -0.027187307080839,		///
                   0,		///
   0.140573386182651,		///
                   0,		///
  -0.040297610145709,		///
   0.043091980749192,		///
                   0,		///
                   0,		///
                   0,		///
                   0,		///
  22.361611212285307,		///
   8.864499676282328,		///
  -2.336920687857595,		///
   1.571888537853250		///
   )

rlasso price																///
		sum_othbig_1 sum_othbig_2 sum_othbig_3 sum_othbig_4 sum_othbig_5	///
		sum_rivbig_1 sum_rivbig_2 sum_rivbig_3 sum_rivbig_4 sum_rivbig_5	///
		hpwt air mpd space													///
		, displayall gamma(1) rob maxupsiter(15)							///
		tolups(1e-6) tolzero(1e-4) tolopt(1e-5)

mat b_rlasso = e(betaAll)
mat b_rlasso = b_rlasso[1,1..colsof(b_rlasso)-1]
// note looser tolerance
assert mreldif(b_rlasso,b_matlab)<1e-3


// lasso, dep var (line 83)
// Matlab output:
// bYx =
//  -2.226431503473688
//  -0.894181896404704
//   0.234706642184791
//   1.782524104280113
mat b_matlab = 				///
	(						///
  -2.226431503473688,		///
  -0.894181896404704,		///
   0.234706642184791,		///
   1.782524104280113		///
   )

rlasso y																	///
		hpwt air mpd space 													///
		, displayall gamma(1) rob maxupsiter(15)							///
		tolups(1e-6) tolzero(1e-4) tolopt(1e-5)

mat b_rlasso = e(betaAll)
mat b_rlasso = b_rlasso[1,1..colsof(b_rlasso)-1]
// note tougher tolerance
assert mreldif(b_rlasso,b_matlab)<1e-5


// post-regularization IV (line 94)
// post-lasso-orthogonalized replicates
// Matlab output:
// aL =
//   -0.184934034947562
ivlasso y (price=															///
		sum_othbig_1 sum_othbig_2 sum_othbig_3 sum_othbig_4 sum_othbig_5	///
		sum_rivbig_1 sum_rivbig_2 sum_rivbig_3 sum_rivbig_4 sum_rivbig_5	///
		)																	///
		(hpwt air mpd space)												///
		, rob lopt(gamma(1) maxupsiter(15) tolups(1e-6))
// note tougher tolerance
assert reldif(el(e(beta_plasso),1,1), -0.184934034947562)<1e-5

******************************
******** Big set *************
******************************

// lasso, price (line 135)
// Matlab output:
/*
bDzx =

  -0.026309868626536
                   0
   0.064576639402394
                   0
                   0
                   0
                   0
                   0
                   0
                   0
  -0.061816175757360
                   0
                   0
                   0
                   0
                   0
                   0
                   0
                   0
                   0
                   0
                   0
                   0
                   0
                   0
                   0
                   0
                   0
   0.057797411950973
                   0
                   0
                   0
                   0
                   0
                   0
                   0
                   0
                   0
                   0
                   0
                   0
                   0
                   0
                   0
                   0
                   0
                   0
                   0
                   0
                   0
 -14.890463278505399
                   0
                   0
                   0
  20.239078392116543
                   0
                   0
                   0
                   0
                   0
                   0
  15.468957465755846
                   0
   3.499130491002932
                   0
                   0
   6.172115587542651
                   0
                   0
                   0
   1.525359161441577
 */
mat b_matlab = 				///
	(						///
  -0.026309868626536,		///
                   0,		///
   0.064576639402394,		///
                   0,		///
                   0,		///
                   0,		///
                   0,		///
                   0,		///
                   0,		///
                   0,		///
  -0.061816175757360,		///
                   0,		///
                   0,		///
                   0,		///
                   0,		///
                   0,		///
                   0,		///
                   0,		///
                   0,		///
                   0,		///
                   0,		///
                   0,		///
                   0,		///
                   0,		///
                   0,		///
                   0,		///
                   0,		///
                   0,		///
   0.057797411950973,		///
                   0,		///
                   0,		///
                   0,		///
                   0,		///
                   0,		///
                   0,		///
                   0,		///
                   0,		///
                   0,		///
                   0,		///
                   0,		///
                   0,		///
                   0,		///
                   0,		///
                   0,		///
                   0,		///
                   0,		///
                   0,		///
                   0,		///
                   0,		///
                   0,		///
 -14.890463278505399,		///
                   0,		///
                   0,		///
                   0,		///
  20.239078392116543,		///
                   0,		///
                   0,		///
                   0,		///
                   0,		///
                   0,		///
                   0,		///
  15.468957465755846,		///
                   0,		///
   3.499130491002932,		///
                   0,		///
                   0,		///
   6.172115587542651,		///
                   0,		///
                   0,		///
                   0,		///
   1.525359161441577		///
   )

rlasso price																///
		Z*																	///
		hpwt air mpdu spaceu tu												///
		hpwt_sq-spaceu_tu													///
		, displayall gamma(1) rob maxupsiter(15)							///
		tolups(1e-6) tolzero(1e-4) tolopt(1e-5)

mat b_rlasso = e(betaAll)
mat b_rlasso = b_rlasso[1,1..colsof(b_rlasso)-1]
// note looser tolerance
assert mreldif(b_rlasso,b_matlab)<1e-3

// lasso, dep var (line 146)
// Matlab output:
/*
bYx =
                   0
  -0.448213818098585
                   0
                   0
                   0
                   0
  -2.798818364136941
                   0
                   0
   0.849364554746214
   1.079486139873257
                   0
                   0
  -0.748743293530742
  -0.289563827442536
                   0
                   0
                   0
                   0
                   0
   3.512233383389987
                   0
                   0
*/

mat b_matlab = 				///
	(						///
                   0,		///
  -0.448213818098585,		///
                   0,		///
                   0,		///
                   0,		///
                   0,		///
  -2.798818364136941,		///
                   0,		///
                   0,		///
   0.849364554746214,		///
   1.079486139873257,		///
                   0,		///
                   0,		///
  -0.748743293530742,		///
  -0.289563827442536,		///
                   0,		///
                   0,		///
                   0,		///
                   0,		///
                   0,		///
   3.512233383389987,		///
                   0,		///
                   0		///
	)

rlasso y																	///
		hpwt air mpdu spaceu tu												///
		hpwt_sq-spaceu_tu													///
		, displayall gamma(1) rob maxupsiter(15)							///
		tolups(1e-6) tolzero(1e-4) tolopt(1e-5)

mat b_rlasso = e(betaAll)
mat b_rlasso = b_rlasso[1,1..colsof(b_rlasso)-1]
// note looser tolerance
assert mreldif(b_rlasso,b_matlab)<1e-3

// post-regularization IV (line 157)
// post-lasso-orthogonalized replicates
// Matlab output:
// aB =
//  -0.221208122513718
ivlasso y (price=															///
		Z*																	///
		)																	///
		(hpwt air mpdu spaceu tu											///
		hpwt_sq-spaceu_tu)													///
		, rob lopt(gamma(1) maxupsiter(15) tolups(1e-6))
// note tougher tolerance
assert reldif(el(e(beta_plasso),1,1), -0.221208122513718)<1e-5

// ***************** END BLP DATASET ************************* //

// ***************** AJR DATASET ************************* //

// used to check options etc.; no replication.
// datasets separately downloaded from economics.mit.edu; see help ivlasso.
clear
use maketable6
merge 1:1 shortnam using maketable8
keep if baseco==1
order shortnam logpgp95 avexpr lat_abst logem4 edes1975 avelf, first
order indtime euro1900 democ1 cons1 democ00a cons00a, last

// examples based on those in ivlasso help file.

//Examples with exogenous regressors:

// Basic usage: select from high-dim controls.
pdslasso logpgp95 avexpr										///
	(lat_abst edes1975 avelf temp* humid* steplow-oilres)

// As above, hetoroskedastic-robust.
pdslasso logpgp95 avexpr										///
	(lat_abst edes1975 avelf temp* humid* steplow-oilres)		///
	, rob

// Specify that latitude is an unpenalized control to be partialled out.
pdslasso logpgp95 avexpr										///
	(lat_abst edes1975 avelf temp* humid* steplow-oilres)	///
	, partial(lat_abst)

// Specify that latitude is an unpenalized control using the notpen option (equivalent).
pdslasso logpgp95 avexpr										///
	(lat_abst edes1975 avelf temp* humid* steplow-oilres)	///
	, pnotpen(lat_abst)

// Specify that latitude is in the amelioration set.
pdslasso logpgp95 avexpr										///
	(lat_abst edes1975 avelf temp* humid* steplow-oilres)	///
	, aset(lat_abst)

// Example with endogenous regressor, high-dimensional controls
// and low-dimensional instrument:

// Select controls; specify that logem4 is an unpenalized instrument
// to be partialled out.
ivlasso logpgp95 (avexpr=logem4)							///
	(lat_abst edes1975 avelf temp* humid* steplow-oilres)	///
	, partial(logem4)

// Example with endogenous regressor and high-dimensional instruments and controls:

// Select controls and instruments; specify that logem4 is an unpenalized
// instrument and lat_abst is an unpenalized control; request weak
// identification stats and first-stage results.
ivlasso logpgp95											///
	(lat_abst edes1975 avelf temp* humid* steplow-oilres)	///
	(avexpr=logem4 euro1900-cons00a)						///
	, partial(logem4 lat_abst) idstats first

// Select controls and instruments; specify that lat_abst
// is an unpenalized control; request weak identification
// stats and sup-score confidence sets.
ivlasso logpgp95											///
	(lat_abst edes1975 avelf temp* humid* steplow-oilres)	///
	(avexpr=logem4 euro1900-cons00a)						///
	,partial(lat_abst) idstats sscset

// As above but heteroskedastic-robust and use grid options
// to control grid search and test level; also set seed in rlasso
// options to make multiplier-bootstrap p-values replicable.
ivlasso logpgp95											///
	(lat_abst edes1975 avelf temp* humid* steplow-oilres)	///
	(avexpr=logem4 euro1900-cons00a)						///
	,partial(lat_abst) rob idstats							///
	sscset ssgridmin(0) ssgridmax(2) ssgamma(0.1)			///
	lopt(seed(1))

// ***************** END AJR DATASET ************************* //

// ***************** ABDATA DATASET ************************** //

webuse abdata, clear

// FE and noftools options
pdslasso ys L.k (L(1/3).w yr*), fe
savedresults save ftools e()
cap noi assert "`e(noftools)'"==""  // will be error if ftools not installed
pdslasso ys L.k (L(1/3).w yr*), fe noftools
assert "`e(noftools)'"=="noftools"
savedresults comp ftools e()


// *************** END ABDATA DATASET ************************ //

cap log close
set more on
set rmsg off

