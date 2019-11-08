*! lasso2 1.0.05 4apr2018
*! authors aa/ms
* eclass wrapper for elastic net & sqrt-lasso estimation
* all partialling, transformations, standardisation, FE, tempvars handled here
* keeps lists of original and temp varlists
* plotting supported using "plotpath" program
* display of output handled by "DisplayPath" and "DisplayCoefs" programs
* marksample and markout used here so that e(sample) can be saved
* options relevant to eclass and saved results spelled out here
* all varlists and options passed to lassoshooting
* supports replay syntax
* lasso2 accommodates two cases: scalar lambda or list of lambdas (default)

* Updates (release date):
* 1.0.03  (30jan2018)
*         First public release.
*         Promoted to require version 13 or higher.
*         Added holdout option and related changes.
*         Replaced noprestd with prestd and related changes; added unitloadings option.
*         Recoding of cons and demeaning flags. Std loadings based on demeaned vars even with nocons.
*         partial and nocons no longer compatible.
* 1.0.04  (10feb2018)
*         Support for Sergio Correia's FTOOLS FE transform (if installed).
* 1.0.05  (4apr2018)
*         Support for information criteria added. DisplayPath program has been modified accordingly.
* 		  lic(string) and ic(string) options were added. Added various results that are stored in e().
*		  See lassoutils 1.0.08 for underlying technical changes.
*		  Added ic(none) and noic options; both suppress calculation of information criteria.


program lasso2, eclass sortpreserve
	version 13
	syntax [anything] [if] [in] [,					///
			PLOTpath(string)						/// "norm" or "lambda" allowed
			PLOTVar(varlist min=1 fv ts)			/// optional subset of variables to plot
			PLOTOpt(string)							/// options to pass to graph command
			PLOTLabel 								///
			POSTEst 								///
			NEWLambda(numlist >0 min=1 max=1)		///
			NEWAlpha(numlist >=0 min=1 max=1)		///
			wnorm									///
			NOPATH									/// suppress display
			displayall 								/// display zeros of beta, only applicable for one lambda
			NORecover								///
			long 									///
			lic(string)								/// for replay
			ic(string)								/// for lasso2 output
			NOIC 									///
			* 										///
			]
	
	*** noic and ic(none)
	if "`noic'"!="" {
		local ic none
	}
	if "`ic'"=="none" {
		local noic noic
	}
	*
	
	local inhold=0
	if (~replay()) & ("`lic'"!="") {
		di as err "lic only allowed as replay command. Type 'lasso2, lic' after estimation."
		exit 198 	
	}
	else if (~replay()) & ("`lic'"=="") { 
		// no replay. estimate model.
		// this is the standard case of a fully specified model.
		tokenize "`0'", parse(",")
		_lasso2 `1', `options'  ///
						newlambda(`newlambda') ///	
						newalpha(`newalpha') ///
						`norecover' `noic'
		ereturn local lasso2opt `options'  
	}
	else if (replay()) & ("`newlambda'`newalpha'"!="") & ("`lic'"=="") {
		// replay syntax. 
		// re-estimate model with (new) single lambda (and alpha) value.
		// newlambda() option is undocumented, 
		// and only used within lasso2_p.
		if ("`e(cmd)'"!="lasso2") {
			di as error "lasso2 estimation results not found"
			exit 301
		}
		local depvar `e(depvar)'
		local varXmodel `e(varXmodel)'
		local lasso2opt `e(lasso2opt)'
		tempvar esample
		gen `esample' = e(sample) // ensure same sample is used
		_lasso2 `depvar' `varXmodel' if `esample', ///
								`lasso2opt' ///
								newlambda(`newlambda') ///
								newalpha(`newalpha')
		ereturn local lasso2opt `lasso2opt' 
	}
	else if (replay()) & ("`newlambda'"=="") & ("`lic'"!="") {
		// replay syntax. 
		// re-estimate model with (new) single lambda (and alpha) value.
		// newlambda() option is undocumented, 
		// and only used within lasso2_p.
		if ("`e(cmd)'"!="lasso2") {
			di as error "lasso2 estimation results not found"
			exit 301
		}
		local depvar `e(depvar)'
		local varXmodel `e(varXmodel)'
		local lasso2opt `e(lasso2opt)'
		if ("`lic'"=="bic") {
			local newlambda = e(lbic)
		}
		else if ("`lic'"=="aic") {
			local newlambda = e(laic)
		}
		else if ("`lic'"=="aicc") {
			local newlambda = e(laicc)
		}
		else if ("`lic'"=="ebic") {
			local ic ebic
			local newlambda = e(lebic)	
		}
		else if ("`lic'"=="") {
			di as err "lic() option required."
			exit 198
		}
		else {
			di as err "lic(`lic') not allowed. Select aic, bic, aicc or ebic."
			exit 198		
		}
		local licstrupper=strupper("`lic'")
		di as text "Use lambda=`newlambda' (selected by `licstrupper')."
		tempvar esample
		gen `esample' = e(sample) // ensure same sample is used
		if ("`postest'"=="") {
			tempname model0
			_estimates hold `model0'
			local inhold = 1
		}
		_lasso2 `depvar' `varXmodel' if `esample', ///
								`lasso2opt' ///
								newlambda(`newlambda') ///
								//newalpha(`newalpha')
		ereturn local lasso2opt `lasso2opt' 
	}
	else {
		if ("`e(cmd)'"!="lasso2") {
			di as error "lasso2 estimation results not found"
			exit 301
		}
	}
 
	if `e(lcount)'==1 {
		DisplayCoefs, `displayall' `norecover'
		if ("`plotpath'`plotvar'`plotopt'`plotlabel'"!="") {
			di as error "Plotting only supported for list of lambda values."
			di as error "Plotting options ignored."
		}
	}
	else if (`e(lcount)'>1) & !missing(`e(lcount)') {
		if "`nopath'"=="" {
			DisplayPath, `wnorm' `long' ic(`ic') 
		}
		if ("`plotpath'`plotvar'`plotopt'"!="")  {
			plotpath, plotpath(`plotpath') 		///
					  plotvar(`plotvar')   		///
					  plotopt(`plotopt') 		///
					  `plotlabel'				///
					  `wnorm'
		}
	}
	else {
		di as error "lasso2 estimation results not found"
		exit 301 
	}
	if ("`postest'"=="") & (`inhold'==1) {
		_estimates unhold `model0'
	}
end

program _lasso2, eclass sortpreserve
	version 13

	syntax varlist(numeric min=2 fv ts) [if] [in] [,	///
			NOTPen(string) 							/// list of variables not penalised
			PARtial(string)							/// string so that list can contain "_cons"
			fe										/// do within-transformation
			NOCONStant								///
			NORecover 								/// recover partialled out coefficients
			///
			/// debug & more info
			debug									/// used for debugging
			Verbose									/// pass to lassoshooting
			VVerbose								/// pass to lassoshooting
			displaynames_o(string)					/// dictionary with names of vars as supplied in varlist
			displaynames_d(string)					/// corresponding display names of vars
			pminus(int 0)							/// not used; just means rlasso code also works here
			///
			/// lambda
			Lambda(numlist >0 min=1 descending)		/// either list or scalar
			LFactor(real 1) 						/// 
			LAMBDAMat(string)						/// alternative: specify lambda as matrix
			NEWLambda(numlist >0 min=1  max=1 )		/// scalar
			NEWPloadings(string) 					///
			Ploadings(string) 						///
			UNITLoadings							///
			///
			/// standardisation
			PREStd 									///
			STDCoef 								/// 
			///
			/// choice of estimator
			ADAptive  								/// adaptive lasso
			ADATheta(real 1) 						/// gamma paramater for adapLASSO
			ADALoadings(string)						///
			ALPha(numlist >=0 ascending) 			/// elastic net parameter
			NEWAlpha(numlist >=0 min=1  max=1) 		///
			SQRT 									/// square-root lasso
			OLS										///
													///
			POSTAll									///
			holdout(varlist numeric min=1 max=1) 	///
													///
			NOFTOOLS								///
													///
			NOIC									///
			*										///
			]
	
	** reset lambda, used for predict & replay
	if ("`newlambda'"!="") {
		local lambda = `newlambda'
	}
	if ("`newalpha'"!="") {
		local alpha = `newalpha'
	}
	if ("`newploadings'"!="") {
		tempname ploadings
		mat `ploadings' = `newploadings'
		// clear these macros
		local adaptive
		local prestd
	}
	// set alpha default. 
	local alphacount	: word count `alpha'
	if (`alphacount'==0) {
		local alpha = 1	
	}
	else if (`alphacount'>1) {
		di as err "alpha() must be a scalar."
		exit 198
	}
	// adapative - any adaptive options/variants implies adaptive
	if ("`adaloadings'"~="") | (`adatheta'~=1) {
		local adaptive adaptive
	}
	*
	
	****** syntax checks *******************************************************
	if (`alpha'>1) | (`alpha'<0) {
		di as err "alpha is out of range."
		exit 198
	}
	if ("`sqrt'"!="") & (`alpha'!=1) {
		di as error "sqrt-lasso only allowed with alpha=1."
		exit 198
	}
	local notpenpar : list notpen & partial
	if ("`notpenpar'"!="") {
		di as error "`notpenpar' listed in both notpen(.) and partial(.)"
		exit 198
	}
	if ("`stdcoef'"!="") & ("`prestd'"!="") {
		di as text "note: option stdcoef implies prestd; data will be pre-standardized" 
		local prestd prestd
	}
	local checkflag 	= ("`ploadings'"!="")+("`unitloadings'"!="")+("`adaptive'"!="")
	if `checkflag'>1 {
		di as error "error: cannot combine options ploadings(.), unitloadings and/or adaptive"
		exit 198
	}
	*
	****************************************************************************
	
	*** debug mode; create flag
	local debugflag	=("`debug'"~="")
	*
	
	*** Record which observations have non-missing values
	marksample touse
	markout `touse' `varlist' `ivar' `holdout'
	sum `touse' if `touse', meanonly		//  will sum weight var when weights are used
	local N		= r(N)
	tempvar toest
	qui gen `toest' = `touse'
	if ("`holdout'"!="") {
		assert `holdout' == 1 | `holdout'==0 if `touse'
		qui replace `toest' = 0 if `holdout'
	}
	*

	*** FEs. Create 1/0 flag.
	// Get panel id
	local feflag=("`fe'"~="")
	if `feflag' {
		cap _xt
		if _rc ~= 0 {
			di as err "Error: fe option requires data to be xtset"
			exit 459
		}
		else {
			local ivar `r(ivar)'
		}
	}
	*
	
	*** constant, partial, etc.
	// conmodel: constant in original model
	// consflag: constant in transformed equation to estimate
	local consmodel		=("`noconstant'"=="") & ~`feflag'	// if fe, then cons=0 & partialcons=""
	local partial		: subinstr local partial "_cons" "", all word count(local pconscount)
	local notpen		: subinstr local notpen "_cons" "", all word count(local notpenconscount)
	if (`notpenconscount'>1) {
		di as err "Warning: notpen(_cons) not supported. Constant is always partialled out."
	}
	local partialflag	= ("`partial'"~="")   	// =1 if regressor other than constant is partialled out
	local notpenflag	= ("`notpen'"~="")  	// =1 if regressor other than constant is not penalised
	local stdcoefflag	= ("`stdcoef'"~="")		// return coef estimates in std units
	local prestdflag 	= ("`prestd'"~="")		// =1 if data to be pre-standardized
	// default is to use standardization loadings; overridden by ploadings, unitloadings, pre-standardization, adaptive
	local stdloadflag	= ("`ploadings'`unitloadings'`prestd'`adaptive'"=="")
	local sqrtflag 		= ("`sqrt'"!="")
	local parrflag		= ("`norecover'"=="")	
	// if partial list has factor vars, will need to be replaced with tempvars
	cap _fv_check_depvar `partial'
	local partialfvflag	=(_rc==198)
	// Tell estimation code if cons has been partialled out or there isn't one in the first place
	if `feflag' | `partialflag' | `prestdflag' | (~`consmodel') {
		local consflag	0
	}
	else {
		local consflag	1
	}
	*
	
	*** create main varlist and tempvars
	// remove duplicates from varlist
	// _o list is vars with original names
	fvexpand `varlist' if `touse'  
	local varlist_o	`r(varlist)'
	// check for duplicates has to follow expand
	local dups			: list dups varlist_o
	if "`dups'"~="" {
		di as text "Dropping duplicates: `dups'"
	}
	local varlist_o		: list uniq varlist_o
	*

	*** Create separate _o varlists: Y, X, notpen, partial
	// Y, X
	local varY_o		: word 1 of `varlist_o'
	local varX_o		: list varlist_o - varY_o				//  incl notpen/partial
	// notpen
	fvexpand `notpen' if `touse'
	local notpen_o		`r(varlist)'
	local dups			: list dups notpen_o
	if "`dups'"~="" {
		di as text "Dropping duplicates: `dups'"
	}
	local notpen_o		: list uniq notpen_o
	// partial
	fvexpand `partial' if `touse'
	local partial_o		`r(varlist)'
	local dups			: list dups partial_o
	if "`dups'"~="" {
		di as text "Dropping duplicates: `dups'"
	}
	local partial_o		: list uniq partial_o
	// "model" = vars without partialled-out
	local varXmodel_o	: list varX_o - partial_o
	*
	
	*** syntax checks
	// check that notpen vars are in full list
	local checklist	: list notpen_o - varX_o
	local checknum	: word count `checklist'
	if `checknum' {
		di as err "syntax error - `checklist' in notpen(.) but not in list of regressors"
		exit 198
	}
	// check that partial vars are in full list
	local checklist	: list partial_o - varX_o
	local checknum	: word count `checklist'
	if `checknum' {
		di as err "syntax error - `checklist' in partial(.) but not in list of regressors"
		exit 198
	}
	// check that ivar (FE) is not a used variable
	if `feflag' {
		fvrevar `varY_o' `varX_o', list					//  list option means we get only base vars
		local vlist `r(varlist)'
		local checklist	: list ivar - vlist
		local checknum	: word count `checklist'
		if `checknum'==0 {
			di as err "syntax error - `ivar' is xtset variable and cannot be used in model"
			exit 198
		}
	}
	// other checks
	if `pconscount' & `feflag' {
		di as err "error: incompatible options, partial(_cons) and fe"
		exit 198
	}
	if "`partial'"~="" & "`noconstant'"~="" {
		di as err "error: incompatible options, partial and nocons"
		exit 198
	}
	if `feflag' & "`noconstant'"~="" {
		di as err "error: incompatible options, fe and nocons"
		exit 198
	}
	*
	
	*** Create _t varlists: Y, X, notpen, partial
	// _o list is vars with original names
	// _t list is temp vars if transform needed, original vars if not
	if `feflag' {												//  everything needs to be transformed including partial
		local temp_ct : word count `varlist_o'
		mata: s_maketemps(`temp_ct')
		local varlist_t `r(varlist)'
	}
	else if `partialflag' | `prestdflag' {						//  everything except partial_o needs to be transformed
		local varYXmodel_o `varY_o' `varXmodel_o'
		local temp_ct : word count `varYXmodel_o'
		mata: s_maketemps(`temp_ct')
		local varYXmodel_t `r(varlist)'
		matchnames "`varlist_o'" "`varYXmodel_o'" "`varYXmodel_t'"
		local varlist_t		`r(names)'
	}
	else {														//  no transformation needed but still need temps
		fvrevar `varlist_o' if `touse'							//  fvrevar creates temps only when needed
		local varlist_t		`r(varlist)'
	}
	// dictionary is now varlist_o / varlist_t
	// now create separate _o and _t varlists using dictionary
	foreach vlist in varY varX varXmodel notpen partial {
		matchnames "``vlist'_o'" "`varlist_o'" "`varlist_t'"
		local `vlist'_t		`r(names)'						//  corresponding tempnames; always need this because of possible fvs
	}
	*

	******************* Display names ***********************************************************
	//  may be called by another program with tempvars and display names for them
	//  if display names option not used, use _o names as provided in rlasso command
	//  if display names option used, use display names matched with _o names
	//  if display names macros are empty, has no effect
	matchnames "`varY_o'" "`displaynames_o'" "`displaynames_d'"
	local varY_d		`r(names)'
	matchnames "`varXmodel_o'" "`displaynames_o'" "`displaynames_d'"
	local varXmodel_d	`r(names)'
	matchnames "`varX_o'" "`displaynames_o'" "`displaynames_d'"
	local varX_d		`r(names)'
	matchnames "`notpen_o'" "`displaynames_o'" "`displaynames_d'"
	local notpen_d		`r(names)'
	matchnames "`partial_o'" "`displaynames_o'" "`displaynames_d'"
	local partial_d		`r(names)'
	*

	*** summary varlists and flags:
	* cons			= 1 if constant, 0 if not
	* varY_o		= dep var
	* varY_t		= dep var, temp var
	* varX_o		= full, expanded set of RHS, original names, includes partial
	* varX_t		= as above but with temp names for all variables
	* varXmodel_o	= full, expanded set of RHS, original names, excludes partial
	* varXmodel_t	= as above but with temp names for all variables
	* notpen_o		= full, expanded set of not-penalized
	* notpen_t		= as above but with temp names for all variables

	//  p is number of penalized vars in the model; follows convention in BCH papers
	//  p is calculated in lassoutils/_rlasso as number of model vars excluding constant
	//  here we calculate which of the model vars are unpenalized or omitted/base vars
	//  to provide as `pminus' to lassoutils/_rlasso (unless provided by user)
	//  do here so that code above is compatible with lasso2
	//  use _o names / display names since they have info on whether var is omitted/base/etc.
	if ~`pminus' {
		foreach vn of local varXmodel_d {								//  display names
			_ms_parse_parts `vn'
			// increment pminus if model variable is MISSING
			if r(omit) {
				local ++pminus
			}
		}
		foreach vn of local notpen_d {									//  display names
			_ms_parse_parts `vn'
			// increment pminus if notpen variable is NOT MISSING
			if ~r(omit) {
				local ++pminus
			}
		}
	}
	//  p0 here is total number of variables provided to model EXCLUDING constant
	local p0	: word count `varXmodel_o'
	local p		=`p0'-`pminus'
	// warn
	if `p'<=0 {
		di as text "warning: no penalized regressors; results are OLS"
	}
	//  now for error-checking below, p0 should INCLUDE constant unless partialled-out etc.
	local p0	=`p0'+`consflag'
	*

	******************* FE, partialling out, standardization ************************************
	//  If FE:    partial-out FEs from temp variables, then preserve,
	//            then partial-out low-dim ctrls from temp variables
	//            restore will restore all temp vars with only FEs partialled-out
	//  If no FE: leave original variables unchanged.
	//            partial-out low-dim ctrls from temp variables.
	//            if no FE/low-dim ctrls, no transform needed

	local dmflag	=0										//  initialize demeaned flag
	if `feflag' {											//  FE-transform all variables
		fvrevar `varY_o' `varX_o' if `touse'				//  in case any FV or TS vars in _o list
		local vlist `r(varlist)'
		lassoutils `vlist',									/// call on _o list
						touse(`touse') 						///
						toest(`toest') 						///
						tvarlist(`varY_t' `varX_t')			/// overwrite/initialize these
						`noftools'							///
						fe(`ivar')							//  triggers branching to FE utility
		local N_g	=r(N_g)									//  N_g will be empty if no FEs
		local noftools `r(noftools)'						//  either not installed or user option
		local dmflag=1										//  data are now demeaned
		if `partialflag' {									//  And then partial out any additional vars	
			preserve										//  preserve the original values of tempvars before partialling out
			lassoutils `varY_t' `varXmodel_t',				/// _t vars have been created and filled so use here
							touse(`touse')					/// don't need tvarlist because vars already created
							toest(`toest')					/// don't need tvarlist because vars already created
							partial(`partial_t')			/// _t vars have been created and filled so use here
							partialflag(`partialflag')		/// triggers branching to partial utility
							dmflag(1)						//  FE => mean zero
		}
		if `prestdflag' {
			tempname prestdY prestdX
			lassoutils `varY_t',							/// _t vars have been created and filled so use here
							touse(`touse')					/// don't need tvarlist because vars already created
							toest(`toest')					///
							std								///
							dmflag(1)						//  FE => data already mean zero
			mat `prestdY'=r(stdvec)
			lassoutils `varXmodel_t',						/// 
							touse(`touse')					/// 
							toest(`toest')					///
							std								///
							dmflag(1)						//  FE => data already mean zero
			mat `prestdX'=r(stdvec)
		}
	}
	else if `partialflag' {									//  Just partial out
		fvrevar `varY_o' `varXmodel_o' if `touse'			//  in case any FV or TS vars in _o list
		local vlist `r(varlist)'
		fvrevar `partial_o' if `touse'						//  in case any FV or TS vars in _o list
		local pvlist `r(varlist)'
		lassoutils `vlist',									/// call on _o list
						touse(`touse') 						///
						toest(`toest') 						///
						partial(`pvlist')					///
						tvarlist(`varY_t' `varXmodel_t')	/// overwrite/initialize these
						partialflag(`partialflag')			/// triggers branching to partial utility
						dmflag(0)							//  data are not yet demeaned
		local dmflag	=1									//  data are now demeaned
		if `prestdflag' {
			tempname prestdY prestdX
			lassoutils `varY_t',							/// _t vars have been created and filled so use here
							touse(`touse')					/// don't need tvarlist because vars already created
							toest(`toest')					///
							std								///
							dmflag(1)						//  partial => already mean zero
			mat `prestdY'=r(stdvec)
			lassoutils `varXmodel_t',						/// 
							touse(`touse')					///
							toest(`toest')					///
							std								///
							dmflag(1)						//  partial => already mean zero
			mat `prestdX'=r(stdvec)
		}
	}
	else if `prestdflag' {
		tempname prestdY prestdX
		lassoutils `varY_o',								/// call on _o list
						touse(`touse')						///
						toest(`toest')						///
						std									///
						tvarlist(`varY_t')					/// overwrite/initialize these
						consmodel(`consmodel')				/// =1 => data should be demeaned
						dmflag(0)							//  data not (yet) mean zero
		mat `prestdY'=r(stdvec)
		fvrevar `varXmodel_o' if `touse'					//  in case any FV or TS vars in _o list
		local vlist `r(varlist)'
		lassoutils `vlist',									/// call on _o list
						touse(`touse')						///
						toest(`toest')						///
						std									///
						tvarlist(`varXmodel_t')				/// overwrite/initialize these
						consmodel(`consmodel')				/// =1 => data should be demeaned
						dmflag(0)							//  data not yet mean zero
		mat `prestdX'=r(stdvec)
		if `consmodel' {
			local dmflag	=1								//  if cons in model, data are now demeaned
		}
	}

 	*************** lambda to matrix **************************************************
	if ("`lambda'"!="") {
		local lcount	: word count `lambda'
		tempname lambdamat0
		mat `lambdamat0'	= J(1,`lcount',.)
		local j = 1
		foreach lami of local lambda {
			mat `lambdamat0'[1,`j'] = `lami'  
			local j=`j'+1
		}
		// optional adjustment using undocumented lfactor option
		// used for CV
		mat `lambdamat0'=`lambdamat0'*`lfactor'
	}
	else if ("`lambdamat'"!="") {
		tempname lambdamat0
		mat `lambdamat0'=`lambdamat'
		// optional adjustment using undocumented lfactor option
		// used for CV
		mat `lambdamat0'=`lambdamat0'*`lfactor'
	}
	*
	
	************* Partialling/standardization END ***********************************************
	
	*** Lasso estimation with transformed/partialled-out vars
	if "`verbose'`vverbose'"=="" {
		local quietly "quietly"							//  don't show lassoutils output
	}	

	*** Lasso estimation
	`quietly' lassoutils `varY_t',							///
					path									/// branches to _lassopath
					toest(`toest')							///
					xnames_o(`varXmodel_d')					/// display name
					xnames_t(`varXmodel_t')					///
					consflag(`consflag')					/// =0 if cons already partialled out or if no cons
					dmflag(`dmflag')						/// =1 if data have been demeaned
					notpen_o(`notpen_d') 					/// not penalised (display name)
					notpen_t(`notpen_t')					///
					lambda(`lambdamat0')					/// pass to lassoshooting
					`adaptive'								///
					adatheta(`adatheta')					///
					adaloadings(`adaloadings')				///
					`sqrt'									///
					`ols'									///
					alpha(`alpha')							///
					stdy(`prestdY')							///
					stdx(`prestdX')							///
					stdl(`stdloadflag')						/// use standardisation loadings
					stdcoef(`stdcoefflag')					/// return in standard units
					ploadings(`ploadings') 					///
					`verbose' `vverbose'					///
					holdout(`holdout')						///
					`noic' 									///
					`options'

	************* Finish up ********************************************************

	*** Create macros etc.
	local lcount	=r(lcount)
	if (`lcount'==1) { //------- scalar lambda -----------------------------------------------//	
		
		*** e-return lasso estimation results
		tempname b beta betaOLS Ups stdvec
		tempname betaAll betaAllOLS
		tempname lambda lambda0 rmse rmseOLS
		if "`cluster'" ~= "" {
			local N_clust		=r(N_clust)
		}
		mat `beta'			=r(beta)		//  may be empty!
		mat `betaOLS'		=r(betaOLS)		//  may be empty!
		mat `betaAll'		=r(betaAll)
		mat `betaAllOLS'	=r(betaAllOLS)
		mat `Ups'			=r(Ups)
		//*//mat `sUps'			=r(sUps)
		mat `stdvec'		=r(stdvec)
		scalar `lambda'		=r(lambda)
		//*//scalar `slambda'	=r(slambda)
		scalar `lambda0'	=r(lambda0)
		scalar `rmse'		=r(rmse)		//  Lasso RMSE
		scalar `rmseOLS'	=r(rmseOLS)		//  post-Lasso RMSE
		local selected		`r(selected)'	//  EXCL NOTPEN/CONS
		local selected0		`r(selected0)'	//  INCL NOTPEN, EXCL CONS
		local s				=r(s)			//  EXCL NOTPEN/CONS; number of elements in selected
		local s0			=r(s0)			//  INCL NOTPEN, EXCL CONS; number of elements in selected0
		local k				=r(k)			//  number of all variables in beta INCL NOTPEN/CONS (if present)
		local p0			=r(p0)			//  number of all variables in betaAll INCL NOTPEN/CONS (if present)
		//*//local clustvar		`r(clustvar)'
		//*//local robust		`r(robust)'
		//*//local center		=r(center)
		local sqrtflag 		=r(sqrt)
		local alpha			=r(alpha)
		local olsflag 		= r(olsflag)
		local method		`r(method)'		//  lasso or sqrt-lasso
		local niter			=r(niter)
		local maxiter		=r(maxiter)
		//*//local nupsiter		=r(nupsiter)
		//*//local maxupsiter	=r(maxupsiter)
		// issue warning if lasso max iteration limit hit
		if `niter'==`maxiter' {
			di as text "Warning: reached max shooting iterations w/o achieving convergence."
		}
		// fix depvar (rownames) of beta vectors to use _o (or _d if display names provided) not _t
		mat rownames `beta'			= `varY_d'
		mat rownames `betaOLS'		= `varY_d'
		mat rownames `betaAll'		= `varY_d'
		mat rownames `betaAllOLS'	= `varY_d'
		// used below
		if `k'>0 {										// cnames will be empty if k=0
			local cnames_o	: colnames `beta'
			fvstrip `cnames_o'							//  colnames may insert b/n/o operators - remove
			local cnames_o	`r(varlist)'
			matchnames "`cnames_o'" "`varlist_o'" "`varlist_t'"
			local cnames_t	`r(names)'
		}
		if `debugflag' {
			di as text "selected: `selected'"
			di as text "Returned results from lassoutils:"
			return list
			di as text "beta and betaOLS:"
			mat list `beta'
			mat list `betaOLS'
		}
		*
		
		*********** Get coeff estimates for partialled-out vars. ********************
		if `feflag' & `partialflag' {					//  FE case and there are partialled-out notpen vars
			restore										//  Restores dataset with tempvars after FE transform but before notpen partialled out
		}
		if (`partialflag' | (`prestdflag' & `consmodel')) & (`parrflag') {	//  standardization removes constant so must enter for that
			if `feflag' {
				local depvar `varY_t'					//  use FE-transformed depvar and X vars
				local scorevars `cnames_t'
			}
			else {
				local depvar `varY_o'					//  use original depvar and X vars
				local scorevars `cnames_o'
			}
			lassoutils `depvar',						///
				unpartial								///
				touse(`toest')							///
				beta(`beta')							///
				scorevars(`scorevars')					///
				partial(`partial_t')					///
				names_o(`varlist_o')					/// dictionary
				names_t(`varlist_t')					///	dictionary
				consmodel(`consmodel')
			mat `beta'			= r(b)
			mat `betaAll'		= `betaAll', r(bpartial)
			lassoutils `depvar',						///
				unpartial								///
				touse(`toest')							///
				beta(`betaOLS')							///
				scorevars(`scorevars')					///
				partial(`partial_t')					///
				names_o(`varlist_o')					/// dictionary
				names_t(`varlist_t')					///	dictionary
				consmodel(`consmodel')
			mat `betaOLS'		= r(b)
			mat `betaAllOLS'	= `betaAllOLS', r(bpartial)
			// finish by adding partialled-out to k
			local k				=colsof(`beta')
		}
		*	
		
		// restore here if coefs returned in standard units
		if `feflag' & `partialflag' {					//  FE case and there are partialled-out notpen vars
			restore										//  Restores dataset with tempvars after FE transform but before notpen partialled out
		}
		
		*** Post results
		if "`ols'"=="" & "`postall'"=="" {
			mat `b' = `beta'		//  selected post-lasso coeffs by default
		}
		else if "`postall'"=="" {
			mat `b' = `betaOLS'
		}
		else if "`ols'"=="" {
			mat `b' = `betaAll'
		}
		else {
			mat `b' = `betaAllOLS'
		}

		if `k'==0 {				//  no vars selected
			ereturn post    , obs(`N') depname(`varY_d') esample(`toest')		// display name
		}
		else {
			ereturn post `b', obs(`N') depname(`varY_d') esample(`toest')		// display name
		}	
		
		// additional returned results
		ereturn local noftools		`noftools'
		ereturn local postall		`postall'
		ereturn scalar niter		=`niter'
		ereturn scalar maxiter		=`maxiter'
		//*//ereturn scalar nupsiter		=`nupsiter'
		//*//ereturn scalar maxupsiter	=`maxupsiter'
		//*//ereturn local robust		`robust'
		ereturn local ivar			`ivar'
		ereturn local selected		`selected'			//  selected only
		ereturn local varXmodel		`varXmodel_d'		//  display name
		ereturn local varX			`varX_d'			//  display name
		ereturn local method		`method'
		ereturn local predict		lasso2_p
		ereturn local cmd			lasso2
		//*//ereturn scalar pminus		=`pminus'
		//*//ereturn scalar center		=`center'
		ereturn scalar cons			=`consmodel'
		//*//ereturn scalar slambda		=`slambda'
		//*//ereturn scalar lambda0		=`lambda0'
		ereturn scalar lambda		=`lambda'

		if "`N_clust'" ~= "" {
			ereturn local clustvar	`clustvar'
			ereturn scalar N_clust	=`N_clust'
		}
		if "`N_g'" ~= "" {
			ereturn scalar N_g		=`N_g'
		}
		ereturn scalar fe			=`feflag'
		ereturn scalar rmse			=`rmse'
		ereturn scalar rmseOLS		=`rmseOLS'
		ereturn scalar p			=`p'
		ereturn scalar k			=`k'				//  number of all estimated coefs INCLUDING PARTIALLED-OUT AND CONSTANT
		ereturn scalar s			=`s'				//  number of selected

		ereturn matrix stdvec		=`stdvec'
		//*//ereturn matrix sUps 		=`sUps'
		ereturn matrix Ups 			=`Ups'
		ereturn matrix betaAllOLS	=`betaAllOLS'
		ereturn matrix betaAll		=`betaAll'
		ereturn matrix betaOLS		=`betaOLS'
		ereturn matrix beta			=`beta'

		// lasso2-specific:
		// constant is always considered partialled out
		if (`consmodel') {
			local selected0		`selected0' _cons
			local partial_d		`partial_d' _cons		//  display name
		}
		local notpen_ct		: word count `notpen_d'		//  number of notpen INCLUDING CONSTANT (if not partialled-out)
		local partial_ct	: word count `partial_d'	//  number of partialled-out INCLUDING CONSTANT
		ereturn local selected0		`selected0'			//  selected or notpen, INCL CONS
		local s0			: word count `selected0'	//  overwrite s0 to include partialled-out/constant count
		ereturn scalar s0			=`s0'				//  number of selected or notpen, INCL CONS
		ereturn local partial		`partial_d'			//  display name
		ereturn local notpen		`notpen_d'			//  display name
		ereturn scalar notpen_ct	=`notpen_ct'		//  number of notpen INCLUDING CONS (unless partialled-out)
		ereturn scalar partial_ct	=`partial_ct'		//  number of partialled-out INCLUDING CONS
		*		
		
		*** more lasso2 ereturns
		ereturn scalar alpha		=`alpha'
		ereturn scalar fe 			=`feflag'
		ereturn scalar sqrt  		= `sqrtflag'
		ereturn scalar prestd		= `prestdflag'
		ereturn scalar ols 			= `olsflag'
		ereturn scalar adaptive		= "`adaptive'"!=""
		ereturn scalar lcount		=`lcount'
	
	}
	else if (`lcount'>1) { //------- list of lambdas -------------------------------------------------//
	
		*** Create macros etc.
		local nobs		=r(N)
		local lcount	=r(lcount)
		local method	`r(method)'	//  "lasso", "sqrt-lasso", "elastic net"
		local alpha 	= r(alpha)
		local sqrt		= r(sqrt)
		tempname Ups sups stdvec  
		mat `Ups' = r(Ups)
		mat `stdvec'	= r(stdvec)
		local olsflag 	= r(olsflag)
		local xvars		`varXmodel_o'
		local depvar	`r(depvar)'
		local lmin		=r(lmin)
		local lmax		=r(lmax)
		tempname betas lambdas l1norm wl1norm Rsquared dof shat
		mat `betas'		=r(betas)
		mat `lambdas'	=r(lambdalist)
		mat `l1norm'	=r(l1norm)
		mat `wl1norm'	=r(wl1norm)
		mat `Rsquared'	=r(Rsquared)
		mat `dof'		=r(dof)
		mat `shat'		=r(shat)
		local s			=r(s)
		if ("`holdout'"!="") {
			tempname mspe0
			mat `mspe0' = r(mspe)
			//mat list `mspe0'
		}	
		else if "`noic'"=="" {
			tempname rss ess tss rsq aicmat bicmat aiccmat ebicmat
			mat `rss' = r(rss)
			mat `ess' = r(ess)
			mat `tss' = r(tss)
			mat `rsq' = r(rsq)	
			// aic
			mat `aicmat' = r(aic)
			local aicmin = r(aicmin)
			local laicid = r(laicid)
			local laic = `lambdas'[`laicid',1]
			// aicc
			mat `aiccmat' = r(aicc)
			local aiccmin = r(aiccmin)
			local laiccid = r(laiccid)
			local laicc = `lambdas'[`laiccid',1]
			// ebic
			mat `ebicmat' = r(ebic)
			local ebicmin = r(ebicmin)
			local lebicid = r(lebicid)
			local lebic = `lambdas'[`lebicid',1]
			// bic
			mat `bicmat' = r(bic)
			local bicmin = r(bicmin)
			local lbicid = r(lbicid)
			local lbic = `lambdas'[`lbicid',1]
		}

		*********** Unstandardize = convert coef estimates etc. back into original units
		// standardization removed constant if present so k is number of elements in vectors
		local cnames_o	: colnames `betas'
		local pmodel	: word count `cnames_o' 

		*********** Get coeff estimates for constant and/or unpenalized vars. ********************
		if (!`feflag') & ((`partialflag') | (`consmodel' & `prestdflag')) & (`parrflag') {	
		// recovery of constant 
		// only supported if there are no other partialled out regressors and no FE

			fvstrip `cnames_o'					//  colnames may insert b/n/o operators - remove
			local cnames_o	`r(varlist)'
			matchnames "`cnames_o'" "`varlist_o'" "`varlist_t'"
			local cnames_t	`r(names)'
			
			tempname bi binew betasnew
			forvalues i= 1/`lcount' {
				mat `bi' = `betas'[`i',1..`pmodel']
				lassoutils `varY_o',						///
					unpartial								///
					touse(`toest')							///
					beta(`bi')								///
					scorevars(`cnames_o')					///
					partial(`partial_t')					///
					names_o(`varlist_o')					/// dictionary
					names_t(`varlist_t')					///	dictionary
					consmodel(`consmodel')
				mat `binew' = r(b)
				if `i'==1 {
					mat `betasnew' = `binew'
				}
				else {
					mat `betasnew' = (`betasnew' \ `binew')
				}
			}
			mat `betas' = `betasnew'
		}		
		*
	
		*** ereturns
		ereturn post    , obs(`N') depname(`varY_d') esample(`toest')	//  display name
		ereturn scalar cons 		=`consmodel'
		ereturn scalar fe 			=`feflag'
		ereturn scalar alpha		=`alpha'
		ereturn scalar sqrt  		=`sqrtflag'
		ereturn scalar ols	 		=`olsflag' 
		ereturn scalar adaptive		="`adaptive'"!=""
		ereturn scalar p 			=`p'
		local notpen_ct		: word count `notpen_d'		//  number of notpen INCLUDING CONSTANT (if not partialled-out)
		local partial_ct	: word count `partial_d'	//  number of partialled-out INCLUDING CONSTANT
		ereturn scalar notpen_ct	=`notpen_ct'		//  number of notpen INCLUDING CONS (unless partialled-out)
		ereturn scalar partial_ct	=`partial_ct'		//  number of partialled-out INCLUDING CONS
		ereturn scalar prestd		=`prestdflag'
		ereturn scalar lcount 		=`lcount'
		ereturn scalar lmax			=`lmax'
		ereturn scalar lmin			=`lmin'
		ereturn local noftools		`noftools'
		ereturn local method		`method'
		ereturn local predict		lasso2_p
		ereturn local cmd			lasso2
		ereturn local varXmodel		`varXmodel_d'		//  display name
		ereturn local varX			`varX_d'			//  display name
		ereturn local partial		`partial_d'			//  display name
		ereturn local notpen		`notpen_d'			//  display name
		ereturn matrix l1norm		=`l1norm'
		ereturn matrix wl1norm		=`wl1norm'
		ereturn matrix Ups			=`Ups'
		ereturn matrix betas		=`betas' 	 
		ereturn matrix dof			=`dof'
		ereturn matrix s			=`shat'
		ereturn matrix lambdamat	=`lambdas'
		ereturn matrix stdvec		=`stdvec'
		if ("`holdout'"!="") {
			ereturn matrix mspe = `mspe0'
		}
		else if "`noic'"=="" {
			ereturn scalar aicmin = `aicmin'
			//ereturn scalar laicid = `laicid'
			ereturn scalar bicmin = `bicmin'
			//ereturn scalar lbicid = `lbicid'
			ereturn scalar aiccmin = `aiccmin'
			//ereturn scalar laiccid = `laiccid'
			ereturn scalar ebicmin = `ebicmin'
			//ereturn scalar lebicid = `lebicid'
			ereturn matrix rss 		= `rss' 
			ereturn matrix ess 		= `ess' 
			ereturn matrix tss		= `tss' 
			ereturn matrix rsq 		= `rsq' 
			ereturn matrix aic 		= `aicmat' 
			ereturn scalar laic		= `laic'
			ereturn matrix bic 		= `bicmat' 
			ereturn scalar lbic		= `lbic'
			ereturn matrix aicc		= `aiccmat' 
			ereturn scalar laicc	= `laicc'
			ereturn matrix ebic		= `ebicmat' 
			ereturn scalar lebic 	= `lebic'
		}
	}
end


program define plotpath

	syntax [anything] [, 	plotvar(string)		///
							plotpath(string)	///
							plotopt(string)		///
							plotlabel			///
							wnorm				///
							]
	
	version 12
	
	if ("`plotpath'"!="") {
	
		if (("`plotpath'"!="lambda") & ("`plotpath'"!="norm") & ("`plotpath'"!="lnlambda")) {
			di as err "Plotpath() allows 'lambda' or 'norm'."
			error 198
		}
	
// Contents of b matrix and lambda vector made into Stata variables for plotting.
// Varnames taken from colnames of b matrix.
// Strip out constant (if it's there) since creating a variable called _cons not alllowed.
// If `plotvar' macro is empty, graph all regressors.
		tempname lambdas l1norm 
		tempvar touse
		gen `touse'=e(sample)
		mat b=e(betas)
		//mat lambdalist = e(lambdalist)
		mat `lambdas' = e(lambdamat)
		if "`wnorm'"=="" {
			mat `l1norm' = e(l1norm)
		}
		else {
			mat `l1norm' = e(wl1norm)
		}
		local lcount = e(lcount)
		local cons = e(cons)
		if `cons' {
			local rb1 = colsof(b) - 1	//  constant is in the last column
			mat b = b[1...,1..`rb1']
		}
		local bnames : colnames b
		fvstrip `bnames'				//  annoying - Stata inserts b/n etc. in first factor variable etc.
		local bnames `r(varlist)'
// process pv names
		if "`plotvar'"=="" {
			local pvnames `bnames'		//  plot all
		}
		else {							//  plot user-provided
			fvstrip `plotvar' if `touse', expand dropomit
			local pvnames	`r(varlist)'
		}
		foreach pvn in `pvnames' {		//  unab one-by-one to standardise, get i prefix etc.
			fvunab pvn_unab	: `pvn'
			local pvnames_unab `pvnames_unab' `pvn_unab'
		}
// process b names
		foreach bn in `bnames' {		//  unab one-by-one to standardise, get i prefix etc.
			fvunab bn_unab	: `bn'
			local bnames_unab `bnames_unab' `bn_unab'
		}
// now that unabbreviated varlists are prepared, check that plotvars are in regressors
		if "`plotvar'"~="" {
			local nplotvarcheck	 : list pvnames_unab - bnames_unab
			if ("`nplotvarcheck'"!="") {								
				di as error "Variable(s) `nplotvarcheck' of plotvar() not listed as regressor(s)." 
				exit 198
			}
		}
// in case there are any . operators included, change to "_"
		local bnames	: subinstr local bnames_unab "." "_", all count(local numsubs)
		local pvnames	: subinstr local pvnames_unab "." "_", all count(local numsubs)
// check for max number of variables to plot
		local npv : word count `pvnames'
		if `npv' >= 100 {
			di as err "Error: lassopath can graph at most 99 regressors"
			di as err "       use plotvar(.) option to specify subset of regressors"
			exit 103
		}

// create graphing data and then plot
 		preserve						//  do this here so that above vars exist
		clear
		qui svmat b
		foreach var of varlist b* {
			tokenize `bnames'
			rename `var' `1'
			mac shift
			local bnames `*'
		}
		if "`plotpath'"=="lnlambda" {
			qui svmat `lambdas', names("lambda")
			replace lambda = ln(lambda)
			if ("`plotlabel'"!="") {
				local txt
				local xcoord = -abs(lambda[_N])*1.03
				local xcoordminus = -abs(lambda[_N])*1.25
				foreach var of varlist `pvnames' {	
					local ycoord = `var'[_N] 
					local vn = abbrev("`var'",8)
					local txt `txt' text(`ycoord' `xcoord' `"`vn'"', place(w) just(left) size(small))	
				}
				local yscalealt yscale(alt)
				local xscale xscale(range(`xcoordminus'))		//  extend plot area on left to allow room for varnames
			}  
			twoway line `pvnames' lambda, `plotopt' `txt' `yscalealt' xtit("ln(Lambda)") `graphr' `xscale'
		}
		else if "`plotpath'"=="lambda" {
			qui svmat `lambdas', names("lambda")
			//replace lambda = ln(lambda)
			if ("`plotlabel'"!="") {
				local txt
				local xcoord = -abs(lambda[1])*0.03
				local xcoordminus = -abs(lambda[1])*0.15
				foreach var of varlist `pvnames' {	
					local ycoord = `var'[_N] 
					local vn = abbrev("`var'",8)
					local txt `txt' text(`ycoord' `xcoord' `"`vn'"', place(w) just(left) size(small))	
				}
				local yscalealt yscale(alt)
				local xscale xscale(range(`xcoordminus'))		//  extend plot area on left to allow room for varnames
			}  
			twoway line `pvnames' lambda, `plotopt' `txt' `yscalealt' xtit("Lambda") `graphr' `xscale'
		}
		else {
			qui svmat `l1norm', names("l1norm")
 			sort l1norm1
			if ("`plotlabel'"!="") {
				local txt
				local xcoord = l1norm1[_N]*1.02		//  extend plot area on right to allow room for varnames
				local xcoordplus = l1norm1[_N]*1.1
				foreach var of varlist `pvnames' {
					local ycoord = `var'[_N]
					local vn = abbrev("`var'",8)
					local txt `txt' text(`ycoord' `xcoord' `"`vn'"', place(e) just(left) size(small))
				}
				local xscale xscale(range(`xcoordplus'))
			}
			if "`wnorm'"=="" {
				local xtitle L1 Norm
			}
			else {
				local xtitle Weighted L1 Norm
			}

			line `pvnames' l1norm, `plotopt' `txt' xtit("`xtitle'") `graphr' `xscale'
		}

 		restore
	}
	*
	
end


// Display table of path with knots, lambda, vars added/removed etc.
program define DisplayPath
	//syntax [anything] [, stdcoef(int 0)]
	syntax [anything] [, wnorm long ic(string)]

	version 12
	tempname betas r1 r2 vnames d addedM removedM lambdas dof l1norm vnames0 allselected allsec
	tempname icmat rsq
	
	***** information criteria *************************************************
	if ("`ic'"=="") {
		local ic ebic
	}
	if ("`ic'"!="aic") & ("`ic'"!="bic") & ("`ic'"!="aicc") & ("`ic'"!="ebic") & ("`ic'"!="none") {
		di as err "Option ic(`ic') not allowed. Using the default ic(ebic)."
		local ic ebic
	}
	if ("`ic'"=="ebic") {
		mat `icmat' 	=e(ebic)
		local icmin 	=e(ebicmin)
		local ic EBIC
	}
	else if ("`ic'"=="aic") {
		mat `icmat' 	=e(aic)
		local icmin 	=e(aicmin)
		local ic AIC
	}
	else if ("`ic'"=="bic") {
		mat `icmat' 	=e(bic)
		local icmin 	=e(bicmin)
		local ic BIC
	}
	else if ("`ic'"=="aicc") {
		mat `icmat' 	=e(aicc)
		local icmin 	=e(aiccmin)	
		local ic AICc
	}
	else {
		mat `icmat' 	=.
		local icmin 	=.
		local ic IC
	}
	****************************************************************************
	
	mat `lambdas'	=e(lambdamat)
	mat `dof'		=e(s)
	mat `rsq' 		=e(rsq)
	mata: `vnames'	=st_matrixcolstripe("e(betas)")		// starts as k x 2
	mata: `vnames'	=(`vnames'[.,2])'					// take 2nd col and transpose into row vector
	mata: `betas'	=st_matrix("e(betas)")
	mata: `r1'		=(`betas'[1,.] :!= 0)
	mata: `addedM'	=select(`vnames', (`r1' :== 1))
	mata: st_local("added",invtokens(`addedM'))
	local knot		=0
	mata: `r1'		=J(1,cols(`betas'),0)
	di
	if "`wnorm'"=="" {
		mat `l1norm' = e(l1norm)
		di as text "  Knot{c |}  ID     Lambda    s      L1-Norm        `ic'" _c
		di as text _col(58) "R-sq   {c |} Entered/removed"
	}
	else {
		mat `l1norm' = e(wl1norm)
		di as text "  Knot{c |}  ID     Lambda    s     wL1-Norm        `ic'" _c
		di as text _col(58) "R-sq   {c |} Entered/removed"  
	}
	di as text "{hline 6}{c +}{hline 57}{c +}{hline 16}"
	forvalues i=1/`e(lcount)' {
		mata: `r2'			=(`betas'[`i',.] :!= 0)
		mata: `d'			=`r2'-`r1'
		mata: `addedM'		=select(`vnames',(`d' :== 1))
		mata: `allselected' = (sum(`r2':==0))==0 // = 1 if all selected
		mata: st_numscalar("`allsec'",`allselected')
		mata: st_local("added",invtokens(`addedM'))
		mata: `removedM'	=select(`vnames',(`d' :== -1))
		mata: st_local("removed",invtokens(`removedM'))
		if ("`added'`removed'" ~= "") | ("`long'"!="") { 
			if ("`added'`removed'" ~= "") {
				local ++knot
				di as res %6.0f `knot' _c
			}
			di as text _col(7) "{c |}" _c
			di as res %4.0f `i' _c
			di as res _col(13) %10.5f el(`lambdas',`i',1) _c
			di as res _col(25) %4.0f el(`dof',`i',1) _c
			di as res _col(31) %10.5f el(`l1norm',`i',1) _c
			di as res _col(43) %11.5f el(`icmat',`i',1) _c			//  can be negative so add a space
			if ("`long'"!="") & (reldif(`icmin',el(`icmat',`i',1))<10^-5) & ("`icmin'"!=".") {
				di as text "*" _c
			}
			else {
				di as text " " _c
			}
			di as res _col(56) %7.4f el(`rsq',`i',1) _c
			di as text _col(65) "{c |}" _c
			// clear macro
			macro drop _dtext
			if (`i'==1) & (`allsec') {
				local dtext All selected.
			}
			else {
				if "`added'" ~= "" {
					local dtext Added `added'.
				}
				if "`removed'" ~= "" {
					local dtext `dtext' Removed `removed'.
				}
			}
			DispVars `dtext', _lc1(7) _lc2(65) _col(67)
		}
		mata: `r1'		=`r2'
	}
	local iclower = strlower("`ic'")
	if ("`long'"=="") {
		di as text "Use 'long' option for full output." _c
	}
	else if ("`ic'"!="IC") {
		di as text "{helpb lasso2##aicbic:*}indicates minimum `ic'." _c
	}
	if ("`ic'"!="IC") {
		di as text " Type '" _c
		di in smcl "{stata lasso2, lic(`iclower')}" _c
		di as text "' to run the model selected by `ic'."
	}
	mata: mata drop `betas' `r1' `r2' `vnames' `d' `addedM' `removedM'
	
end

// Display varlist with specified indentation
program define DispVars
	version 11.2
	syntax [anything] [, _col(integer 15) _lc1(integer 0) _lc2(integer 0) ]
	local maxlen = c(linesize)-`_col'
	local len = 0
	local first = 1
	foreach vn in `anything' {
		local vnlen		: length local vn
		if `len'+`vnlen' > `maxlen' {
			di
			local first = 1
			local len = `vnlen'
			if `_lc1' {
				di as text _col(`_lc1') "{c |}" _c
			}
			if `_lc2' {
				di as text _col(`_lc2') "{c |}" _c
			}
		}
		else {
			local len = `len'+`vnlen'+1
		}
		if `first' {
			local first = 0
			di as res _col(`_col') "`vn'" _c
			}
		else {
			di as res " `vn'" _c
		}
	}
* Finish with a newline
	di
end

// Used in rlasso and lasso2 rlasso.
// version  2017-12-20
prog DisplayCoefs

	syntax	,								///
		[									///
		displayall							///  full coef vector in display (default=selected only)
		varwidth(int 17)					///
		NORecover 						///
		]
	
	local cons			=e(cons)
	if ("`norecover'"=="") {
		local partial		`e(partial)'
		local partial_ct	=e(partial_ct)
	}
	else {
		local partial
		local partial_ct	=0
	}

	// varlists
	local selected		`e(selected)'
	fvstrip `selected'
	local selected		`r(varlist)'
	local notpen		`e(notpen)'
	fvstrip `notpen'
	local notpen		`r(varlist)'
	local selected0		`e(selected0)'
	fvstrip `selected0'
	local selected0		`r(varlist)'
	// coef vectors
	tempname beta betaOLS
	if "`displayall'"~="" {						//  there must be some vars specified even if nothing selected
		mat `beta'		=e(betaAll)
		mat `betaOLS'	=e(betaAllOLS)
		local col_ct	=colsof(`beta')
		local vlist		: colnames `beta'
		local vlistOLS	: colnames `betaOLS'
		local baselevels baselevels
	}
	else if e(k)>0 {							//  display only selected, but only if there are any
		mat `beta'		=e(beta)
		mat `betaOLS'	=e(betaOLS)
		local col_ct	=colsof(`beta')
		local vlist		: colnames `beta'
		local vlistOLS	: colnames `betaOLS'
	}
	else {										//  nothing selected, zero columns in beta
		local col_ct	=0
	}
	if e(k)>0 {
		_ms_build_info `beta' if e(sample)
		_ms_build_info `betaOLS' if e(sample)
	}

	*** (Re-)display coefficients including constant/partial
	local varwidth1		=`varwidth'+1
	local varwidth3		=`varwidth'+3
	local varwidth4		=`varwidth'+4
	local varwidthm7	=`varwidth'-7
	local varwidthm13	=`varwidth'-13
	di
	di as text "{hline `varwidth1'}{c TT}{hline 32}"
	if "`e(method)'"=="sqrt-lasso" {
		di as text _col(`varwidthm7') "Selected {c |}      Sqrt-lasso   Post-est OLS"
	}
	else if "`e(method)'"=="ridge" {
		di as text _col(`varwidthm7') "Selected {c |}           Ridge   Post-est OLS"
	}
	else if "`e(method)'"=="elastic net" {
		di as text _col(`varwidthm7') "Selected {c |}     Elastic net   Post-est OLS"
		di as text _col(`varwidthm7') "         {c |}" _c
		di as text "   (alpha=" _c
		di as text %4.3f `e(alpha)' _c
		di as text ")"
	}
	else if "`e(method)'"=="lasso" {
		di as text _col(`varwidthm7') "Selected {c |}           Lasso   Post-est OLS"
	}
	else {
		di as err "internal DisplayCoefs error. unknown method."
		exit 1
	}
	di as text "{hline `varwidth1'}{c +}{hline 32}"
	local anynotpen = 0
	local i 1
	local lastcol = `col_ct' - `partial_ct'
	tokenize `vlist'								//  put elements of coef vector into macros 1, 2, ...
	while `i' <= `lastcol' {
		local vn ``i''
		fvstrip `vn'								// get rid of o/b/n prefix for display purposes
		local vn		`r(varlist)'
		_ms_display, element(`i') matrix(`beta') width(`varwidth') `baselevels'
		// in selected or notpen list?
		local isselnotpen	: list posof "`vn'" in selected0
		local isnotpen		: list posof "`vn'" in notpen
		local anynotpen		= `anynotpen' + `isnotpen'
		// note attached? base, empty, omitted
		qui _ms_display, element(`i') matrix(`beta')
		local note `r(note)'
		qui _ms_display, element(`i') matrix(`betaOLS')
		local noteOLS `r(note)'
		// if notpen, add footnote
		if `isnotpen' & "`note'"=="" {
			di as text "{helpb rlasso##notpen:*}" _c
		}
		if `isselnotpen' {
			// lasso coef
			if "`note'"=="" {
				di _col(`varwidth4') as res %15.7f el(`beta',1,`i') _c
			}
			else {
				di _col(`varwidth4') as text %15s "`note'" _c
			}
			// post-lasso coef - can be omitted if collinear
			if "`noteOLS'"=="" {
				di as res %15.7f el(`betaOLS',1,`i')
			}
			else {
				di as text %15s "`noteOLS'"
			}
		}
		else if "`note'"=="(omitted)" {
			// not selected
			di _col(`varwidth4') as text %15s "(not selected)" _c
			di                   as text %15s "(not selected)"
		}
		else {
			// other eg base var
			di as text %15s "`note'" _c
			di as text %15s "`noteOLS'"
		}
		local ++i
	}
	if `partial_ct' {
		di as text "{hline `varwidth1'}{c +}{hline 32}"
		di as text _col(`varwidthm13') "Partialled-out{help lasso2##examples_partialling:*}{c |}"
		di as text "{hline `varwidth1'}{c +}{hline 32}"
		local i = `lastcol'+1
		while `i' <= `col_ct' {
			local vn ``i''
			fvstrip `vn'								// get rid of o/b/n prefix for display purposes
			local vn		`r(varlist)'
			_ms_display, element(`i') matrix(`beta') width(`varwidth') `baselevels'
			// note attached? base, empty, omitted
			qui _ms_display, element(`i') matrix(`beta')
			local note `r(note)'
			qui _ms_display, element(`i') matrix(`betaOLS')
			local noteOLS `r(note)'
			// lasso coef
			if "`note'"=="" {
				di _col(`varwidth4') as res %15.7f el(`beta',1,`i') _c
			}
			else {
				di _col(`varwidth4') as text %15s "`note'" _c
			}
			// post-lasso coef - can be omitted if collinear
			if "`noteOLS'"=="" {
				di as res %15.7f el(`betaOLS',1,`i')
			}
			else {
				di as text %15s "`noteOLS'"
			}
			local ++i
		}
	}
	di as text "{hline `varwidth1'}{c BT}{hline 32}"
	
	if `anynotpen' {
		di "{help lasso2##examples_partialling:*Not penalized}"
	}
	
end
	
// internal version of fvstrip 1.01 ms 24march2015
// takes varlist with possible FVs and strips out b/n/o notation
// returns results in r(varnames)
// optionally also omits omittable FVs
// expand calls fvexpand either on full varlist
// or (with onebyone option) on elements of varlist

program define fvstrip, rclass
	version 11.2
	syntax [anything] [if] , [ dropomit expand onebyone NOIsily ]
	if "`expand'"~="" {												//  force call to fvexpand
		if "`onebyone'"=="" {
			fvexpand `anything' `if'								//  single call to fvexpand
			local anything `r(varlist)'
		}
		else {
			foreach vn of local anything {
				fvexpand `vn' `if'									//  call fvexpand on items one-by-one
				local newlist	`newlist' `r(varlist)'
			}
			local anything	: list clean newlist
		}
	}
	foreach vn of local anything {									//  loop through varnames
		if "`dropomit'"~="" {										//  check & include only if
			_ms_parse_parts `vn'									//  not omitted (b. or o.)
			if ~`r(omit)' {
				local unstripped	`unstripped' `vn'				//  add to list only if not omitted
			}
		}
		else {														//  add varname to list even if
			local unstripped		`unstripped' `vn'				//  could be omitted (b. or o.)
		}
	}
// Now create list with b/n/o stripped out
	foreach vn of local unstripped {
		local svn ""											//  initialize
		_ms_parse_parts `vn'
		if "`r(type)'"=="variable" & "`r(op)'"=="" {			//  simplest case - no change
			local svn	`vn'
		}
		else if "`r(type)'"=="variable" & "`r(op)'"=="o" {		//  next simplest case - o.varname => varname
			local svn	`r(name)'
		}
		else if "`r(type)'"=="variable" {						//  has other operators so strip o but leave .
			local op	`r(op)'
			local op	: subinstr local op "o" "", all
			local svn	`op'.`r(name)'
		}
		else if "`r(type)'"=="factor" {							//  simple factor variable
			local op	`r(op)'
			local op	: subinstr local op "b" "", all
			local op	: subinstr local op "n" "", all
			local op	: subinstr local op "o" "", all
			local svn	`op'.`r(name)'							//  operator + . + varname
		}
		else if"`r(type)'"=="interaction" {						//  multiple variables
			forvalues i=1/`r(k_names)' {
				local op	`r(op`i')'
				local op	: subinstr local op "b" "", all
				local op	: subinstr local op "n" "", all
				local op	: subinstr local op "o" "", all
				local opv	`op'.`r(name`i')'					//  operator + . + varname
				if `i'==1 {
					local svn	`opv'
				}
				else {
					local svn	`svn'#`opv'
				}
			}
		}
		else if "`r(type)'"=="product" {
			di as err "fvstrip error - type=product for `vn'"
			exit 198
		}
		else if "`r(type)'"=="error" {
			di as err "fvstrip error - type=error for `vn'"
			exit 198
		}
		else {
			di as err "fvstrip error - unknown type for `vn'"
			exit 198
		}
		local stripped `stripped' `svn'
	}
	local stripped	: list retokenize stripped						//  clean any extra spaces
	
	if "`noisily'"~="" {											//  for debugging etc.
di as result "`stripped'"
	}

	return local varlist	`stripped'								//  return results in r(varlist)
end


// Internal version of matchnames
// Sample syntax:
// matchnames "`varlist'" "`list1'" "`list2'"
// takes list in `varlist', looks up in `list1', returns entries in `list2', called r(names)
program define matchnames, rclass
	version 11.2
	args	varnames namelist1 namelist2

	local k1 : word count `namelist1'
	local k2 : word count `namelist2'

	if `k1' ~= `k2' {
		di as err "namelist error"
		exit 198
	}
	foreach vn in `varnames' {
		local i : list posof `"`vn'"' in namelist1
		if `i' > 0 {
			local newname : word `i' of `namelist2'
		}
		else {
* Keep old name if not found in list
			local newname "`vn'"
		}
		local names "`names' `newname'"
	}
	local names	: list clean names
	return local names "`names'"
end


version 13
mata:
void s_maketemps(real scalar p)
{
	(void) st_addvar("double", names=st_tempname(p), 1)
	st_global("r(varlist)",invtokens(names))
}


// END MATA SECTION
end
