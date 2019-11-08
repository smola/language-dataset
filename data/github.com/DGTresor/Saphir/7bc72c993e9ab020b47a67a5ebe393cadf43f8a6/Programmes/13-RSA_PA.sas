
/**************************************************************************************************************************************************************/
/*                                   								SAPHIR E2013 L2017                                      						   		  */
/*                                       								PROGRAMME 13                                        						   		  */
/*                      									Création des foyers RSA et calcul du RSA                              					   		  */
/**************************************************************************************************************************************************************/


/**************************************************************************************************************************************************************/
/* Dans l’ERFS, les informations sont fournies à différents niveaux selon les tables. Lors de l’imputation des transferts sociaux, les familles au sens de la */
/* CAF et les foyers RSA doivent être construits au début des programmes correspondant aux prestations pour lesquelles ces unités servent de référence. Ces   */
/* constructions permettent de prendre en compte les cas où il existe plusieurs familles par ménage, mais pas ceux où une famille serait éclatée dans         */
/* plusieurs ménages. 																																		  */
/*																																							  */
/* La construction des foyers RSA/prime est plus complexe que celle des foyers CAF. Les enfants peuvent rester à charge jusqu’à l’âge de 25 ans si la 		  */
/* majoration à laquelle ils donnent droit dépasse leurs revenus. Ceci implique que le montant de RSA doit être calculé avant la détermination du périmètre   */
/* définitif des foyers. 																																	  */
/* 																																							  */
/* Ce programme recense tout d'abord les personnes qui sont considérées comme à charge sans prendre en compte les conditions de ressources. Les foyers 		  */
/* correspondant sont crées en utilisant la même méthode que pour les foyers CAF dans un premier temps. Un programme d'optimisation pour le rattachement des  */
/* personnes à charges est ensuite mis en place.																											  */
/*																																							  */	
/* Ce programme détermine enfin l'éligibilité à la CMU-c et à l'ACS.																						  */	 
/**************************************************************************************************************************************************************/



/**************************************************************************************************************************************************************/
/**************************************************************************************************************************************************************/
/*                             									I.  Création des foyers RSA                           									  	  */
/**************************************************************************************************************************************************************/
/**************************************************************************************************************************************************************/

/**************************************************************************************************************************************************************/
/* Les macros utilisées dans ce programme sont définies dans le programme 11.										  									      */
/**************************************************************************************************************************************************************/


/**************************************************************************************************************************************************************/
/*				1 - Détermination des foyers RSA dans la table individuelle											  										  */
/**************************************************************************************************************************************************************/

%fam (tabin=scenario.indiv_prest,age=25, prest=rsa, nom_unite=numfoyrsa);

data fam_prest25 ; set fam_prest25 ; 
%macro trim_BR;
%do t=1 %to 4;
BR_ind_t&t.= max(0,revactp&asuiv4._t&t.) + max(0,zchopi&asuiv4._t&t.)+ max(0,produitfin&asuiv4.)/4 
					+ max(0,zrstpi&asuiv4._t&t.)+ max(0,sum(zalri&asuiv4.,zrtoi&asuiv4.,revpatm&asuiv4.))/4; 
%end;
%mend; %trim_BR;
run ;

%macro tri_trim; 
%do t=1 %to 4;
proc sort data=fam_prest25; by ident&acour. numfoyrsa pcrsa25 BR_ind_t&t. naia naim; run; 

data fam_prest25 (compress = yes);
set fam_prest25;
by ident&acour. numfoyrsa pcrsa25 BR_ind_t&t. naia naim;
retain rgfoy_t&t. 0; /*RGFOY : rang dans le foyer RSA des PC selon leurs ressources*/
if first.numfoyrsa then rgfoy_t&t.=0;
if pcrsa25=1 then rgfoy_t&t.=rgfoy_t&t.+1;
run;
%end;
%mend; %tri_trim;

data fam_prest25;
set fam_prest25;
bonus_t1=0; bonus_t2=0; bonus_t3=0; bonus_t4=0;
%macro bonus; 
	%do i=1 %to 4;
	    if revactp&asuiv4._t&i.>0 & revactp&asuiv4._t&i.<3*&borne2_bonus. then bonus_t&i.=max(0,(revactp&asuiv4._t&i.-3*&borne1_bonus.)*3*&maxbonus./(3*&borne2_bonus.-3*&borne1_bonus.));
		if revactp&asuiv4._t&i.>=3*&borne2_bonus. then bonus_t&i.=3*&maxbonus.;
	%end ; 
%mend; %bonus; 
run;


/**************************************************************************************************************************************************************/
/*				2 - Informtions sur les personnes à charge : date de naissance, en études 									  								  */
/**************************************************************************************************************************************************************/

%pc(listvar=naia naim rgfoy_t1 rgfoy_t2 rgfoy_t3 rgfoy_t4 sexe agenq forter aah_ind
revactp&asuiv4._t1 revactp&asuiv4._t2 revactp&asuiv4._t3 revactp&asuiv4._t4
ZCHOpI&asuiv4._t1 ZCHOpI&asuiv4._t2 ZCHOpI&asuiv4._t3 ZCHOpI&asuiv4._t4 
ZRSTPI&asuiv4._t1 ZRSTPI&asuiv4._t2 ZRSTPI&asuiv4._t3 ZRSTPI&asuiv4._t4
zalri&asuiv4. zrtoi&asuiv4.
revpatm&asuiv4. zalvm&asuiv4. produitfin&asuiv4.
salaire_etr&asuiv4._t1 salaire_etr&asuiv4._t2 salaire_etr&asuiv4._t3 salaire_etr&asuiv4._t4
revactp&asuiv3._t1 revactp&asuiv3._t2 revactp&asuiv3._t3 revactp&asuiv3._t4
ZCHOpI&asuiv3._t1 ZCHOpI&asuiv3._t2 ZCHOpI&asuiv3._t3 ZCHOpI&asuiv3._t4 
ZRSTPI&asuiv3._t1 ZRSTPI&asuiv3._t2 ZRSTPI&asuiv3._t3 ZRSTPI&asuiv3._t4
zalri&asuiv3. zrtoi&asuiv3.
revpatm&asuiv3. zalvm&asuiv3. produitfin&asuiv3.
salaire_etr&asuiv3._t1 salaire_etr&asuiv3._t2 salaire_etr&asuiv3._t3 salaire_etr&asuiv3._t4
noi bonus_t1 bonus_t2 bonus_t3 bonus_t4, age=25, prest=rsa, nom_unite=numfoyrsa);


/**************************************************************************************************************************************************************/
/*				3 - Informations sur l'allocataire principal et son conjoint : enceinte, a connu une séparation, salaires... 				  				  */
/**************************************************************************************************************************************************************/

%rename (lien=prrsa,age=25, nom_unite=numfoyrsa,
liste=sexe agenq forter declarant persfip 
naiss_futur_a naiss_futur_m naiss_futur dv_fip sep_eec api_eec 
ZRSTPI&asuiv4._t1 ZRSTPI&asuiv4._t2 ZRSTPI&asuiv4._t3 ZRSTPI&asuiv4._t4
zalri&asuiv4. zrtoi&asuiv4. revpatm&asuiv4. zalvm&asuiv4. produitfin&asuiv4.
ZCHOpI&asuiv4._t1 ZCHOpI&asuiv4._t2 ZCHOpI&asuiv4._t3 ZCHOpI&asuiv4._t4 
revactp&asuiv4._t1 revactp&asuiv4._t2 revactp&asuiv4._t3 revactp&asuiv4._t4 aah_ind
salaire_etr&asuiv4._t1 salaire_etr&asuiv4._t2 salaire_etr&asuiv4._t3 salaire_etr&asuiv4._t4
ZRSTPI&asuiv3._t1 ZRSTPI&asuiv3._t2 ZRSTPI&asuiv3._t3 ZRSTPI&asuiv3._t4
zalri&asuiv3. zrtoi&asuiv3. revpatm&asuiv3. zalvm&asuiv3. produitfin&asuiv3.
ZCHOpI&asuiv3._t1 ZCHOpI&asuiv3._t2 ZCHOpI&asuiv3._t3 ZCHOpI&asuiv3._t4 
revactp&asuiv3._t1 revactp&asuiv3._t2 revactp&asuiv3._t3 revactp&asuiv3._t4 aah_ind
salaire_etr&asuiv3._t1 salaire_etr&asuiv3._t2 salaire_etr&asuiv3._t3 salaire_etr&asuiv3._t4 
bonus_t1 bonus_t2 bonus_t3 bonus_t4);

%rename (lien=cprrsa, age=25, nom_unite=numfoyrsa,
liste=sexe agenq forter declarant persfip  
ZRSTPI&asuiv4._t1 ZRSTPI&asuiv4._t2 ZRSTPI&asuiv4._t3 ZRSTPI&asuiv4._t4
zalri&asuiv4. zrtoi&asuiv4. revpatm&asuiv4. zalvm&asuiv4. produitfin&asuiv4.
ZCHOpI&asuiv4._t1 ZCHOpI&asuiv4._t2 ZCHOpI&asuiv4._t3 ZCHOpI&asuiv4._t4 
revactp&asuiv4._t1 revactp&asuiv4._t2 revactp&asuiv4._t3 revactp&asuiv4._t4 aah_ind
salaire_etr&asuiv4._t1 salaire_etr&asuiv4._t2 salaire_etr&asuiv4._t3 salaire_etr&asuiv4._t4
ZRSTPI&asuiv3._t1 ZRSTPI&asuiv3._t2 ZRSTPI&asuiv3._t3 ZRSTPI&asuiv3._t4
zalri&asuiv3. zrtoi&asuiv3. revpatm&asuiv3. zalvm&asuiv3. produitfin&asuiv3.
ZCHOpI&asuiv3._t1 ZCHOpI&asuiv3._t2 ZCHOpI&asuiv3._t3 ZCHOpI&asuiv3._t4 
revactp&asuiv3._t1 revactp&asuiv3._t2 revactp&asuiv3._t3 revactp&asuiv3._t4 aah_ind
salaire_etr&asuiv3._t1 salaire_etr&asuiv3._t2 salaire_etr&asuiv3._t3 salaire_etr&asuiv3._t4 
bonus_t1 bonus_t2 bonus_t3 bonus_t4);


/**************************************************************************************************************************************************************/
/*				4 - Ajout des prestations familiales et AL perçues par le foyer RSA 									  									  */
/**************************************************************************************************************************************************************/

proc sql ; create table prest20 as select a.*, b.numfoyrsa, b.al from  scenario.prest_fam20 as a left join fam_prest25 as b
	on a.ident&acour.=b.ident&acour. and a.noi_prpf=b.noi order by ident&acour., numfoyrsa ; quit;


proc means data=prest20 noprint nway;
by ident&acour. numfoyrsa;
var asf af afssmaj pn_paje clca  ars aah minvi cf_base 
asf_t: asf_sans_revalo_t: afssmaj_t:  ab_paje_rsa_t: ab_paje_rsa_maj_t:
clca_t: cf_base_t: al; 
output out=prest20 (drop = _TYPE_ _FREQ_) sum=;
run;


data foyrsa (compress = yes); merge famp (in=a) pcrsa prrsa cprrsa prest20 ; by ident&acour. numfoyrsa; if a;
%zero(liste=asf af afssmaj ab_paje pn_paje clca   ars al aah minvi nbpc020_12
asf_t1 asf_t2 asf_t3 asf_t4  asf_sans_revalo_t1 asf_sans_revalo_t2 asf_sans_revalo_t3 asf_sans_revalo_t4
afssmaj_t1 afssmaj_t2 afssmaj_t3 afssmaj_t4 
ab_paje_rsa_t1 ab_paje_rsa_t2 ab_paje_rsa_t3 ab_paje_rsa_t4 
ab_paje_rsa_maj_t1 ab_paje_rsa_maj_t2 ab_paje_rsa_maj_t3 ab_paje_rsa_maj_t4 
clca_t1 clca_t2 clca_t3 clca_t4 
cf_base_t1 cf_base_t2 cf_base_t3 cf_base_t4);
 run;


/**************************************************************************************************************************************************************/
/*				5 - Ajout des informations du ménage 															  											  */
/**************************************************************************************************************************************************************/

data foyrsa (compress = yes);
merge foyrsa (in=a) saphir.menage_saphir (keep= ident&acour. logt collm colla collj wprm&asuiv4.) ;
by ident&acour.;
if a;
run;


proc datasets library=work; delete pcrsa prrsa cprrsa famp indiv_prest25 prest20;run;quit;


/**************************************************************************************************************************************************************/
/**************************************************************************************************************************************************************/
/*                      											II.    Calcul du RSA                                  									  */
/**************************************************************************************************************************************************************/
/**************************************************************************************************************************************************************/

data scenario.rsa (compress =yes
drop = 	elig_rsa_t1_: elig_rsa_t2_: elig_rsa_t3_: elig_rsa_t4_:
		nbpcrsa_t1_: nbpcrsa_t2_: nbpcrsa_t3_: nbpcrsa_t4_:
		Min_rsa_t1_: Min_rsa_t2_: Min_rsa_t3_: Min_rsa_t4_:
		FL_rsa_t1_: FL_rsa_t2_: FL_rsa_t3_: FL_rsa_t4_:
		BR_RSA_t1_: BR_RSA_t2_: BR_RSA_t3_: BR_RSA_t4_:
		REVACT_pa_t1_: REVACT_pa_t2_: REVACT_pa_t3_: REVACT_pa_t4_:
		cible_pa_t1_: cible_pa_t2_: cible_pa_t3_: cible_pa_t4_:
		nbpers_fl_t1_: nbpers_fl_t2_: nbpers_fl_t3_: nbpers_fl_t4_:
		rsa_maji_t1_: rsa_maji_t2_: rsa_maji_t3_: rsa_maji_t4_:
		BR_PA_cohab_t: bonus_t1_: bonus_t2_: bonus_t3_: bonus_t4_:
		compteur					
rename=(RSA_t1=RSA_pr_t1 RSA_t2=RSA_pr_t2 RSA_t3=RSA_pr_t3 RSA_t4=RSA_pr_t4 RSA=RSA_pr
prime_t1=prime_pr_t1 prime_t2=prime_pr_t2 prime_t3=prime_pr_t3 prime_t4=prime_pr_t4 prime=prime_pr
prime_cohab_t1=prime_cohab_pr_t1 prime_cohab_t2=prime_cohab_pr_t2 prime_cohab_t3=prime_cohab_pr_t3 prime_cohab_t4=prime_cohab_pr_t4 
prime_decohab_t1=prime_decohab_pr_t1 prime_decohab_t2=prime_decohab_pr_t2 prime_decohab_t3=prime_decohab_pr_t3 prime_decohab_t4=prime_decohab_pr_t4 
RSA_prime_cumul_t1=RSA_prime_cumul_pr_t1 RSA_prime_cumul_t2=RSA_prime_cumul_pr_t2 RSA_prime_cumul_t3=RSA_prime_cumul_pr_t3 RSA_prime_cumul_t4=RSA_prime_cumul_pr_t4
prime_cohab=prime_cohab_pr prime_decohab=prime_decohab_pr RSA_prime_cumul=RSA_prime_cumul_pr));

set foyrsa;

%macro rsa;

prrsa=(noi_prrsa ne ' '); cprrsa=(noi_cprrsa ne ' ');

%do t=1 %to 4; /*boucle sur les trimestres */

	/*ENCEINTEPRRSAP3_Ti : Personne de référence enceinte de plus de 3 mois au trimestre i*/
	enceinte_prrsap3_t&t.=0;
	if sexe_prrsa='2' then do;
		%do k=1 %to &nb_max_pc.;
			if naia_pcrsa&k. ne . then do;
				if 0<(naia_pcrsa&k.*12+naim_pcrsa&k.)-(colla*12+collm-(4-&t.)*3)<=6 then enceinte_prrsap3_t&t.=1;
				else ;
			end;
		%end;
		if naiss_futur_a_prrsa ne . & 0<(naiss_futur_a_prrsa*12+naiss_futur_m_prrsa)-(colla*12+collm-(4-&t.)*3)<=6 then enceinte_prrsap3_t&t.=1;
	end;


/**************************************************************************************************************************************************************/
/*				1- Initialisation du calcul du RSA et de la prime d'activité : on met aucun enfant 															  */
/**************************************************************************************************************************************************************/

/**************************************************************************************************************************************************************/
/*		a. RSA																													 		                      */
/**************************************************************************************************************************************************************/

	/*NBPCRSA_Tt_0*/
	nbpcrsa_t&t._0=0;
	nbpcrsa02_t&t._0=0;

	/*ELIG_RSA_Tt_0*/
	elig_rsa_t&t._0=( (agenq_prrsa>=25 & forter_prrsa in ('1','3','9')) 
					! (agenq_cprrsa>=25 & forter_cprrsa in ('1','3','9')) 
					! ((nbpcrsa_t&t._0>0 ! enceinte_prrsap3_t&t.=1) & (forter_prrsa in ('1','3','9') ! forter_cprrsa in ('1','3','9')))); 

	/*RSA_MAJI : RSA avec majoration pour isolement (ex-API)*/
	RSA_MAJI_t&t._0=(enceinte_prrsap3_t&t.=1)*(cprrsa=0);

	/*Min_RSA_Tt_0*/
	Min_RSA_t&t._0=((cprrsa=1)*(1+0.5)
					+(cprrsa=0)*(rsa_maji_t&t._0=0)
					+(cprrsa=0)*(rsa_maji_t&t._0=1)*&taux_rsa_maji_pr.) 
					*&&basersa&asuiv4.*3;
                    
	
	/*FL_RSA_Tt_0 : Forfait Logement au trimestre t*/
	nbpers_fl_t&t._0=prrsa+cprrsa; 	/*Nombre de personnes prises en compte pour le FL*/
	FL_rsa_t&t._0=(
						(famp=1)*(((logt in('1','3','4','5'))*min(AL,&FL_1p.*(nbpers_fl_t&t._0=1)+&FL_2p.*(nbpers_fl_t&t._0=2)))
								+((logt in('2','6'))*(&FL_1p.*(nbpers_fl_t&t._0=1)+&FL_2p.*(nbpers_fl_t&t._0=2))))
						+(famp=0)*(&FL_1p.*(nbpers_fl_t&t._0=1)+&FL_2p.*(nbpers_fl_t&t._0=2))
						)/4;	

	/*BR_RSA_Tt_0 : Base ressources du RSA au trimestre t*/
	BR_RSA_t&t._0 = max(0,sum(revactp&asuiv4._t&t._prrsa,revactp&asuiv4._t&t._cprrsa))
				+ max(0,sum(salaire_etr&asuiv4._t&t._prrsa,salaire_etr&asuiv4._t&t._cprrsa))
				+ max(0,sum(zchopi&asuiv4._t&t._prrsa,zchopi&asuiv4._t&t._cprrsa)) 
				+ max(0,sum(zrstpi&asuiv4._t&t._prrsa,zrstpi&asuiv4._t&t._cprrsa))
				+ max(0,sum(produitfin&asuiv4._prrsa,produitfin&asuiv4._cprrsa))/4
				+ max(0,sum(zalri&asuiv4._prrsa,zalri&asuiv4._cprrsa,zrtoi&asuiv4._prrsa,zrtoi&asuiv4._cprrsa))/4
				+ max(0,sum(revpatm&asuiv4._prrsa,revpatm&asuiv4._cprrsa))/4
				+ afssmaj_t&t. + cf_base_t&t. + asf_sans_revalo_t&t. 
				+ clca_t&t. + ab_paje_rsa_t&t.*(rsa_maji_t&t._0=0) + ab_paje_rsa_maj_t&t.*(rsa_maji_t&t._0=1)
				+ minvi/4 + AAH/4 
				+ FL_rsa_t&t._0;

	/*RSA_tt._0 : Montant de RSA au trimestre t*/
	RSA_t&t._0 = max(0, sum(Min_RSA_t&t._0,-BR_RSA_t&t._0)) ; 


/**************************************************************************************************************************************************************/
/*		b. Prime d'activité																										 		                      */
/**************************************************************************************************************************************************************/

	/*NBPCRSA_Tt_0*/
	nbpcpa_t&t._0=0;

	/*Bonus pour la prime d'activité*/
	bonus_t&t._0 = max(0, sum(bonus_t&t._prrsa, bonus_t&t._cprrsa) ) ; 

	/*PA_MAJI : prime avec majoration pour isolement (ex-API)*/
	PA_MAJI_t&t._0=(enceinte_prrsap3_t&t.=1)*(cprrsa=0);

	/*Min_PA_Tt_0 : R0 de la prime d'activité au trimestre t*/
	Min_PA_t&t._0=((cprrsa=1)*(1+0.5)
					+(cprrsa=0)*(pa_maji_t&t._0=0)
					+(cprrsa=0)*(pa_maji_t&t._0=1)*&taux_rsa_maji_pr.) 
					*&&basepa&asuiv4.*3;
                    
	/*REVACT_PA_Tt_0 : revenus d'activité pour le calcul de la prime*/
	REVACT_PA_t&t._0=max(0,sum(revactp&asuiv4._t&t._prrsa,revactp&asuiv4._t&t._cprrsa,
						salaire_etr&asuiv4._t&t._prrsa,salaire_etr&asuiv4._t&t._cprrsa));

	/*FL_PA_Tt_0 : Forfait logement au trimestre t*/
	nbpers_fl_pa_t&t._0=prrsa+cprrsa; 
	FL_pa_t&t._0=(
				 (famp=1)*(((logt in('1','3','4','5'))*min(AL,&FL_pa_1p.*(nbpers_fl_pa_t&t._0=1)+&FL_pa_2p.*(nbpers_fl_pa_t&t._0=2)))
						  +((logt in('2','6'))*(&FL_pa_1p.*(nbpers_fl_pa_t&t._0=1)+&FL_pa_2p.*(nbpers_fl_pa_t&t._0=2))))
				 +(famp=0)*(&FL_pa_1p.*(nbpers_fl_pa_t&t._0=1)+&FL_pa_2p.*(nbpers_fl_pa_t&t._0=2))
				 )/4;	


	/*BR_PA_Tt_0 : Base ressources de la prime au trimestre t*/
	BR_PA_t&t._0= max(0,sum(revactp&asuiv4._t&t._prrsa,revactp&asuiv4._t&t._cprrsa))
				+ max(0,sum(salaire_etr&asuiv4._t&t._prrsa,salaire_etr&asuiv4._t&t._cprrsa))
				+ max(0,sum(zchopi&asuiv4._t&t._prrsa,zchopi&asuiv4._t&t._cprrsa)) 
				+ max(0,sum(zrstpi&asuiv4._t&t._prrsa,zrstpi&asuiv4._t&t._cprrsa))
				+ max(0,sum(produitfin&asuiv4._prrsa,produitfin&asuiv4._cprrsa))/4
				+ max(0,sum(zalri&asuiv4._prrsa,zalri&asuiv4._cprrsa,zrtoi&asuiv4._prrsa,zrtoi&asuiv4._cprrsa))/4
				+ max(0,sum(revpatm&asuiv4._prrsa,revpatm&asuiv4._cprrsa))/4
				+ afssmaj_t&t. + cf_base_t&t. + asf_sans_revalo_t&t. 
				+ clca_t&t. + ab_paje_rsa_t&t.*(rsa_maji_t&t._0=0) + ab_paje_rsa_maj_t&t.*(rsa_maji_t&t._0=1)
				+ minvi/4 + AAH/4 
				+ FL_pa_t&t._0;


	/*cible_PA_Tt_0*/
	cible_PA_t&t._0= bonus_t&t._0 + Min_PA_t&t._0+(1-&tmipa.)*REVACT_PA_t&t._0;

	/*prime d'activité*/
	prime_t&t._0 = (elig_rsa_t&t._0=1)*max(0,cible_PA_t&t._0-max(BR_PA_t&t._0,Min_PA_t&t._0)) ;


/**************************************************************************************************************************************************************/
/*				2- Calcul de la prime des jeunes cohabitants 			 									 												  */
/**************************************************************************************************************************************************************/
	
	%do i=1 %to &nb_max_pc.;
    	if bonus_t&t._pcrsa&i.=. then bonus_t&t._pcrsa&i.=0; 
		REVACT_PA_cohab_t&t._&i.=0;
		BR_PA_cohab_t&t._&i. = 0 ;  
		cible_PA_cohab_t&t._&i. =0 ; 
		prime_cohab_t&t._&i. = 0; 
		elig_prime_cohab_t&t._&i.= ((agenq_pcrsa&i.>=18 & agenq_pcrsa&i.<25 & forter_pcrsa&i. in ('1','3','9')));
	 	if elig_prime_cohab_t&t._&i.=1 then do;
       		BR_PA_cohab_t&t._&i. = &FL_pa_1p./4
								+ max(0,revactp&asuiv4._t&t._pcrsa&i.) 
								+ max(0,zchopi&asuiv4._t&t._pcrsa&i.)
								+ max(0,produitfin&asuiv4._pcrsa&i.)/4
								+ max(0,zrstpi&asuiv4._t&t._pcrsa&i.)
								+ max(0,sum(zalri&asuiv4._pcrsa&i.,zrtoi&asuiv4._pcrsa&i.,revpatm&asuiv4._pcrsa&i.))/4 ;
       		REVACT_PA_cohab_t&t._&i.= max(0,sum(revactp&asuiv4._t&t._pcrsa&i.,salaire_etr&asuiv4._t&t._pcrsa&i.)) ;
       		cible_PA_cohab_t&t._&i. = &&basepa&asuiv4.*3 +(1-&tmipa.)*REVACT_PA_cohab_t&t._&i. + bonus_t&t._pcrsa&i.; 
       		prime_cohab_t&t._&i.=max(0,cible_PA_cohab_t&t._&i.-max(&&basepa&asuiv4.*3,BR_PA_cohab_t&t._&i.));
       		if prime_cohab_t&t._&i.<&seuil_pa.*3 then prime_cohab_t&t._&i.=0 ; /*Seuil de versement*/
	  	end;
    %end;


/**************************************************************************************************************************************************************/
/*				3- Rattachement des personnes à charge : optimisation	 																					  */
/**************************************************************************************************************************************************************/
		
	%do j=1 %to &nb_max_pc.; /*Boucle sur les rangs de naissance*/
		%do i=1 %to &nb_max_pc.; /*Boucle sur les personnes à charge*/
			if rgfoy_t&t._pcrsa&i.="&j." then do;

/**************************************************************************************************************************************************************/
/*		a. RSA																													 		                      */
/**************************************************************************************************************************************************************/
				
				/*Incrémentation du nombre de personnes à charge*/
				nbpcrsa_t&t._&j.=nbpcrsa_t&t._%eval(&j.-1)+1;

				if 0<=(colla*12+collm-(4-&t.)*3)-(naia_pcrsa&i.*12+naim_pcrsa&i.)<36 then nbpcrsa02_t&t._&j.=nbpcrsa02_t&t._%eval(&j.-1)+1;
				else nbpcrsa02_t&t._&j.=nbpcrsa02_t&t._%eval(&j.-1);

				/*ELIG_RSA_Tt : éligibilité au RSA au trimestre t -> dépend des personnes à charge pour le RSA jeune*/
				elig_rsa_t&t._&j.=((agenq_prrsa>=25 & forter_prrsa in('1','3','9')) 
					! (agenq_cprrsa>=25 & forter_cprrsa in('1','3','9') ) 
					! ((nbpcrsa_t&t._&j.>0 ! enceinte_prrsap3_t&t.=1) & (forter_prrsa in('1','3','9') ! forter_cprrsa in('1','3','9')))); 

				/*RSA_MAJI : RSA avec majoration pour isolement (ex-API)*/
				rsa_maji_t&t._&j.=(cprrsa=0)
				*(((dv_fip_prrsa=1 ! sep_eec_prrsa=1 ! api_eec_prrsa=1) & nbpcrsa_t&t._&j.>0) ! enceinte_prrsap3_t&t.=1 ! nbpcrsa02_t&t._&j.>0);

				/*Min_RSA_Tt*/
				Min_RSA_t&t._&j.=((cprrsa=1)*(1+0.5+0.3*(nbpcrsa_t&t._&j.>=1)+0.3*(nbpcrsa_t&t._&j.>=2)+0.4*max(0,nbpcrsa_t&t._&j.-2))
								+(cprrsa=0)*(rsa_maji_t&t._&j.=0)*(1+0.5*(nbpcrsa_t&t._&j.>=1)+0.3*(nbpcrsa_t&t._&j.>=2)+0.4*max(0,nbpcrsa_t&t._&j.-2))
								+(cprrsa=0)*(rsa_maji_t&t._&j.=1)*(1*&taux_rsa_maji_pr.+&taux_rsa_maji_pc.*nbpcrsa_t&t._&j.))
								*&&basersa&asuiv4.*3;

				/*FL RSA_Tt : Forfait logement au trimestre t*/
				nbpers_fl_t&t._&j.=prrsa+cprrsa+nbpcrsa_t&t._&j.; 
				FL_RSA_t&t._&j.=(
					(famp=1)*(((logt in('1','3','4','5'))*min(AL,&FL_1p.*(nbpers_FL_t&t._&j.=1)+&FL_2p.*(nbpers_FL_t&t._&j.=2)+&FL_3p.*(nbpers_FL_t&t._&j.>=3)))
							 +((logt in('2','6'))*(&FL_1p.*(nbpers_FL_t&t._&j.=1)+&FL_2p.*(nbpers_FL_t&t._&j.=2)+&FL_3p.*(nbpers_FL_t&t._&j.>=3))))
					+(famp=0)*(&FL_1p.*(nbpers_FL_t&t._&j.=1)+&FL_2p.*(nbpers_FL_t&t._&j.=2)+&FL_3p.*(nbpers_FL_t&t._&j.>=3))
					)/4;	

				/*BR_RSA*/
				BR_RSA_t&t._&j.=BR_RSA_t&t._%eval(&j.-1) - FL_rsa_t&t._%eval(&j.-1) 
								+ FL_RSA_t&t._&j.
								+ max(0,revactp&asuiv4._t&t._pcrsa&i.) 
								+ max(0,zchopi&asuiv4._t&t._pcrsa&i.)
								+ max(0,produitfin&asuiv4._pcrsa&i.)/4
								+ max(0,zrstpi&asuiv4._t&t._pcrsa&i.)
								+ max(0,sum(zalri&asuiv4._pcrsa&i.,zrtoi&asuiv4._pcrsa&i.,revpatm&asuiv4._pcrsa&i.))/4;
				/*RSA */
				RSA_t&t._&j. = max(0, sum(Min_RSA_t&t._&j.,-BR_RSA_t&t._&j.)) ; 

				

/**************************************************************************************************************************************************************/
/*		b. Prime d'activité																										 		                      */
/**************************************************************************************************************************************************************/
	
				/*Incrémentation du nombre de personnes à charge*/
				nbpcpa_t&t._&j.=nbpcpa_t&t._%eval(&j.-1)+1;	

				/*REVACT_PA*/
				REVACT_PA_t&t._&j.=REVACT_PA_t&t._%eval(&j.-1) + 
					max(0,sum(revactp&asuiv4._t&t._pcrsa&i.,salaire_etr&asuiv4._t&t._pcrsa&i.)) ; 

				/*PA_MAJI : prime avec majoration pour isolement (ex-API)*/
				pa_maji_t&t._&j.=(cprrsa=0)
				*(((dv_fip_prrsa=1 ! sep_eec_prrsa=1 ! api_eec_prrsa=1) & nbpcpa_t&t._&j.>0) ! enceinte_prrsap3_t&t.=1 ! nbpcrsa02_t&t._&j.>0);

				/*Min_PA_Tt_j : R0 de la prime*/
				Min_PA_t&t._&j.=((cprrsa=1)*(1+0.5+0.3*(nbpcpa_t&t._&j.>=1)+0.3*(nbpcpa_t&t._&j.>=2)+0.4*max(0,nbpcpa_t&t._&j.-2))
								+(cprrsa=0)*(pa_maji_t&t._&j.=0)*(1+0.5*(nbpcpa_t&t._&j.>=1)+0.3*(nbpcpa_t&t._&j.>=2)+0.4*max(0,nbpcpa_t&t._&j.-2))
								+(cprrsa=0)*(pa_maji_t&t._&j.=1)*(1*&taux_rsa_maji_pr.+&taux_rsa_maji_pc.*nbpcpa_t&t._&j.))
								*&&basepa&asuiv4.*3 ; 

				/*FL_PA_Tt : Forfait logement au trimestre t*/
				nbpers_fl_pa_t&t._&j.=prrsa+cprrsa+nbpcpa_t&t._&j.; 
				FL_PA_t&t._&j.=(
					(famp=1)*(((logt in('1','3','4','5'))*min(AL,&FL_pa_1p.*(nbpers_FL_pa_t&t._&j.=1)+&FL_pa_2p.*(nbpers_FL_pa_t&t._&j.=2)+&FL_pa_3p.*(nbpers_FL_pa_t&t._&j.>=3)))
							 +((logt in('2','6'))*(&FL_pa_1p.*(nbpers_FL_pa_t&t._&j.=1)+&FL_pa_2p.*(nbpers_FL_pa_t&t._&j.=2)+&FL_pa_3p.*(nbpers_FL_pa_t&t._&j.>=3))))
					+(famp=0)*(&FL_pa_1p.*(nbpers_FL_pa_t&t._&j.=1)+&FL_pa_2p.*(nbpers_FL_pa_t&t._&j.=2)+&FL_pa_3p.*(nbpers_FL_pa_t&t._&j.>=3))
					)/4;

				/*BR_PA*/
				BR_PA_t&t._&j.=BR_PA_t&t._%eval(&j.-1) - FL_PA_t&t._%eval(&j.-1) 
								+ FL_PA_t&t._&j.
								+ max(0,revactp&asuiv4._t&t._pcrsa&i.) 
								+ max(0,zchopi&asuiv4._t&t._pcrsa&i.)
								+ max(0,produitfin&asuiv4._pcrsa&i.)/4
								+ max(0,zrstpi&asuiv4._t&t._pcrsa&i.)
								+ max(0,sum(zalri&asuiv4._pcrsa&i.,zrtoi&asuiv4._pcrsa&i.,revpatm&asuiv4._pcrsa&i.))/4;

				/*Cible_PA*/
				bonus_t&t._&j. = sum(bonus_t&t._%eval(&j.-1),bonus_t&t._pcrsa&i.); 
				cible_PA_t&t._&j.= bonus_t&t._&j.+ Min_PA_t&t._&j.+(1-&tmipa.)*REVACT_PA_t&t._&j.;

				/*Prime d'activité*/
				prime_t&t._&j. = max(0,cible_PA_t&t._&j.-max(BR_PA_t&t._&j.,Min_PA_t&t._&j.)) ; 



				/*Décision de rattachement pour le RSA*/
				if sum(Min_RSA_t&t._&j.,-BR_RSA_t&t._&j.)>= sum(Min_RSA_t&t._%eval(&j.-1),-BR_RSA_t&t._%eval(&j.-1)) then do; /*est rattaché*/
					pcrsa_t&t._&i.=1;
				end;
				else do; /*n'est pas rattaché*/
					pcrsa_t&t._&i.=0;
					nbpcrsa_t&t._&j.=nbpcrsa_t&t._%eval(&j.-1);					
					Min_RSA_t&t._&j.=Min_RSA_t&t._%eval(&j.-1);
					nbpers_fl_t&t._&j.=nbpers_fl_t&t._%eval(&j.-1);
					FL_RSA_t&t._&j.=FL_RSA_t&t._%eval(&j.-1);
					BR_RSA_t&t._&j.=BR_RSA_t&t._%eval(&j.-1);
					RSA_t&t._&j.=RSA_t&t._%eval(&j.-1);
					rsa_maji_t&t._&j.=rsa_maji_t&t._%eval(&j.-1);
				 end ;

				/*Décision de rattachement pour la prime*/
				if  (prime_cohab_t&t._&i.>0 & prime_t&t._&j. >= sum(prime_t&t._%eval(&j.-1),prime_cohab_t&t._&i.)) or 
					(prime_cohab_t&t._&i.=0 & sum(cible_PA_t&t._&j.,-max(BR_PA_t&t._&j.,Min_PA_t&t._&j.)) >= 
						sum(cible_PA_t&t._%eval(&j.-1),-max(BR_PA_t&t._%eval(&j.-1),Min_PA_t&t._%eval(&j.-1)))) then do; /*est rattaché*/	
					pcpa_t&t._&i.=1;
					prime_cohab_t&t._&i. =0 ; 
				end;

				else do; /*n'est pas rattaché*/
					pcpa_t&t._&i.=0;
					nbpcpa_t&t._&j.=nbpcpa_t&t._%eval(&j.-1);
					bonus_t&t._&j. = bonus_t&t._%eval(&j.-1); 
					Min_PA_t&t._&j.=Min_PA_t&t._%eval(&j.-1);
					nbpers_fl_pa_t&t._&j.=nbpers_fl_pa_t&t._%eval(&j.-1);
					FL_PA_t&t._&j.=FL_PA_t&t._%eval(&j.-1);
					BR_PA_t&t._&j.=BR_PA_t&t._%eval(&j.-1);
					REVACT_PA_t&t._&j.=REVACT_PA_t&t._%eval(&j.-1);
					cible_PA_t&t._&j.=cible_PA_t&t._%eval(&j.-1);
					prime_t&t._&j. = prime_t&t._%eval(&j.-1); 
					pa_maji_t&t._&j.=pa_maji_t&t._%eval(&j.-1);
				end;
			end;
		%end;
	%end;

	/*Détermination du RSA qui maximise : RSA calculé pour la dernière personne à charge*/
	compteur=0;			
	%do i=0 %to &nb_max_pc.;
		%let j=%eval(&nb_max_pc.-&i.);
		if compteur=0 & elig_rsa_t&t._&j.=1 then do;	
			compteur=1;									
			elig_rsa_t&t.=elig_rsa_t&t._&j.;
			nbpcrsa_t&t.=nbpcrsa_t&t._&j.;
			Min_RSA_t&t.=Min_RSA_t&t._&j.;
			FL_RSA_t&t.=FL_RSA_t&t._&j.;
			BR_RSA_t&t.=BR_RSA_t&t._&j.;
			rsa_maji_t&t.=rsa_maji_t&t._&j.;

			nbpcpa_t&t.=nbpcpa_t&t._&j.;
			bonus_t&t. = bonus_t&t._&j.; 
			Min_PA_t&t. =Min_PA_t&t._&j. ;
			FL_PA_t&t.=FL_PA_t&t._&j.;
			BR_PA_t&t.=BR_PA_t&t._&j.;
			REVACT_PA_t&t.=REVACT_PA_t&t._&j.;
			cible_PA_t&t.=cible_PA_t&t._&j.;
			pa_maji_t&t.=pa_maji_t&t._&j.;

		end;
	%end;
	if compteur=0 then do; /*personnes à charge jamais rattachées*/	
			elig_rsa_t&t.=elig_rsa_t&t._0;
			nbpcrsa_t&t.=nbpcrsa_t&t._0;
			Min_RSA_t&t.=Min_RSA_t&t._0;
			FL_RSA_t&t.=FL_RSA_t&t._0;
			BR_RSA_t&t.=BR_RSA_t&t._0;
			rsa_maji_t&t.=rsa_maji_t&t._0;

			nbpcpa_t&t.=nbpcpa_t&t._0;
			bonus_t&t. = bonus_t&t._0;
			Min_PA_t&t. =Min_PA_t&t._0 ;
			FL_PA_t&t.=FL_PA_t&t._0;
			BR_PA_t&t.=BR_PA_t&t._0;
			REVACT_PA_t&t.=REVACT_PA_t&t._0;
			cible_PA_t&t.=cible_PA_t&t._0;
			pa_maji_t&t.=pa_maji_t&t._0;

	end;


/**************************************************************************************************************************************************************/
/*				4- Calcul de la prime des jeunes décohabitants et agrégation  																				  */
/**************************************************************************************************************************************************************/

	/*On autorise les personnes de moins de 25 ans décohabitant à devenir éligibles à la prime*/
	elig_prime_decohab_t&t.= (elig_rsa_t&t.=0)*((agenq_prrsa>=18 & forter_prrsa in ('1','3','9')) ! (agenq_cprrsa>=18 & forter_cprrsa in('1','3','9'))); 

	/*Calcul PA RSA*/
	prime_t&t. =(elig_rsa_t&t.=1 or elig_prime_decohab_t&t.=1)*max(0,cible_PA_t&t.-max(BR_PA_t&t.,Min_PA_t&t.));
	RSA_t&t.=(elig_rsa_t&t.=1)*max(Min_RSA_t&t.-BR_RSA_t&t.,0);

	/*Seuil de non versement*/
	if RSA_t&t.<&seuil_rsa.*3 then RSA_t&t.=0;
	if prime_t&t.<&seuil_pa.*3 then prime_t&t.=0; 

	/*Assujettissement de la prime d'activité à la CRDS*/	
	%let n=&nb_max_pc. ; 
	prime_cohab_t&t.= max(0, sum(of prime_cohab_t&t._1-prime_cohab_t&t._&n.))*(1-&&tx_crds&asuiv4.);
	prime_decohab_t&t.=prime_t&t.*(1-&&tx_crds&asuiv4.);
	prime_t&t. = sum(prime_decohab_t&t., prime_cohab_t&t.); 

	/*On ajoute la variable de cumul des deux (pour le non-recours)*/
	RSA_prime_cumul_t&t. = sum(RSA_t&t., prime_t&t.); 
	elig_cumul_t&t. = (RSA_prime_cumul_t&t.>0); 
	elig_RSA_seul_t&t. = (RSA_t&t.>0 & prime_t&t.<=0); 
	elig_prime_seul_t&t. = (RSA_t&t.<=0 & prime_t&t.>0); 

	%do i=1 %to &nb_max_pc.;
	drop rgfoy_t&t._pcrsa&i.;
	%end;
%end; /*fin de la boucle sur les trimestres*/

/*Agrégats des valeurs trimestrielles*/
RSA=max(0,sum(of RSA_t1-RSA_t4));

prime=max(0,sum(of prime_t1-prime_t4));
prime_cohab=max(0, sum(of prime_cohab_t1-prime_cohab_t4)); 
prime_decohab=max(0, sum(of prime_decohab_t1-prime_decohab_t4)); 

RSA_prime_cumul = sum(RSA , prime); 


%mend;
%rsa;

run;


/**************************************************************************************************************************************************************/
/**************************************************************************************************************************************************************/
/*                 										III. Ajout des informations du foyer RSA à la table individuelle                                  	  */
/**************************************************************************************************************************************************************/
/**************************************************************************************************************************************************************/ 


/**************************************************************************************************************************************************************/
/*				1- Personnes à charge (effectifs) d'un foyer RSA 										  													  */
/**************************************************************************************************************************************************************/

/*Récupération de l'identifiant du ménage (NOI)*/
%let n=&nb_max_pc. ; 
proc transpose data=scenario.rsa  out=noi  (rename=(col1=noi) drop= COL2 COL3);
	by ident&acour. numfoyrsa;
	var noi_pcrsa1-noi_pcrsa&n.;
run;

data noi(drop=_NAME_); set noi (where=(noi ne ' '));rgpc=substr(_NAME_,10,1); run;

/*Récupération des personnes à charge*/
proc transpose data=scenario.rsa  out=pcrsa  (rename=(col1=pcrsa_eff) drop=COL2 COL3);
	by ident&acour. numfoyrsa;
	var pcrsa_t4_1-pcrsa_t4_&n.;
run;

data pcrsa (drop=_NAME_); set pcrsa (where=(pcrsa_eff ne .)) ; rgpc=substr(_NAME_,10,1); run;

/*Mise en commun*/
proc sort data=noi; by ident&acour. numfoyrsa rgpc; run;
proc sort data=pcrsa; by ident&acour. numfoyrsa rgpc; run;

data ind_pcrsa (drop = rgpc numfoyrsa); merge noi pcrsa; by ident&acour. numfoyrsa rgpc; run;


proc sort data=scenario.indiv_prest; by ident&acour. noi; run;
proc sort data=fam_prest25 ; by ident&acour. noi; run;
proc sort data=ind_pcrsa; by ident&acour. noi; run;

data scenario.indiv_prest (rename=(pcrsa25=pcrsa)); 
merge scenario.indiv_prest(in=a) fam_prest25 (keep = ident&acour. noi pcrsa25 numfoyrsa) ind_pcrsa; 
by ident&acour. noi; if a;
if pcrsa_eff=. then pcrsa_eff=0;
run;


/**************************************************************************************************************************************************************/
/*				2- Montant du RSA du foyer auquel appartient l'individu 										  											  */
/**************************************************************************************************************************************************************/

proc sort data=scenario.indiv_prest; by ident&acour. numfoyrsa; run;
proc sort data=scenario.rsa out=rsa (keep = ident&acour. numfoyrsa noi_prrsa noi_cprrsa elig_rsa_t4 rsa_pr prime_pr  rsa_pr_t: prime_pr_t: ) ; 
by ident&acour. numfoyrsa; 
run;

data scenario.indiv_prest;  merge scenario.indiv_prest(in=a) rsa;  by ident&acour. numfoyrsa;  if a; run;

proc datasets library=work; delete rsa; run; quit;


/**************************************************************************************************************************************************************/
/*				3- Détermination de l'éligibilité à la CMU-c et à l'ACS 										  											  */
/**************************************************************************************************************************************************************/

data scenario.rsa; 
set scenario.rsa;
%macro pcrsa;
%do i=1 %to 9;
	+ (noi_pcrsa&i. ne "")
%end; 
%mend;
nbpers=(noi_prrsa ne"") + (noi_cprrsa ne"") %pcrsa; /*on calcule le nombre de personnes*/

/*Pour le calcul des unités de consommation : calcul de l'âge des enfants*/
%macro agepcrsa;
%do i=1 %to 9;
	age&i.=2009-naia_pcrsa&i.; 
	uc_grand&i.=sum(age&i.>=14);
	uc_petit&i.=sum(age&i.<14 & age&i. ne""); 
%end; 
%mend;
%agepcrsa

nbuc_grand=sum(uc_grand1,uc_grand2,uc_grand3,uc_grand4,uc_grand5,uc_grand6,uc_grand7,uc_grand8,uc_grand9);
nbuc_petit=sum(uc_petit1,uc_petit2,uc_petit3,uc_petit4,uc_petit5,uc_petit6,uc_petit7,uc_petit8,uc_petit9);
nbuc_cmuc=(nbuc_grand*0.5) + (nbuc_petit*0.3) + ((noi_cprrsa ne"")*0.5) + 1;

%macro rev;
revact_pc=0;
salaire_etr&asuiv3._pc=0;
ZCHOpI&asuiv3._pc=0;
zrstpi&asuiv3._pc=0;
produitfin&asuiv3._pc=0;
zalri&asuiv3._pc=0;

%do i=1 %to 9 ; 

	revactp&asuiv3._pcrsa&i.=sum(revactp&asuiv3._t1_pcrsa&i.,revactp&asuiv3._t2_pcrsa&i.,revactp&asuiv3._t3_pcrsa&i.,revactp&asuiv3._t4_pcrsa&i.); 
	revact_pc=sum(revact_pc,revactp&asuiv3._pcrsa&i.);

	salaire_etr&asuiv3._pcrsa&i.=sum(salaire_etr&asuiv3._t1_pcrsa&i.,salaire_etr&asuiv3._t2_pcrsa&i.,salaire_etr&asuiv3._t3_pcrsa&i.,salaire_etr&asuiv3._t4_pcrsa&i.); 
	salaire_etr&asuiv3._pc=sum(salaire_etr&asuiv3._pc,salaire_etr&asuiv3._pcrsa&i.);

	ZCHOpI&asuiv3._pcrsa&i.=sum(ZCHOpI&asuiv3._t1_pcrsa&i.,ZCHOpI&asuiv3._t2_pcrsa&i.,ZCHOpI&asuiv3._t3_pcrsa&i.,ZCHOpI&asuiv3._t4_pcrsa&i.); 
	ZCHOpI&asuiv3._pc=sum(ZCHOpI&asuiv3._pc,ZCHOpI&asuiv3._pcrsa&i.);

	zrstpi&asuiv3._pcrsa&i.=sum(zrstpi&asuiv3._t1_pcrsa&i.,zrstpi&asuiv3._t2_pcrsa&i.,zrstpi&asuiv3._t3_pcrsa&i.,zrstpi&asuiv3._t4_pcrsa&i.); 
	zrstpi&asuiv3._pc=sum(zrstpi&asuiv3._pc,zrstpi&asuiv3._pcrsa&i.);


produitfin&asuiv3._pc= sum(of produitfin&asuiv3._pcrsa:);
zalri&asuiv3._pc=sum(of zalri&asuiv3._pcrsa:);
zrtoi&asuiv3._pc=sum(of zrtoi&asuiv3._pcrsa:);
revpatm&asuiv3._pc=sum(of revpatm&asuiv3._pcrsa:);
zalvm&asuiv3._pc=sum(of zalvm&asuiv3._pcrsa:);


%end; 
%mend;



/*Forfait logement CMUC annuel */
%let FL_1p_CMUC=%sysevalf(&&basersa&asuiv4.*0.12*12);  		/*1 personne*/
%let FL_2p_CMUC=%sysevalf(&&basersa&asuiv4.*0.14*1.5*12);  	/*2 personnes*/
%let FL_3p_CMUC=%sysevalf(&&basersa&asuiv4.*0.14*1.8*12); 	/*3 personnes ou plus*/


if ((logt in ("2","6"))|(numfoyrsa=0)) then 
FL_CMUC= &FL_1p_CMUC.*(nbpers=1)+&FL_2p_CMUC.*(nbpers=2)+&FL_3p_CMUC.*(nbpers>2);
else FL_CMUC= min(AL,&FL_1p.*(nbpers=1) + &FL_2P.*(nbpers=2) + &FL_3p.*(nbpers>=3));


/*Obligation de sommer les trimestres pour avoir en annuel (en N-1)*/
revactp&asuiv3._prrsa=sum(revactp&asuiv3._t1_prrsa,revactp&asuiv3._t2_prrsa,revactp&asuiv3._t3_prrsa,revactp&asuiv3._t4_prrsa);
revactp&asuiv3._cprrsa=sum(revactp&asuiv3._t1_cprrsa,revactp&asuiv3._t2_cprrsa,revactp&asuiv3._t3_cprrsa,revactp&asuiv3._t4_cprrsa);

salaire_etr&asuiv3._prrsa=sum(salaire_etr&asuiv3._t1_prrsa,salaire_etr&asuiv3._t2_prrsa,salaire_etr&asuiv3._t3_prrsa,salaire_etr&asuiv3._t4_prrsa);
salaire_etr&asuiv3._cprrsa=sum(salaire_etr&asuiv3._t1_cprrsa,salaire_etr&asuiv3._t2_cprrsa,salaire_etr&asuiv3._t3_cprrsa,salaire_etr&asuiv3._t4_cprrsa);

ZCHOpI&asuiv3._prrsa=sum (ZCHOpI&asuiv3._t1_prrsa,ZCHOpI&asuiv3._t2_prrsa,ZCHOpI&asuiv3._t3_prrsa,ZCHOpI&asuiv3._t4_prrsa); 
ZCHOpI&asuiv3._cprrsa=sum (ZCHOpI&asuiv3._t1_cprrsa,ZCHOpI&asuiv3._t2_cprrsa,ZCHOpI&asuiv3._t3_cprrsa,ZCHOpI&asuiv3._t4_cprrsa); 

zrstpi&asuiv3._prrsa=sum (zrstpi&asuiv3._t1_prrsa,zrstpi&asuiv3._t2_prrsa,zrstpi&asuiv3._t3_prrsa,zrstpi&asuiv3._t4_prrsa); 
zrstpi&asuiv3._cprrsa=sum (zrstpi&asuiv3._t1_cprrsa,zrstpi&asuiv3._t2_cprrsa,zrstpi&asuiv3._t3_cprrsa,zrstpi&asuiv3._t4_cprrsa); 

%rev;

/*Base ressource CMUC*/
BR_CMUC =  sum (max (0,sum(revactp&asuiv3._prrsa,revactp&asuiv3._cprrsa,revact_pc)),
			    max(0,sum(salaire_etr&asuiv3._prrsa,salaire_etr&asuiv3._cprrsa,salaire_etr&asuiv3._pc)),
				max(0,sum(zchopi&asuiv3._prrsa,zchopi&asuiv3._cprrsa,ZCHOpI&asuiv3._pc)), 
				max(0,sum(zrstpi&asuiv3._prrsa,zrstpi&asuiv3._cprrsa,zrstpi&asuiv3._pc)),
				max(0,sum(produitfin&asuiv3._prrsa,produitfin&asuiv3._cprrsa,produitfin&asuiv3._pc)),
				max(0,sum(zalri&asuiv3._prrsa,zalri&asuiv3._cprrsa,zalri&asuiv3._pc,zrtoi&asuiv3._prrsa,zrtoi&asuiv3._cprrsa,zrtoi&asuiv3._pc)),
				max(0,sum(revpatm&asuiv3._prrsa,revpatm&asuiv3._cprrsa,revpatm&asuiv3._pc)),
				af,cf_base,asf,minvi,AAH,clca,FL_CMUC);

/*Détermination de l'éligibilité en fonction du barème*/
elig_cmuc=0;
if (nbpers=1 & BR_CMUC<(&plaf_cmuc1.*12)) then elig_cmuc=1; 
if (nbpers=2 & BR_CMUC<(&plaf_cmuc2.*12)) then elig_cmuc=1;
if (nbpers=3 & BR_CMUC<(&plaf_cmuc3.*12)) then elig_cmuc=1; 
if (nbpers>=4 & BR_CMUC<((&plaf_cmuc4. +(nbpers-4)*&plaf_cmuc_supp.)*12)) then elig_cmuc=1;

/*Détermination de l'éligibilité en fonction du barème*/
elig_acs=0;
if elig_cmuc=0 then do; 
    if ((nbpers=1 & BR_CMUC<(&plaf_cmuc1.*12*1.35)) & elig_cmuc=0) then elig_acs=1; 
    if ((nbpers=2 & BR_CMUC<(&plaf_cmuc2.*12*1.35)) & elig_cmuc=0) then elig_acs=1;
    if ((nbpers=3 & BR_CMUC<(&plaf_cmuc3.*12*1.35)) & elig_cmuc=0) then elig_acs=1; 
    if ((nbpers>=4 & BR_CMUC<((&plaf_cmuc4. +(nbpers-4)*&plaf_cmuc_supp.)*12)*1.35) & elig_cmuc=0) then elig_acs=1;
end; 

/*Détermination du nombre de personnes éligibles*/ 
pers_cmuc=nbpers*elig_cmuc;
pers_acs=nbpers*elig_acs;


run;



/*Nettoyage de la work*/
proc datasets library=work;delete foyrsa fam_prest25 ;run;quit;


/*************************************************************************************************************************************************************
**************************************************************************************************************************************************************

Ce logiciel est régi par la licence CeCILL V2.1 soumise au droit français et respectant les principes de diffusion des logiciels libres. 

Vous pouvez utiliser, modifier et/ou redistribuer ce programme sous les conditions de la licence CeCILL V2.1. 

Le texte complet de la licence CeCILL V2.1 est dans le fichier `LICENSE`.

Les paramètres de la législation socio-fiscale figurant dans les programmes 6, 7a et 7b sont régis par la « Licence Ouverte / Open License » Version 2.0.
**************************************************************************************************************************************************************
*************************************************************************************************************************************************************/
