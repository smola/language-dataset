Released PSS*1*139 SEQ #126
Extracted from mail message
**KIDS**:PSS*1.0*139^

**INSTALL NAME**
PSS*1.0*139
"BLD",7709,0)
PSS*1.0*139^PHARMACY DATA MANAGEMENT^0^3080825^y
"BLD",7709,1,0)
^^26^26^3080825^
"BLD",7709,1,1,0)
This patch has enhancements which extend the capabilities of the Veterans 
"BLD",7709,1,2,0)
Health Information Systems and Technology Architecture (VistA) electronic 
"BLD",7709,1,3,0)
pharmacy (ePharmacy) billing system.
"BLD",7709,1,4,0)
 
"BLD",7709,1,5,0)
All sites should install this patches regardless of whether or not they 
"BLD",7709,1,6,0)
have been activated for ePharmacy by the Central Business Office (CBO).  
"BLD",7709,1,7,0)
After the installation of these patches, the activation status of the 
"BLD",7709,1,8,0)
site will not be impacted.  As before, activation of the ePharmacy 
"BLD",7709,1,9,0)
product will require instructions provided by the CBO.  Sites are not to 
"BLD",7709,1,10,0)
activate unless instructed specifically by the CBO.
"BLD",7709,1,11,0)
 
"BLD",7709,1,12,0)
This patch is being released along with PSO*7*303 and IB*2*405.  These 
"BLD",7709,1,13,0)
patches can be installed in any order but the full functionality contained
"BLD",7709,1,14,0)
within these patches will not be available until all 3 patches are
"BLD",7709,1,15,0)
installed.
"BLD",7709,1,16,0)
 
"BLD",7709,1,17,0)
This patch modifies the Pharmacy Data Management v1.0 application as 
"BLD",7709,1,18,0)
described below:
"BLD",7709,1,19,0)
 
"BLD",7709,1,20,0)
1. A new Drug Enforcement Administration (DEA) Special Handling code, N
"BLD",7709,1,21,0)
for Nutritional Supplement, was added as a selection to the DEA SPECIAL
"BLD",7709,1,22,0)
HDLG prompt for DRUG ENTER/EDIT [PSS DRUG ENTER/EDIT] option.  Once 
"BLD",7709,1,23,0)
Outpatient Pharmacy patch PSO*7*303 and Integrated Billing patch IB*2*405
"BLD",7709,1,24,0)
is installed, any drug defined with this new code will be treated in the
"BLD",7709,1,25,0)
same manner as supply items and investigational drugs.  The N DEA Special 
"BLD",7709,1,26,0)
Handling code must be defined manually.
"BLD",7709,4,0)
^9.64PA^^
"BLD",7709,6)
1^
"BLD",7709,6.3)
6
"BLD",7709,"KRN",0)
^9.67PA^8989.52^19
"BLD",7709,"KRN",.4,0)
.4
"BLD",7709,"KRN",.401,0)
.401
"BLD",7709,"KRN",.402,0)
.402
"BLD",7709,"KRN",.403,0)
.403
"BLD",7709,"KRN",.5,0)
.5
"BLD",7709,"KRN",.84,0)
.84
"BLD",7709,"KRN",3.6,0)
3.6
"BLD",7709,"KRN",3.8,0)
3.8
"BLD",7709,"KRN",9.2,0)
9.2
"BLD",7709,"KRN",9.8,0)
9.8
"BLD",7709,"KRN",9.8,"NM",0)
^9.68A^1^1
"BLD",7709,"KRN",9.8,"NM",1,0)
PSSDDUT2^^0^B81884236
"BLD",7709,"KRN",9.8,"NM","B","PSSDDUT2",1)

"BLD",7709,"KRN",19,0)
19
"BLD",7709,"KRN",19.1,0)
19.1
"BLD",7709,"KRN",101,0)
101
"BLD",7709,"KRN",409.61,0)
409.61
"BLD",7709,"KRN",771,0)
771
"BLD",7709,"KRN",870,0)
870
"BLD",7709,"KRN",8989.51,0)
8989.51
"BLD",7709,"KRN",8989.52,0)
8989.52
"BLD",7709,"KRN",8994,0)
8994
"BLD",7709,"KRN","B",.4,.4)

"BLD",7709,"KRN","B",.401,.401)

"BLD",7709,"KRN","B",.402,.402)

"BLD",7709,"KRN","B",.403,.403)

"BLD",7709,"KRN","B",.5,.5)

"BLD",7709,"KRN","B",.84,.84)

"BLD",7709,"KRN","B",3.6,3.6)

"BLD",7709,"KRN","B",3.8,3.8)

"BLD",7709,"KRN","B",9.2,9.2)

"BLD",7709,"KRN","B",9.8,9.8)

"BLD",7709,"KRN","B",19,19)

"BLD",7709,"KRN","B",19.1,19.1)

"BLD",7709,"KRN","B",101,101)

"BLD",7709,"KRN","B",409.61,409.61)

"BLD",7709,"KRN","B",771,771)

"BLD",7709,"KRN","B",870,870)

"BLD",7709,"KRN","B",8989.51,8989.51)

"BLD",7709,"KRN","B",8989.52,8989.52)

"BLD",7709,"KRN","B",8994,8994)

"BLD",7709,"QDEF")
^^^^NO^^^^NO^^NO
"BLD",7709,"QUES",0)
^9.62^^
"BLD",7709,"REQB",0)
^9.611^1^1
"BLD",7709,"REQB",1,0)
PSS*1.0*126^2
"BLD",7709,"REQB","B","PSS*1.0*126",1)

"MBREQ")
0
"PKG",488,-1)
1^1
"PKG",488,0)
PHARMACY DATA MANAGEMENT^PSS^Maintenance of Pharmacy files.
"PKG",488,20,0)
^9.402P^^
"PKG",488,22,0)
^9.49I^1^1
"PKG",488,22,1,0)
1.0^2970930^3000316^66481
"PKG",488,22,1,"PAH",1,0)
139^3080825^123457114
"PKG",488,22,1,"PAH",1,1,0)
^^26^26^3080825
"PKG",488,22,1,"PAH",1,1,1,0)
This patch has enhancements which extend the capabilities of the Veterans 
"PKG",488,22,1,"PAH",1,1,2,0)
Health Information Systems and Technology Architecture (VistA) electronic 
"PKG",488,22,1,"PAH",1,1,3,0)
pharmacy (ePharmacy) billing system.
"PKG",488,22,1,"PAH",1,1,4,0)
 
"PKG",488,22,1,"PAH",1,1,5,0)
All sites should install this patches regardless of whether or not they 
"PKG",488,22,1,"PAH",1,1,6,0)
have been activated for ePharmacy by the Central Business Office (CBO).  
"PKG",488,22,1,"PAH",1,1,7,0)
After the installation of these patches, the activation status of the 
"PKG",488,22,1,"PAH",1,1,8,0)
site will not be impacted.  As before, activation of the ePharmacy 
"PKG",488,22,1,"PAH",1,1,9,0)
product will require instructions provided by the CBO.  Sites are not to 
"PKG",488,22,1,"PAH",1,1,10,0)
activate unless instructed specifically by the CBO.
"PKG",488,22,1,"PAH",1,1,11,0)
 
"PKG",488,22,1,"PAH",1,1,12,0)
This patch is being released along with PSO*7*303 and IB*2*405.  These 
"PKG",488,22,1,"PAH",1,1,13,0)
patches can be installed in any order but the full functionality contained
"PKG",488,22,1,"PAH",1,1,14,0)
within these patches will not be available until all 3 patches are
"PKG",488,22,1,"PAH",1,1,15,0)
installed.
"PKG",488,22,1,"PAH",1,1,16,0)
 
"PKG",488,22,1,"PAH",1,1,17,0)
This patch modifies the Pharmacy Data Management v1.0 application as 
"PKG",488,22,1,"PAH",1,1,18,0)
described below:
"PKG",488,22,1,"PAH",1,1,19,0)
 
"PKG",488,22,1,"PAH",1,1,20,0)
1. A new Drug Enforcement Administration (DEA) Special Handling code, N
"PKG",488,22,1,"PAH",1,1,21,0)
for Nutritional Supplement, was added as a selection to the DEA SPECIAL
"PKG",488,22,1,"PAH",1,1,22,0)
HDLG prompt for DRUG ENTER/EDIT [PSS DRUG ENTER/EDIT] option.  Once 
"PKG",488,22,1,"PAH",1,1,23,0)
Outpatient Pharmacy patch PSO*7*303 and Integrated Billing patch IB*2*405
"PKG",488,22,1,"PAH",1,1,24,0)
is installed, any drug defined with this new code will be treated in the
"PKG",488,22,1,"PAH",1,1,25,0)
same manner as supply items and investigational drugs.  The N DEA Special 
"PKG",488,22,1,"PAH",1,1,26,0)
Handling code must be defined manually.
"QUES","XPF1",0)
Y
"QUES","XPF1","??")
^D REP^XPDH
"QUES","XPF1","A")
Shall I write over your |FLAG| File
"QUES","XPF1","B")
YES
"QUES","XPF1","M")
D XPF1^XPDIQ
"QUES","XPF2",0)
Y
"QUES","XPF2","??")
^D DTA^XPDH
"QUES","XPF2","A")
Want my data |FLAG| yours
"QUES","XPF2","B")
YES
"QUES","XPF2","M")
D XPF2^XPDIQ
"QUES","XPI1",0)
YO
"QUES","XPI1","??")
^D INHIBIT^XPDH
"QUES","XPI1","A")
Want KIDS to INHIBIT LOGONs during the install
"QUES","XPI1","B")
NO
"QUES","XPI1","M")
D XPI1^XPDIQ
"QUES","XPM1",0)
PO^VA(200,:EM
"QUES","XPM1","??")
^D MG^XPDH
"QUES","XPM1","A")
Enter the Coordinator for Mail Group '|FLAG|'
"QUES","XPM1","B")

"QUES","XPM1","M")
D XPM1^XPDIQ
"QUES","XPO1",0)
Y
"QUES","XPO1","??")
^D MENU^XPDH
"QUES","XPO1","A")
Want KIDS to Rebuild Menu Trees Upon Completion of Install
"QUES","XPO1","B")
NO
"QUES","XPO1","M")
D XPO1^XPDIQ
"QUES","XPZ1",0)
Y
"QUES","XPZ1","??")
^D OPT^XPDH
"QUES","XPZ1","A")
Want to DISABLE Scheduled Options, Menu Options, and Protocols
"QUES","XPZ1","B")
NO
"QUES","XPZ1","M")
D XPZ1^XPDIQ
"QUES","XPZ2",0)
Y
"QUES","XPZ2","??")
^D RTN^XPDH
"QUES","XPZ2","A")
Want to MOVE routines to other CPUs
"QUES","XPZ2","B")
NO
"QUES","XPZ2","M")
D XPZ2^XPDIQ
"RTN")
1
"RTN","PSSDDUT2")
0^1^B81884236^B81292420
"RTN","PSSDDUT2",1,0)
PSSDDUT2 ;BIR/LDT - Pharmacy Data Management DD Utility ; 8/21/07 8:43am
"RTN","PSSDDUT2",2,0)
 ;;1.0; PHARMACY DATA MANAGEMENT; **3,21,61,81,95,127,126,139**;9/30/97;Build 6
"RTN","PSSDDUT2",3,0)
 ;
"RTN","PSSDDUT2",4,0)
 ;Reference to ^DIC(42 supported by DBIA #10039
"RTN","PSSDDUT2",5,0)
 ;Reference to ^DD(59.723 supported by DBIA #2159
"RTN","PSSDDUT2",6,0)
 ;Reference to ^PSNDF(50.68 supported by DBIA 3735
"RTN","PSSDDUT2",7,0)
 ;
"RTN","PSSDDUT2",8,0)
DEA ;(Replaces ^PSODEA)
"RTN","PSSDDUT2",9,0)
 S PSSHLP(1)="THE SPECIAL HANDLING CODE IS A 2 TO 6 POSTION FIELD.  IF APPLICABLE,"
"RTN","PSSDDUT2",10,0)
 S PSSHLP(2)="A SCHEDULE CODE MUST APPEAR IN THE FIRST POSITION.  FOR EXAMPLE,"
"RTN","PSSDDUT2",11,0)
 S PSSHLP(3)="A SCHEDULE 3 NARCOTIC WILL BE CODED '3A', A SCHEDULE 3 NON-NARCOTIC WILL BE"
"RTN","PSSDDUT2",12,0)
 S PSSHLP(4)="CODED '3C' AND A SCHEDULE 2 DEPRESSANT WILL BE CODED '2L'."
"RTN","PSSDDUT2",13,0)
 S PSSHLP(5)="THE CODES ARE:"
"RTN","PSSDDUT2",14,0)
 D WRITE
"RTN","PSSDDUT2",15,0)
 F II=1:1 Q:$P($T(D+II),";",3)=""  S PSSHLP(II)=$P($T(D+II),";",3,99)
"RTN","PSSDDUT2",16,0)
 S PSSHLP(1,"F")="!!" D WRITE
"RTN","PSSDDUT2",17,0)
 D PKIND,WRITE
"RTN","PSSDDUT2",18,0)
D K II Q
"RTN","PSSDDUT2",19,0)
 ;;0          MANUFACTURED IN PHARMACY
"RTN","PSSDDUT2",20,0)
 ;;1          SCHEDULE 1 ITEM
"RTN","PSSDDUT2",21,0)
 ;;2          SCHEDULE 2 ITEM
"RTN","PSSDDUT2",22,0)
 ;;3          SCHEDULE 3 ITEM
"RTN","PSSDDUT2",23,0)
 ;;4          SCHEDULE 4 ITEM
"RTN","PSSDDUT2",24,0)
 ;;5          SCHEDULE 5 ITEM
"RTN","PSSDDUT2",25,0)
 ;;6          LEGEND ITEM
"RTN","PSSDDUT2",26,0)
 ;;9          OVER-THE-COUNTER
"RTN","PSSDDUT2",27,0)
 ;;L          DEPRESSANTS AND STIMULANTS
"RTN","PSSDDUT2",28,0)
 ;;A          NARCOTICS AND ALCOHOLS
"RTN","PSSDDUT2",29,0)
 ;;P          DATED DRUGS
"RTN","PSSDDUT2",30,0)
 ;;I          INVESTIGATIONAL DRUGS
"RTN","PSSDDUT2",31,0)
 ;;M          BULK COMPOUND ITEMS
"RTN","PSSDDUT2",32,0)
 ;;C          CONTROLLED SUBSTANCES - NON NARCOTIC
"RTN","PSSDDUT2",33,0)
 ;;R          RESTRICTED ITEMS
"RTN","PSSDDUT2",34,0)
 ;;S          SUPPLY ITEMS
"RTN","PSSDDUT2",35,0)
 ;;B          ALLOW REFILL (SCH. 3, 4, 5 ONLY)
"RTN","PSSDDUT2",36,0)
 ;;W          NOT RENEWABLE
"RTN","PSSDDUT2",37,0)
 ;;F          NON REFILLABLE
"RTN","PSSDDUT2",38,0)
 ;;E          ELECTRONICALLY BILLABLE
"RTN","PSSDDUT2",39,0)
 ;;N          NUTRITIONAL SUPPLEMENT
"RTN","PSSDDUT2",40,0)
 ;;
"RTN","PSSDDUT2",41,0)
DEATBL ; More Help regarding DEA Codes
"RTN","PSSDDUT2",42,0)
 K PSSHLP
"RTN","PSSDDUT2",43,0)
 F II=1:1 Q:$P($T(TBL+II),";",3)=""  S PSSHLP(II)=$P($T(TBL+II),";",3,99)
"RTN","PSSDDUT2",44,0)
 S PSSHLP(1,"F")="!!" D WRITE
"RTN","PSSDDUT2",45,0)
 ;
"RTN","PSSDDUT2",46,0)
TBL K II Q
"RTN","PSSDDUT2",47,0)
 ;;          DEA CODE TABLE
"RTN","PSSDDUT2",48,0)
 ;; CODE   ALLOW RENEWS ALLOW REFILLS
"RTN","PSSDDUT2",49,0)
 ;; 1            NO           NO
"RTN","PSSDDUT2",50,0)
 ;; 2            NO           NO
"RTN","PSSDDUT2",51,0)
 ;; 2A           NO           NO 
"RTN","PSSDDUT2",52,0)
 ;; 3            YES          YES
"RTN","PSSDDUT2",53,0)
 ;; 3A           YES          NO
"RTN","PSSDDUT2",54,0)
 ;; 3AB          YES          YES
"RTN","PSSDDUT2",55,0)
 ;; 4            YES          YES
"RTN","PSSDDUT2",56,0)
 ;; 4A           YES          NO
"RTN","PSSDDUT2",57,0)
 ;; 4AB          YES          YES
"RTN","PSSDDUT2",58,0)
 ;; 5            YES          YES
"RTN","PSSDDUT2",59,0)
 ;; 5A           YES          NO
"RTN","PSSDDUT2",60,0)
 ;; 5AB          YES          YES
"RTN","PSSDDUT2",61,0)
 ;; ADDING W TO A SCHED. 3,4,OR 5 CODE DISALLOWS RENEWS.
"RTN","PSSDDUT2",62,0)
 ;; ADDING F TO A SCHED. 3,4,OR 5 CODE DISALLOWS REFILLS
"RTN","PSSDDUT2",63,0)
 ;; IF A CODE IS NOT LISTED IN THE ABOVE TABLE
"RTN","PSSDDUT2",64,0)
 ;; IT HAS NO EFFECT ON RENEW OR REFILL
"RTN","PSSDDUT2",65,0)
SIG ;checks SIG for RXs (Replaces SIG^PSOHELP)
"RTN","PSSDDUT2",66,0)
 I $E(X)=" " D EN^DDIOL("Leading spaces are not allowed in the SIG! ","","$C(7),!") K X Q
"RTN","PSSDDUT2",67,0)
SIGONE S SIG="" Q:$L(X)<1  F Z0=1:1:$L(X," ") G:Z0="" EN S Z1=$P(X," ",Z0) D  G:'$D(X) EN
"RTN","PSSDDUT2",68,0)
 .I $L(Z1)>32 D EN^DDIOL("MAX OF 32 CHARACTERS ALLOWED BETWEEN SPACES.","","$C(7),!?5") K X Q
"RTN","PSSDDUT2",69,0)
 .D:$D(X)&($G(Z1)]"")  S SIG=SIG_" "_Z1
"RTN","PSSDDUT2",70,0)
 ..S Y=$O(^PS(51,"B",Z1,0)) Q:'Y!($P($G(^PS(51,+Y,0)),"^",4)>1)  S Z1=$P(^PS(51,Y,0),"^",2) Q:'$D(^(9))  S Y=$P(X," ",Z0-1),Y=$E(Y,$L(Y)) S:Y>1 Z1=^(9)
"RTN","PSSDDUT2",71,0)
EN K Z1,Z0 ;S:$G(POERR) PSOERR("SIG")="("_$E(SIG,2,999999999)_")"
"RTN","PSSDDUT2",72,0)
 Q
"RTN","PSSDDUT2",73,0)
 ;
"RTN","PSSDDUT2",74,0)
DRUGW ;(Replaces DRUGW^PSOUTLA)
"RTN","PSSDDUT2",75,0)
 F Z0=1:1 Q:$P(X,",",Z0,99)=""  S Z1=$P(X,",",Z0) D:$D(^PS(54,Z1,0)) EN^DDIOL($P(^(0),"^"),"","!,?35") I '$D(^(0)) D EN^DDIOL("NO SUCH WARNING LABEL","","?35") K X Q
"RTN","PSSDDUT2",76,0)
 Q
"RTN","PSSDDUT2",77,0)
 ;
"RTN","PSSDDUT2",78,0)
P ;(Replaces ^PSODSRC)
"RTN","PSSDDUT2",79,0)
 S PSSHLP(1)="A TWO OR THREE POSITION CODE IDENTIFIES THE SOURCE OF SUPPLY AND WHETHER"
"RTN","PSSDDUT2",80,0)
 S PSSHLP(2)="THE DRUG IS STOCKED BY THE STATION SUPPLY DIVISION.  THE FIRST"
"RTN","PSSDDUT2",81,0)
 S PSSHLP(3)="POSITION OF THE CODE IDENTIFIES SOURCE OF SUPPLY.  THE CODES ARE:"
"RTN","PSSDDUT2",82,0)
 D WRITE
"RTN","PSSDDUT2",83,0)
 F II=0:1:10 S PSSHLP(II+1)=$P($T(S+II+1),";",3),PSSHLP(II+1,"F")="!?10"
"RTN","PSSDDUT2",84,0)
 S PSSHLP(1,"F")="!!?10"
"RTN","PSSDDUT2",85,0)
 D WRITE
"RTN","PSSDDUT2",86,0)
 S PSSHLP(1)="THE SECOND POSITION OF THE CODE INDICATES WHETHER THE ITEM IS"
"RTN","PSSDDUT2",87,0)
 S PSSHLP(2)="OR IS NOT AVAILABLE FROM SUPPLY WAREHOUSE STOCK.  THE CODES ARE:"
"RTN","PSSDDUT2",88,0)
 S PSSHLP(3)="P          POSTED STOCK"
"RTN","PSSDDUT2",89,0)
 S PSSHLP(3,"F")="!!?10"
"RTN","PSSDDUT2",90,0)
 S PSSHLP(4)="U          UNPOSTED"
"RTN","PSSDDUT2",91,0)
 S PSSHLP(4,"F")="!?10"
"RTN","PSSDDUT2",92,0)
 S PSSHLP(5)="M          BULK COMPOUND"
"RTN","PSSDDUT2",93,0)
 S PSSHLP(5,"F")="!?10"
"RTN","PSSDDUT2",94,0)
 S PSSHLP(6)="*  USE CODE 0 ONLY WITH SECOND POSITION M."
"RTN","PSSDDUT2",95,0)
 D WRITE Q
"RTN","PSSDDUT2",96,0)
 ;
"RTN","PSSDDUT2",97,0)
S ;;DESCRIPTION MEANINGS
"RTN","PSSDDUT2",98,0)
 ;;0          BULK COMPOUND ITEMS *
"RTN","PSSDDUT2",99,0)
 ;;1          VA SERVICING SUPPLY DEPOT
"RTN","PSSDDUT2",100,0)
 ;;2          OPEN MARKET
"RTN","PSSDDUT2",101,0)
 ;;3          GSA STORES DEPOT
"RTN","PSSDDUT2",102,0)
 ;;4          VA DECENTRALIZED CONTRACTS
"RTN","PSSDDUT2",103,0)
 ;;5          FEDERAL PRISON INDUSTRIES, INC.
"RTN","PSSDDUT2",104,0)
 ;;6          FEDERAL SUPPLY SCHEDULES
"RTN","PSSDDUT2",105,0)
 ;;7          VA SUPPLY DEPOT, HINES
"RTN","PSSDDUT2",106,0)
 ;;8          VA SUPPLY DEPOT, SOMERVILLE
"RTN","PSSDDUT2",107,0)
 ;;9          APPROPRIATE MARKETING DIVISION
"RTN","PSSDDUT2",108,0)
 ;;10         VA SUPPLY DEPOT, BELL
"RTN","PSSDDUT2",109,0)
EDIT ;INPUT XFORM FOR DEA FIELD IN DRUG FILE (Replaces EDIT^PSODEA)
"RTN","PSSDDUT2",110,0)
 I X["F",X["B" D EN^DDIOL("Inappropriate F designation!","","$C(7),!") K X Q
"RTN","PSSDDUT2",111,0)
 ;;DEA CHANGE PSS*1*126
"RTN","PSSDDUT2",112,0)
 I X["B",(+X<3) D EN^DDIOL("The B designation is only valid for schedule 3, 4, 5 !","","$C(7),!") K X Q
"RTN","PSSDDUT2",113,0)
 I X["A"&(X["C"),+X=2!(+X=3) D EN^DDIOL("The A & C designation is not valid for schedule 2 or 3 narcotics !","","$C(7),!") K X Q
"RTN","PSSDDUT2",114,0)
 I $E(X)=1,X[2!(X[3)!(X[4)!(X[5) D EN^DDIOL("It contains other inappropriate schedule 2-5 narcotics!","","$C(7),!") K X Q
"RTN","PSSDDUT2",115,0)
 I $E(X)=2,X[1!(X[3)!(X[4)!(X[5) D EN^DDIOL("It contains other inappropriate schedule 1,3-5 narcotics!","","$C(7),!") K X Q
"RTN","PSSDDUT2",116,0)
 I $E(X)=3,X[1!(X[2)!(X[4)!(X[5) D EN^DDIOL("It contains other inappropriate schedule 1-2,4-5 narcotics!","","$C(7),!") K X Q
"RTN","PSSDDUT2",117,0)
 I $E(X)=4,X[1!(X[2)!(X[3)!(X[5) D EN^DDIOL("It contains other inappropriate schedule 1-3,5 narcotics!","","$C(7),!") K X Q
"RTN","PSSDDUT2",118,0)
 I $E(X)=5,X[1!(X[2)!(X[3)!(X[4) D EN^DDIOL("It contains other inappropriate schedule 1-4 narcotics!","","$C(7),!") K X Q
"RTN","PSSDDUT2",119,0)
 I $E(X)="E" D EN^DDIOL("Inappropriate E designation! Can only modify other codes.","","$C(7),!") K X Q
"RTN","PSSDDUT2",120,0)
 Q
"RTN","PSSDDUT2",121,0)
 ;
"RTN","PSSDDUT2",122,0)
WRITE ;Calls EN^DDIOL to write text
"RTN","PSSDDUT2",123,0)
 D EN^DDIOL(.PSSHLP) K PSSHLP Q
"RTN","PSSDDUT2",124,0)
 Q
"RTN","PSSDDUT2",125,0)
 ;
"RTN","PSSDDUT2",126,0)
PKIND I +$P($G(^PSDRUG(DA,"ND")),"^",3) S PSSK=$P(^("ND"),"^",3) D
"RTN","PSSDDUT2",127,0)
 .S PSSK=$$GET1^DIQ(50.68,PSSK,19,"I") I PSSK S PSSK=$$CSDEA^PSSDDUT2(PSSK) D
"RTN","PSSDDUT2",128,0)
 ..I $L(PSSK)=1,$P(^PSDRUG(DA,0),"^",3)[PSSK Q
"RTN","PSSDDUT2",129,0)
 ..I $P(^PSDRUG(DA,0),"^",3)[$E(PSSK),$P(^PSDRUG(DA,0),"^",3)[$E(PSSK,2) Q
"RTN","PSSDDUT2",130,0)
 ..W !!,"The CS Federal Schedule associated with this drug in the VA Product file"
"RTN","PSSDDUT2",131,0)
 ..W !,"represents a DEA, Special Handling code of "_PSSK
"RTN","PSSDDUT2",132,0)
 Q
"RTN","PSSDDUT2",133,0)
 ;
"RTN","PSSDDUT2",134,0)
CSDEA(CS) ;
"RTN","PSSDDUT2",135,0)
 Q:'CS ""
"RTN","PSSDDUT2",136,0)
 Q $S(CS?1(1"2n",1"3n"):+CS_"C",+CS=2!(+CS=3)&(CS'["C"):+CS_"A",1:CS)
"RTN","PSSDDUT2",137,0)
 ;
"RTN","PSSDDUT2",138,0)
CLOZ ;DEL node of DRUG file 50, fields 17.2, 17.3, 17.4
"RTN","PSSDDUT2",139,0)
 S PSSHLP(1)="To delete this field use the Unmark Clozapine Drug option in the"
"RTN","PSSDDUT2",140,0)
 S PSSHLP(2)="Clozapine Pharmacy Manager menu."
"RTN","PSSDDUT2",141,0)
 D WRITE
"RTN","PSSDDUT2",142,0)
 Q
"RTN","PSSDDUT2",143,0)
 ;
"RTN","PSSDDUT2",144,0)
NONF ;Non-Formulary Input Transform DRUG file 50, field 51
"RTN","PSSDDUT2",145,0)
 S PSSHLP(1)="This drug cannot be marked as a non-formulary item because it is"
"RTN","PSSDDUT2",146,0)
 S PSSHLP(2)="designated as a formulary alternative for the following drugs."
"RTN","PSSDDUT2",147,0)
 S PSSHLP(3)=" ",PSSHLP(1,"F")="!!"
"RTN","PSSDDUT2",148,0)
 D WRITE
"RTN","PSSDDUT2",149,0)
 F MM=0:0 S MM=$O(^PSDRUG("AFA",DA,MM)) Q:'MM  S SHEMP=$P(^PSDRUG(MM,0),"^") D EN^DDIOL(SHEMP,"","!?3")
"RTN","PSSDDUT2",150,0)
 S X=""
"RTN","PSSDDUT2",151,0)
 Q
"RTN","PSSDDUT2",152,0)
 ;
"RTN","PSSDDUT2",153,0)
ATC ;Executable help for field 212.2, DRUG file 50
"RTN","PSSDDUT2",154,0)
 S PSSHLP(1)="The mnemonic entered here must match the mnemonic entered into the"
"RTN","PSSDDUT2",155,0)
 S PSSHLP(2)="ATC for this drug EXACTLY, and cannot be numbers only."
"RTN","PSSDDUT2",156,0)
 D WRITE
"RTN","PSSDDUT2",157,0)
 Q
"RTN","PSSDDUT2",158,0)
 ;
"RTN","PSSDDUT2",159,0)
ADTM ;ADMINISTRATION SCHEDULE file 51.1, field 1 Executable Help
"RTN","PSSDDUT2",160,0)
 S PSSHLP(1)="ALL TIMES MUST BE THE SAME LENGTH (2 OR 4 CHARACTERS), MUST BE"
"RTN","PSSDDUT2",161,0)
 S PSSHLP(2)="SEPARATED BY DASHES ('-'), AND BE IN ASCENDING ORDER"
"RTN","PSSDDUT2",162,0)
 D WRITE
"RTN","PSSDDUT2",163,0)
 Q
"RTN","PSSDDUT2",164,0)
 ;
"RTN","PSSDDUT2",165,0)
LBLS ;PHARMACY SYSTEM file 59.7, field 61.2 Executable Help
"RTN","PSSDDUT2",166,0)
 S PSSHLP(1)="ANY NEW LABELS OLDER THAN THE NUMBER OF DAYS SPECIFIED HERE WILL"
"RTN","PSSDDUT2",167,0)
 S PSSHLP(2)="AUTOMATICALLY BE PURGED."
"RTN","PSSDDUT2",168,0)
 D WRITE
"RTN","PSSDDUT2",169,0)
 Q
"RTN","PSSDDUT2",170,0)
NFH I '$D(DA(1)) D EN^DDIOL(" (This non-formulary item is "_$P(^PSDRUG($S($D(DA(1)):DA(1),1:DA),0),"^")_".)")
"RTN","PSSDDUT2",171,0)
 Q
"RTN","PSSDDUT2",172,0)
STRTH S STR=" "_$P(X," ",2),PSSHLP(1)=STR,PSSHLP(1,"F")="" D WRITE K STR
"RTN","PSSDDUT2",173,0)
 Q
"RTN","PSSDDUT2",174,0)
PSYS1 D EN^DDIOL("(""From"" ward is "_$S('$D(^PS(59.7,D0,22,D1,0)):"UNKNOWN",'$D(^DIC(42,+^(0),0)):"UNKNOWN",$P(^(0),"^")]"":$P(^(0),"^"),1:"UNKNOWN")_")","","!?3")
"RTN","PSSDDUT2",175,0)
 Q
"RTN","PSSDDUT2",176,0)
PSYS2 ;PSS*1.0*95
"RTN","PSSDDUT2",177,0)
 D EN^DDIOL("(""From"" service is "_$S('$D(^PS(59.7,D0,23,D1,0)):"UNKNOWN",$P(^(0),"^")]"":$P($P(";"_$P(^DD(59.723,.01,0),"^",3),";"_$P(^PS(59.7,D0,23,D1,0),"^")_":",2),";"),1:"UNKNOWN")_")")
"RTN","PSSDDUT2",178,0)
 Q
"RTN","PSSDDUT2",179,0)
 ;
"RTN","PSSDDUT2",180,0)
NCINIT ;
"RTN","PSSDDUT2",181,0)
 K PSSNQM,PSSNQM2,PSSNQM3,PSSONDU,PSSONQM
"RTN","PSSDDUT2",182,0)
NCINIT1 ;
"RTN","PSSDDUT2",183,0)
 I $P($G(^PSDRUG(DA,"EPH")),"^",2)="" S $P(^PSDRUG(DA,"EPH"),"^",2)="EA",$P(^PSDRUG(DA,"EPH"),"^",3)=1 D
"RTN","PSSDDUT2",184,0)
 . S PSSHLP(1)="  Note:     Defaulting the NCPDP DISPENSE UNIT to EACH and the"
"RTN","PSSDDUT2",185,0)
 . S PSSHLP(2)="            NCPDP QUANTITY MULTIPLIER to 1 (one)." S PSSHLP(1,"F")="!!"
"RTN","PSSDDUT2",186,0)
 . D WRITE S PSSHLP(2,"F")="!" D WRITE
"RTN","PSSDDUT2",187,0)
 S PSSONDU=$P(^PSDRUG(DA,"EPH"),"^",2),PSSONQM=$P(^PSDRUG(DA,"EPH"),"^",3)
"RTN","PSSDDUT2",188,0)
 Q
"RTN","PSSDDUT2",189,0)
 ;
"RTN","PSSDDUT2",190,0)
NCPDPDU ;Drug file 50, field 82
"RTN","PSSDDUT2",191,0)
 S:X="" X="EA"
"RTN","PSSDDUT2",192,0)
 D NCINIT1:'$D(PSSONDU)
"RTN","PSSDDUT2",193,0)
 I $G(PSSONDU)'=X&($G(PSSONQM)'=1) D
"RTN","PSSDDUT2",194,0)
 . S PSSHLP(1)="Defaulting the NCPDP QUANTITY MULTIPLIER to 1 (one)." S PSSHLP(1,"F")="!!" D WRITE
"RTN","PSSDDUT2",195,0)
 . S $P(^PSDRUG(DA,"EPH"),"^",3)=1,PSSONDU=$P(^PSDRUG(DA,"EPH"),"^",2),PSSONQM=$P(^PSDRUG(DA,"EPH"),"^",3)
"RTN","PSSDDUT2",196,0)
 Q
"RTN","PSSDDUT2",197,0)
 ;
"RTN","PSSDDUT2",198,0)
NCPDPQM ;Drug file 50, field 83
"RTN","PSSDDUT2",199,0)
 N ZXX S PSSNQM=0,(PSSNQM2,PSSNQM3)=""
"RTN","PSSDDUT2",200,0)
 I $G(X)<.001 K X S PSSNQM3=1 Q
"RTN","PSSDDUT2",201,0)
 S:$G(X)="" X=1
"RTN","PSSDDUT2",202,0)
 I +$G(X)'=1 D NCPDPWRN D
"RTN","PSSDDUT2",203,0)
NCPDPQM1 . ;
"RTN","PSSDDUT2",204,0)
 . R !,"Ok to continue? (Y/N) ",ZXX:30 S ZXX=$TR(ZXX,"yn","YN")
"RTN","PSSDDUT2",205,0)
 . I ZXX="^" S X=1 W !!?5,"Warning:  Defaulting NCPDP QUANTITY MULTIPLIER to 1 (one).",!! Q
"RTN","PSSDDUT2",206,0)
 . I ZXX'="Y"&(ZXX'="N") W !,"Y or N must be entered." G NCPDPQM1
"RTN","PSSDDUT2",207,0)
 . I ZXX'="Y"&(ZXX'="y") S PSSNQM=1,PSSNQM2=X K X
"RTN","PSSDDUT2",208,0)
 Q
"RTN","PSSDDUT2",209,0)
 ;
"RTN","PSSDDUT2",210,0)
NCPDPWRN ;
"RTN","PSSDDUT2",211,0)
 S PSSHLP(2)="WARNING:    For most drug products, the value for this field should be 1 (one)."
"RTN","PSSDDUT2",212,0)
 S PSSHLP(3)="            Answering NO for the following prompt will display more information"
"RTN","PSSDDUT2",213,0)
 S PSSHLP(4)="            on how this field is used."
"RTN","PSSDDUT2",214,0)
 S PSSHLP(2,"F")="!!" D WRITE
"RTN","PSSDDUT2",215,0)
 S PSSHLP(5,"F")="!" D WRITE
"RTN","PSSDDUT2",216,0)
 Q
"RTN","PSSDDUT2",217,0)
 ;
"VER")
8.0^22.0
"BLD",7709,6)
^126
**END**
**END**
