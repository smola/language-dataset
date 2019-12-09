/* agqpr700.eq

   October 11, 2002

   SCIPS.com, Inc.

   quote worksheet for MSO Special Contractor
*/ 

DEFINE STRING I_NAME[50] = agqcontractor:business_name[1]

INCLUDE "RENAEQ1.INC"

DEFINE NUMBER L_ZIP_CODE[9] = agqcontractor:zipcode

INCLUDE "ZIP1.INC"
 
define signed ascii number l_amount = 0 ;

define string l_company_name = sfscompany:name[1]

define unsigned ascii number l_premium[6]=0 
                
define file agqmoney_alt = access agqmoney, set
agqmoney:app_no = agqcontractor:app_no, generic

define string l_money_limits = "$" + str(agqmoney_alt:money_limit_on) + "/" +
                               "$" + str(agqmoney_alt:money_limit_off)

define file alt_agqmaster = access agqmaster, set
agqmaster:app_no = agqcontractor:app_no, exact

define file alt_agqclasscode = access agqclasscode, set
agqclasscode:app_no = agqcontractor:app_no, 
agqclasscode:prem_no = agqcontractor:prem_no,
agqclasscode:build_no = agqcontractor:build_no, one to many, generic
                                     
define file alt_agqlocation = access agqlocation, set
agqlocation:app_no = agqcontractor:app_no,
agqlocation:prem_no = agqcontractor:prem_no,
agqlocation:build_no = agqcontractor:build_no, generic

define file alt_agqend = access agqend, set
agqend:app_no = agqcontractor:app_no, 
agqend:prem_no = agqcontractor:prem_no,
agqend:build_no = agqcontractor:build_no, generic

define file alt_agqend1 = access agqend, set
agqend:app_no = agqcontractor:app_no, 
agqend:prem_no  = 0,
agqend:build_no = 0, generic

define file alt_agqsupp = access agqsupp, set
agqsupp:app_no = agqcontractor:app_no, 
agqsupp:prem_no = agqcontractor:prem_no,
agqsupp:build_no = agqcontractor:build_no, generic

define file alt_sfsagent = access sfsagent, set
sfsagent:company_id = agqcontractor:company_id,
sfsagent:agent_no = agqcontractor:agent_no, generic
                   
define file alt_sfscomun = access sfscomun, set
sfscomun:company_id= agqcontractor:company_id,
sfscomun:commercial_underwriter= alt_sfsagent:commercial_underwriter, generic  

define file alt_sfspayortype = access sfspayortype, set
sfspayortype:company_id = agqcontractor:company_id,
sfspayortype:payor_code = alt_agqsupp:mortgage_type_1, generic

define file alt_sfsmorttype = access sfsmorttype, set
sfsmorttype:company_id = agqcontractor:company_id,
sfsmorttype:mortgage_code = alt_agqsupp:mortgage_type_2, generic
     
define file alt_sfsmort = access sfsmort, set
sfsmort:company_id = agqcontractor:company_id,
sfsmort:mortgage_id = alt_agqsupp:mortgage_id, generic

define file agqadditionalclass_alias = access agqadditionalclass,
                                          set agqadditionalclass:app_no = agqcontractor:app_no, one to many, generic

define string l_exposure_type[2] = agqadditionalclass_alias:exposure_type                                              

define file cpsliabexpbase_alias = access cpsliabexpbase, 
                                      set cpsliabexpbase:company_id       = agqcontractor:company_id,
                                          cpsliabexpbase:state            = agqcontractor:state,
                                          cpsliabexpbase:line_of_business = agqcontractor:line_of_business,
                                          cpsliabexpbase:exposure_base    = l_exposure_type, approximate

define string l_mortgage = if agqsupp:mortgage_id = "" then
                               trun(alt_agqsupp:name[1]) +  " " +
                               trun(alt_agqsupp:address[1]) +  " " +
                               trun(alt_agqsupp:city) + " " +
                               ", " + trun(alt_agqsupp:str_state) + " " +
                               trun(alt_agqsupp:str_zipcode)

define string l_mort = if agqsupp:mortgage_id <> "" then
                           trun(alt_sfsmort:name[1]) + " " +
                           trun(alt_sfsmort:address[1]) + " " +
                           trun(alt_sfsmort:city) + " " +
                           ", " + trun(alt_sfsmort:str_state) + " " +
                           trun(alt_sfsmort:str_zipcode)

define file alt_sfsstate = access sfsstate, set
                   sfsstate:company_id = agqcontractor:company_id ,
                   sfsstate:state = agqcontractor:state,
                   sfsstate:county= 000, generic

define file alt1_sfsstate = access sfsstate, set
                   sfsstate:company_id = agqcontractor:company_id,
                   sfsstate:state = agqcontractor:state,
                   sfsstate:county = agqcontractor:county, generic

/*define file alt_sfsliability = access sfsliability, set
                               sfsliability:company_id = agqcontractor:company_id,

                               sfsliability:state = agqcontractor:state,
                               sfsliability:line_of_business = agqcontractor:line_of_business,
                               sfsliability:liability_code = alt_agqmaster:liability_code, generic
*/
define file alt_sfsline = access sfsline, set
sfsline:company_id = agqcontractor:company_id,
sfsline:line_of_business = agqcontractor:line_of_business,
sfsline:lob_subline = "00", generic
    
define file alt_sfsfob = access sfsfob, set
sfsfob:company_id = agqcontractor:company_id,
sfsfob:form_of_business = alt_agqmaster:form_of_business, generic
    
define unsigned ascii number l_form[2] = agqmaster:form

define file alt_sfsform = access sfsform, set
sfsform:company_id = agqcontractor:company_id,
sfsform:state = agqcontractor:state,
sfsform:line_of_business = agqcontractor:line_of_business,
sfsform:policy_form = l_form, generic

define file alt_scsclasscode = access scsclasscode, set
scsclasscode:company_id = agqcontractor:company_id,
scsclasscode:state = agqcontractor:state,
scsclasscode:line_of_business= agqcontractor:line_of_business,
scsclasscode:class_code=  alt_agqclasscode:class_code, generic

define file alt_agqcontractor = access agqcontractor_alias, set
agqcontractor_alias:app_no = agqcontractor:app_no ,
agqcontractor_alias:prem_no = agqcontractor:prem_no,
agqcontractor_alias:build_no = agqcontractor:build_no, generic

unsigned ascii number 
l_liability_only_once = if count[agqcontractor:app_no] > 1 then
                                                0
                                              else
                                                agqmaster:total[1] - agqmaster:total[10] - agqmaster:total[9] 
-agqmaster:total[19]
                 
where agqcontractor:app_no = agqprint1:app_no
LIST
/NOPAGEHEADINGS
/NOBANNER
/NOTOTALS
--/NOBLANKLINES
/DOMAIN="agqcontractor"
/INLINE
/NOHEADIngs
/noduplicates
/pagelength=0


"LOCATION:   "/COLUMN=30
agqcontractor:PREM_NO/MASK="ZZZ9"/NEWLINE/column=42
"BUILDING:       "/COLUMN=30 
agqcontractor:BUILD_NO/MASK="ZZZ9"/NEWLINE=2/column=42/duplicates 

box/noblanklines/noheading/column=1
"WAIVE DEDUCTIBLE:"/column=1
switch(agqliablimits:waive_deductible)
  case 0  : "N"
  default : "Y"/column=25         

if agqliablimits:waive_deductible one of 0 then  
  {
    "DEDUCTIBLE:"/column=40
    agqliablimits:liab_deductible/newline/column=65
  }
else
  {
    "Waive Deductible Charge:"/column=40
    agqliablimits:waive_deductible_charge/newline  
  }
     
"INCREASE AGGREGATE:"/column=1
switch(agqliablimits:increased_aggregate)
  case 0  : "N"
  default : "Y"/column=25/duplicates/newline



"OCCURRENCE:"/COLUMN=1
AGQliablimits:OCCURRENCE/column=25/mask="ZZ,ZZZ,ZZZ"/newline/duplicates

"GENERAL AGGREGATE:"/COLUMN=1
AGQliablimits:GENERAL_AGGREGATE/column=25/mask="ZZ,ZZZ,ZZZ"/duplicates/newline

"AGGREGATE:"/COLUMN=1
AGQliablimits:AGGREGATE/column=25/mask="ZZ,ZZZ,ZZZ"/duplicates/newline

"FIRE LEGAL:"/COLUMN=1
AGQliablimits:FIRE_LEGAL/column=25/mask="ZZ,ZZZ,ZZZ"/duplicates/newline

"MEDICAL PAYMENTS:"/COLUMN=1
AGQliablimits:MEDICAL_PAYMENTS/column=25/mask="ZZ,ZZZ,ZZZ"/duplicates/newline

"PERSONAL INJURY:"/COLUMN=1
AGQliablimits:PERsonal_INJURY/column=25/mask="ZZ,ZZZ,ZZZ"/duplicates/newline
end box/newlines=2
 
box/noblanklines/noheadings/duplicates 
"CLASS CODE:"/column=1
alt_agqclasscode:CLASS_CODE/MASK="ZZZZZ"/COLUMN=13
uppercase(alt_scsclasscode:description[1,75])/newline 
/*If sfscompany:special_territory_rating = 1 or
   agqcontractor:state = 19 then
    {
    "OWNER:"/column=1
    alt_agqclasscode:no_owners/column=20 
    "FULL TIME:"/column=30
    alt_agqclasscode:no_workers/column=50
    "PART TIME:"/column=55
    alt_agqclasscode:no_helpers/column=65/newline
    }                                    
else
    {
    "NO WORKERS:"/column=1
    alt_agqclasscode:no_workers/column=20
    "NO HELPERS:"/column=30
    alt_agqclasscode:no_helpers/newline/column=50
    }*/
/*"PROPERTY RATE GRP:"/column=1
alt_agqclasscode:property_rate_group/column=20 
"LIABILITY PREM NO:"/column=30
alt_agqclasscode:liability_prem_no/column=50/newline*/
end box/newlines

followed by

if agqmaster:form < 4 then
box/noblanklines/noheadings/duplicates/column=1
"LOCATION:"/column=1
if alt_agqlocation:address <> "" then
    {
    trun(alt_agqlocation:address) + " " + trun(alt_agqlocation:CITY) + ", " +
    trun(alt_agqlocation:str_state) + " " + trun(alt_agqlocation:str_zipcode)/newline
    }
else
    {
    agqcontractor:str_zipcode/newline
    }
       
"BUSINESS DESCRIPTION:"/column=1
trun(agqcontractor:BUSINESS_DESC[1]) + " " + trun(agqcontractor:business_desc[2])/newline=2

"STATE:"/COLUMN=1
alt_sfsstate:description

"COUNTY:"/COlumn=40
ALT1_sfsstate:DESCRIPTION/newline

"FORM:"/column=1
agqmaster:form
uppercase(sfsform:description)/newline

"CONSTRUCTION: "/column=1
agqcontractor:CONSTRUCTION/MASK="ZZZ"/COLUMN=20

"PROTECTION:"/COLUMN=40
agqcontractor:PROTECTION/NEWLINE/column=60

"YR CONSTRUCTION:"/column=1
agqcontractor:year_construction/mask="ZZZ9"/column=20

"DEDUCTIBLE:"/column=40
agqcontractor:deductible/newline/align=agqcontractor:protection/column=60

"RISK TYPE:"/COLUMN=1
agqcontractor:rating_TYPE/align=agqcontractor:construction/column=20
uppercase(scstype:type_description)
                         
"WIND DEDUCTIBLE:"/column=40
agqcontractor:wind_deductible/newline=3/mask="ZZZZ9"/column=60
 
"BUILDING:"/COLUMN=1
AGQCONTRACTOR:BUILDING_LIMIT/column=25
agqcontractor:building_premium/column=35/NEWLINE/mask="ZZ,ZZZ.99-"

"PERSONAL PROPERTY:"/COLUMN=1
AGQCONTRACTOR:PROPERty_LIMIT/align=agqcontractor:building_limit
agqcontractor:property_premium/align=agqcontractor:building_premium/newline/mask="ZZ,ZZZ.99-"

"EXPANDED COVERAGE:"/column=1
agqcontractor:expanded_premium/align=agqcontractor:building_premium/newline/mask="ZZ,ZZZ.99-"

"PERSONAL PROP OF OTHERS:"/COLUMN=1
AGQCONTRACTOR:PERSONAL_LIMIT/align=agqcontractor:building_limit
agqcontractor:personal_premium/newline/align=agqcontractor:building_premium/mask="ZZ,ZZZ.99-"

"Expanded Personal Property of Other"/column=1 
AGQCONTRACTOR:EXPANDED_PREMIUM_PERSONAL_PROP/align=agqcontractor:building_premium/newline/mask="ZZ,ZZZ.99-"

"LOSS OF USE:"/column=1
AGQCONTRACTOR:LOSS_LIMIT/align=agqcontractor:building_limit
agqcontractor:loss_premium/align=agqcontractor:building_premium/mask="ZZ,ZZZ.99-"/newline
if sfscompany:company_id one of "DELOS" then
  {
    "LIABILITY:"/column=1
     l_liability_only_once-agqcontractor:fire_premium/align=agqcontractor:building_premium/mask="ZZ,ZZZ.99-"/newline/duplicates 
  } 
  
if l_liability_only_once <> 0 then
  {
"FIRE LEGAL:"/column=1
agqcontractor:fire_premium/align=agqcontractor:building_premium/mask="ZZ,ZZZ.99-"/newline=3/duplicates
  
"DELETE THEFT:"/column=1
switch(agqcontractor:excluding_theft)
  case 0  : "N"
  default : "Y"/column=15
"LIMIT THEFT: "/column=40
switch(agqcontractor:limit_theft)
  case 0  : "N"
  default : "Y"/column=60/newline 
"FIRE ALARM: "/column=1
switch(agqcontractor:fire_alarm)
  case 0  : "N"
  default : "Y"/column=15 
"BURGLAR ALARM: "/column=40
switch(agqcontractor:burglary_alarm)
  case 0  : "N"
  default : "Y"/column=60/newline=2

"ACCOUNTS RECEIVABLE:"/column=1
agqcontractor:AR_LIMIT/mask="$$$,$$$"/COLUMN=22
agqcontractor:AR_PREMIUM/mask="ZZ,ZZZ.99-"/COLUMN=32
                       
"OUTDOOR PROPERTY LIMIT:"/COLUMN=45
AGQCONTRACTOR:OUTDOOR_PROPERTY_LIMIT/column=65
AGQCONTRACTOR:OUTDOOR_PROPERTY_PREMIUM/NEWLINE/mask="ZZ,ZZZ.99-"/column=75

"INFLATION GUARD %:"/COLUMN=1
agqcontractor:INFLATION_GUARD[1]/ALIGN=AGQCONTRACTOR:AR_LIMIT
                                                             
"PERSONAL EFFECTS:"/COLUMN=45
AGQCONTRACTOR:PERSONAL_EFFECTS/ALIGN=AGQCONTRACTOR:OUTdoor_PROperty_LIMIT
AGQCONTRACTOR:PERsonal_EFFects_PREMIUM/ALIGN=AGQCONTRACTOR:OUTdoor_PROPERTY_PREMIUM/NEWLINE/mask="ZZ,ZZZ.99-"

"INFLATION GUARD %:"/COLUMN=1
agqcontractor:INFLATION_GUARD[2]/ALIGN=AGQCONTRACTOR:AR_LIMIT

"VALUABLE PAPERS:"/column=45
agqcontractor:VALUABLE_PAPERS/ALIGN=AGQCONTRACTOR:OUTdoor_PROPerty_LIMIT
agqcontractor:VALUABLE_PAPERS_PREMIUM/NEWLINE/mask="ZZ,ZZZ.99-"/ALIGN=AGQCONTRACTOR:OUTdoor_PROPERTY_PREMIUM 

"GLASS:"/COLUMN=1
AGQCONTRACTOR:GLASS_LIMIT/ALIGN=AGQCONTRACTOR:AR_LIMIT
AGQCONTRACTOR:GLASS_PREMIUM/ALIGN=AGQCONTRACTOR:AR_PREMIUM/mask="ZZ,ZZZ.99-" 

"LIMIT WATER BACKUP:"/column=45
switch(agqcontractor:LIMIT_water_bKUP)
  case 0  : "N"
  default : "Y"/newline/align=agqcontractor:outdoor_property_limit

"SIGNS ATTACHED:"/column=1
agqcontractor:SIGNS_ATTACHED_LIMIT/mask="$$$,$$$"/align=agqcontractor:ar_limit
agqcontractor:SIGNS_ATTACHED_PREM/mask="ZZ,ZZZ.99-"/align=agqcontractor:ar_premium 

"WATER BACKUP LIMIT:   "/column=45
if sfscompany:company_id one of "DELOS" then
  {
    agqcontractor:water_bKUP_limit/align=agqcontractor:outdoor_property_limit 
  }
else
  {
    if agqcontractor:water_bKUP_premium <> 0 then
      {
        agqcontractor:water_bKUP_limit/align=agqcontractor:outdoor_property_limit 
      }  
    else    
      {
         "      0"/align=agqcontractor:outdoor_property_limit
      }
  }

agqcontractor:water_bKUP_premium/mask="ZZ,ZZZ.99-"/newline/align=agqcontractor:outdoor_property_premium 

"SIGNS NOT ATTACHED:"/column=1
agqcontractor:SIGNS_not_attached_limit/mask="$$$,$$$"/align=agqcontractor:ar_limit
agqcontractor:SIGNS_not_PREMium/mask="ZZ,ZZZ.99-"/align=agqcontractor:ar_premium 
                                  
"ACV BUILDING:"/column=45
switch(agqcontractor:building_acv)
  case 0  : "N"
  default : "Y"/align=agqcontractor:outdoor_property_limit 
agqcontractor:building_acv_premium/align=agqcontractor:outdoor_property_premium/newline/mask="ZZ,ZZZ.99-"

"BUILDING CODE/LAW:"/column=1
agqcontractor:BUILDING_CODE_LAW/align=agqcontractor:ar_limit
agqcontractor:BUILDING_CODE_PREMIUM/mask="ZZ,ZZZ.99-"/align=agqcontractor:ar_premium

"ACV CONTENTS:"/column=45
switch(agqcontractor:contents_acv)
  case 0  : "N"
  default : "Y"/align=agqcontractor:outdoor_property_limit 
agqcontractor:contents_acv_premium/align=agqcontractor:outdoor_property_premium/newline/mask="ZZ,ZZZ.99-"

"DEBRIS REMOVAL:"/column=1
agqcontractor:debris_limit/align=agqcontractor:ar_limit
agqcontractor:debris_premium/align=agqcontractor:ar_premium/mask="ZZ,ZZZ.99-"

"COMPUTER:"/column=45
agqcontractor:computer_limit/align=agqcontractor:outdoor_property_limit 
agqcontractor:computer_premium/align=agqcontractor:outdoor_property_premium/newline/mask="ZZ,ZZZ.99-"   

"OFF PREMISES:"/column=1
agqcontractor:off_prem_limit/mask="$$$,$$$"/align=agqcontractor:ar_limit
agqcontractor:off_preM_premium/mask="ZZ,ZZZ.99-"/align=agqcontractor:ar_premium

"EMPLOYEE DISHONESTY:"/column=45
agqcontractor:employee_dishonesty/align=agqcontractor:outdoor_property_limit 
agqcontractor:employee_dis_premium/align=agqcontractor:outdoor_property_premium/newline/mask="ZZ,ZZZ.99-"

"MONEY/SECURITIES:"/column=1 
if l_money_limits <> "" then
    { 
    l_money_limits/column=18
    agqmoney_alt:MONEY_PREMIUM/mask="ZZ,ZZZ.99-"/align=agqcontractor:ar_premium
    }
else
    {
    "0"/column=18
    agqmoney_alt:money_premium/Mask="ZZ,ZZZ.99-"/align=agqcontractor:ar_premium
    }
                 
"EXTERIOR GLASS:"/column=45
agqcontractor:exterior_glass/mask="ZZZ,ZZ9"/align=agqcontractor:outdoor_property_limit
agqcontractor:EXTERIOR_GLASS_PREM/mask="ZZ,ZZZ.99-"/align=agqcontractor:outdoor_property_premium
}
end box
                      

else
if agqmaster:form = 4 then
box/noblanklines/noheadings/duplicates/column=1
"LOCATION:"/column=1
trun(alt_agqlocation:address) + " " + trun(alt_agqlocation:CITY) + ", " +
trun(alt_agqlocation:str_state) + " " + trun(alt_agqlocation:str_zipcode)
/newline
       
"BUSINESS DESCRIPTION:"/column=1
trun(agqcontractor:BUSINESS_DESC[1]) + " " + trun(agqcontractor:business_desc
[2])/newline=2

"STATE:"/COLUMN=1
alt_sfsstate:description

"COUNTY:"/COlumn=40
ALT1_sfsstate:DESCRIPTION/newline

"FORM:"/column=1
agqmaster:form
uppercase(sfsform:description)/newline
    

if sfscompany:company_id one of "DELOS" then
  {
    "LIABILITY:"/column=1    --property           --inland marine    --identity theft
     l_liability_only_once /align=agqcontractor:building_premium/mask="ZZ,ZZZ.99-"/newline/duplicates 
  }

if agqcontractor:form <> 4 then
{
"FIRE LEGAL:"/column=1
agqcontractor:fire_premium/align=agqcontractor:building_premium/mask=
"ZZ,ZZZ.99-"/newline/duplicates
}
end box

followed by

box/noheadings/noblanklines 
"LOCATION"/column=1
"BUILDING"/COLUMN=10                  
"FORM/EDITION"/column=30
"PREMIUM"/newline/column=100 
end box

followed by

box/noheadings/noblanklines 
alt_agqend1:code/column=20/duplicates
alt_agqend1:description/column=30/duplicates
alt_agqend1:premium/mask="ZZ,ZZZ.99-"/column= 100/duplicates 
end box

followed by

box/noheadings/noblanklines 
alt_agqend:prem_no/duplicates 
alt_agqend:build_no/duplicates/column=9 
alt_agqend:code/column=20/duplicates
alt_agqend:description/column=30/duplicates
alt_agqend:premium/mask="ZZ,ZZZ.99"/column= 100/duplicates  
end box/newline

followed by  
                                 
if alt_agqsupp:app_no = agqcontractor:app_no then
    box/noblanklines/noheadings
    "LOCATION"/column=1
    "BUILDING"/COLUMN=10                  
    "ADDITIONAL INTEREST"/column=30
    end box

followed by

if alt_agqsupp:app_no = agqcontractor:app_no then
    box/noheadings/noblanklines/duplicates 
    alt_agqsupp:prem_no/duplicates/column= 1
    alt_agqsupp:build_no/duplicates/column=9
    if alt_agqsupp:mortgage_id <> "" then
        {
        trun(alt_sfsmort:name[1]) + " " +
        trun(alt_sfsmort:address[1]) + " " +
        trun(alt_sfsmort:CITY) + ", " +
        trun(alt_sfsmort:str_state) + " " +
        trun(alt_sfsmort:str_ZIPCODE)
        } 
    else
        {  
        trun(alt_agqsupp:NAME[1]) + " " +
        trun(alt_agqsupp:address[1]) + " " +
        trun(alt_agqsupp:CITY) + ", " +
        trun(alt_agqsupp:str_state) + " " +
        trun(alt_agqsupp:str_ZIPCODE)
        }
    xob/newlines
                          
followed by  
           
if agqmisc:app_no = agqcontractor:app_no then
{        
"MISCELLANEOUS"/newline=2
"DESCRIPTION"/column=18
"PREMIUM"/column=50/newline=2
agqmisc:description/column=11
agqmisc:premium/column=52/mask="ZZZZZ-"
}

followed by

box
  "Liability Rating Calculations"
  ""/newline
  "Class Code"/column=1
  "Payroll"/column= 15 
  "CSL Factor Per"/column=30
   agqclasscode:per_payroll/mask="$Z,ZZZ"/column=45
  "HVAC"/column=55
  "Package"/column=75  
  "Premium"/column=95/newline
  "Aggregate Limit"/column= 55
  "Mod Factor"/column= 75
xob

followed by
box
/duplicates 
/noblanklines 
agqclasscode:print_class_code/mask="ZZZZZ"/column=1 
payroll_amount /column= 15
csl_factor /column= 30
hvac_factor /column= 55
package_mod/column= 75
payroll_premium /column= 95
xob

followed by
"Payroll Flat Charge"/column=1 
agqmaster:other_totals[2]/column= 20
   

followed by
  
box
""/newline
"Additional G/L Rated Classes"/newline=2
"Class"/column= 1
"Description"/column= 10
"Exposure Type"/column= 45
"Exposure Base"/column= 80
"Premium"/column= 95/newline
agqadditionalclass_alias:class_code/column= 1
agqadditionalclass_alias:description/column= 10
cpsliabexpbase_alias:description/column= 45
agqadditionalclass_alias:exposure_base/column= 80
agqadditionalclass_alias:premium/column= 95
xob
 
sorted by agqcontractor:app_no/newpage 

end of report
" "/newline=2
todaysdate/heading="DATE PRINTED"
username/column=40/heading="PRINTED BY"/newline
alt_sfscomun:name/heading="UNDERWRITER"/column=1

top of report
l_company_name/column=30/newline=2
agqcontractor:APP_NO/HEADING="APP NO"                    

if agqcontractor:business_name[1] <> "" then
    {
    agqcontractor:business_name[1]/column=30/newline=2 
    }
"EFFECTIVE DATE:"/column=1
agqcontractor:EFF_DATE/column=17
"EXPIRATION DATE:"/column=29
agqcontractor:EXP_DATE
"TRANS DATE:"
agqcontractor:ENTRY_DATE/NEWLiNE=2

"LOB:         "/column=1           
alt_sfsline:description/column=19

"FOB:"/COLUMN=45
if alt_agqmaster:fob_description = "" then
    alt_sfsfob:description/column=59
else
    alt_agqmaster:fob_description/newline

"IRPM:"/COLUMN=1
alt_agqmaster:IRPM/mask="-ZZ.ZZ"/column=19/newline=3

"TOTAL:"/column=20
alt_agqmaster:total[18]/column=35/mask="$$,$$$,$$$.99"/newline

if alt_agqmaster:total[19] <> 0 then                              
  { 
    "IRPM TOTAL:"/column=20
    alt_agqmaster:total[19]/column=35/mask="$$,$$$,$$$.99-"/newline 
  }                         

"TERRORISM:"/column=20
alt_agqmaster:terrorism_premium/column=35/mask="$$,$$$,$$$.99"/newline

"Identity Theft"/column=20
alt_agqmaster:total[7]/newline

"FINAL TOTAL:"/column=20
if alt_agqmaster:total[19] <> 0 then
  box/noblanklines/noheading
    alt_agqmaster:total[18] + alt_agqmaster:total[7] + alt_agqmaster:total[19] + alt_agqmaster:terrorism_premium/column=3/mask="$$,$$$,$$$.99"/newline
  end box
else
    box/noblanklines/noheading
    alt_agqmaster:total[7] + alt_agqmaster:total[18] + alt_agqmaster:terrorism_premium/column=3/mask="$$,$$$,$$$.99"/newline
    end box
IF alt_agqmaster:minimum_prem_applies = "Y" then
    "M.P."/newline
else
    ""                 
"SURCHARGE:"/column=20
alt_agqmaster:surcharge/column=35/mask="$$,$$$,$$$.99"/newline=2
    
box/noblanklines/noheadings/column=1
alt_sfsAGENT:NAME[1]/COLUMN=1/newline
alt_sfsagent:name[2]/COLUMN=1/NEWLINE
alt_sfsagent:name[3]/column=1/newline
alt_sfsagent:address[1]/COLUMN=1/NEWLINE
alt_sfsagent:address[2]/column=1/newline
alt_sfsagent:address[3]/column=1/newline          
box/noblanklines/noheadings/squeeze/column=1
alt_sfsagent:CITY + ","/COLUMN=1
alt_sfsagent:str_state
if alt_sfsagent:zipcode[1,4] = 0 then
    {
     alt_sfsagent:ZIPCODE/mask="99999"
     }

else  
    {
    alt_sfsagent:zipcode/mask="99999-9999"/newline
    }
alt_sfsagent:TELEPHONE[1]/COLUMN=1/newline
end box                   
end box/newline=2


           
