/* boppr400.eq

   November 20, 2000 

   SCIPS.com, Inc.

   policy worksheet for MSO BOP
*/ 

DEFINE STRING I_NAME[30] = sfpname:name[1]

include "renaeq1.inc"

/*DEFINE STRING
        I_REV_NAME[50] =
        TRUN(I_NAME[(POS("=",I_NAME)+1),
        LEN(I_NAME)])+" "+TRUN(I_NAME[0,(POS("=",I_NAME)-1)])*/

DEFINE NUMBER L_ZIP_CODE[9] = sfpname:zipcode

DEFINE STRING L_ZIP = STR(L_ZIP_CODE,"999999999")
              l_policy_string[9] = str(sfpname:previous_policy_no)

define signed ascii number l_amount = 0 ;

define string l_money_limits = "$" + str(bopgeneral:money_limit_on) + "/" +
                               "$" + str(bopgeneral:money_limit_off)
define string l_burglary_limits = "$" + str(bopgeneral:burglary_limit_on) + "/" +
                                  "$" + str(bopgeneral:burglary_limit_off)
 
define file alt_sfpmaster = access sfpmaster, set
sfpmaster:policy_no = bopgeneral:policy_no, 
sfpmaster:pol_year = bopgeneral:pol_year,
sfpmaster:end_sequence = bopgeneral:end_sequence, exact

define file alt_sfpmastsupp = access sfpmastsupp, set
sfpmastsupp:policy_no = bopgeneral:policy_no, 
sfpmastsupp:pol_year = bopgeneral:pol_year,
sfpmastsupp:end_sequence = bopgeneral:end_sequence,
sfpmastsupp:line_of_business = bopgeneral:line_of_business, generic 

define file alt_sfplocation = access sfplocation, set
sfplocation:policy_no = bopgeneral:policy_no,
sfplocation:pol_year = bopgeneral:pol_year,
sfplocation:end_sequence = bopgeneral:end_sequence, 
sfplocation:prem_no = bopgeneral:prem_no,
sfplocation:build_no = bopgeneral:build_no, generic

define file alt_sfpend = access sfpend, set
sfpend:policy_no = bopgeneral:policy_no, 
sfpend:pol_year = bopgeneral:pol_year,
sfpend:end_sequence = bopgeneral:end_sequence,
sfpend:prem_no = bopgeneral:prem_no,
sfpend:build_no = bopgeneral:build_no, generic 

define file alt_sfpend1 = access sfpend, set
sfpend:policy_no = bopgeneral:policy_no, 
sfpend:pol_year = bopgeneral:pol_year,
sfpend:end_sequence = bopgeneral:end_sequence, 
sfpend:prem_no  = 0,
sfpend:build_no = 0, generic

define file alt_sfpsupp = access sfpsupp, set
sfpsupp:policy_no = bopgeneral:policy_no, 
sfpsupp:pol_year = bopgeneral:pol_year,
sfpsupp:end_sequence = bopgeneral:end_sequence, 
sfpsupp:prem_no = bopgeneral:prem_no,
sfpsupp:build_no = bopgeneral:build_no, generic

define file alt_sfsagent = access sfsagent, set
sfsagent:company_id = bopgeneral:company_id,
sfsagent:agent_no = sfpname:agent_no, generic
                   
define file alt_sfscomun = access sfscomun, set
sfscomun:company_id= bopgeneral:company_id,
sfscomun:commercial_underwriter= alt_sfsagent:commercial_underwriter, generic  

define file alt_sfspayortype = access sfspayortype, set
sfspayortype:company_id = bopgeneral:company_id,
sfspayortype:payor_code = alt_sfpsupp:mortgage_type_1, generic

define file alt_sfsmorttype = access sfsmorttype, set
sfsmorttype:company_id = bopgeneral:company_id,
sfsmorttype:mortgage_code = alt_sfpsupp:mortgage_type_2, generic
     
define file alt_sfsmort = access sfsmort, set
sfsmort:company_id = bopgeneral:company_id,
sfsmort:mortgage_id = alt_sfpsupp:mortgage_id, generic

define string l_mortgage = if sfpsupp:mortgage_id = "" then
                               trun(alt_sfpsupp:name[1]) +  " " +
                               trun(alt_sfpsupp:address[1]) +  " " +
                               trun(alt_sfpsupp:city) + " " +
                               ", " + trun(alt_sfpsupp:str_state) + " " +
                               trun(alt_sfpsupp:str_zipcode)

define string l_mort = if sfpsupp:mortgage_id <> "" then
                           trun(alt_sfsmort:name[1]) + " " +
                           trun(alt_sfsmort:address[1]) + " " +
                           trun(alt_sfsmort:city) + " " +
                           ", " + trun(alt_sfsmort:str_state) + " " +
                           trun(alt_sfsmort:str_zipcode)

define file alt_sfpani = access sfpani, set
sfpani:policy_no = bopgeneral:policy_no, 
sfpani:pol_year = bopgeneral:pol_year,
sfpani:end_sequence = bopgeneral:end_sequence, exact

define file alt_sfsstate = access sfsstate, set
                   sfsstate:company_id = sfpname:company_id ,
                   sfsstate:state = bopgeneral:state,
                   sfsstate:county= 000, generic

define file alt1_sfsstate = access sfsstate, set
                   sfsstate:company_id = sfpname:company_id,
                   sfsstate:state = bopgeneral:state,
                   sfsstate:county = bopgeneral:county, generic

define file alt_sfsline = access sfsline, set    
sfsline:company_id = sfpname:company_id,
sfsline:line_of_business = sfpname:line_of_business,
sfsline:lob_subline = "00", generic

define unsigned ascii number l_liability[8] = if alt_sfsline:lob_code one of "CPORTFOLIO" then
                                                  alt_sfpmastsupp:liability_code
                                              else
                                                  alt_sfpmaster:liability_code

define file alt_sfsliability = access sfsliability, set
                               sfsliability:company_id = bopgeneral:company_id,

                               sfsliability:state = bopgeneral:state,
                               sfsliability:line_of_business = bopgeneral:line_of_business,
                               sfsliability:liability_code = l_liability, generic



define file alt1_sfsline = access sfsline, 
                             set sfsline:company_id = bopgeneral:company_id ,
                             sfsline:line_of_business = bopgeneral:line_of_business,
                             sfsline:lob_subline = "00", generic
    
define file alt_sfsfob = access sfsfob, 
              set sfsfob:company_id = bopgeneral:company_id,
                  sfsfob:form_of_business = alt_sfpmaster:form_of_business,
                  generic

define file scipscontrol_alias = access scipscontrol,
            set scipscontrol:company_id = bopgeneral:company_id,
                scipscontrol:state      = bopgeneral:state, 
                scipscontrol:lob_code   = alt1_sfsline:lob_code, exact
    
define unsigned ascii number l_form[2] = if alt_sfsline:lob_code one of "CPORTFOLIO" then
                                             alt_sfpmastsupp:form
                                         else
                                             alt_sfpmaster:form

define file alt_sfsform = access sfsform, set
sfsform:company_id = bopgeneral:company_id,
sfsform:state = bopgeneral:state,
sfsform:line_of_business = bopgeneral:line_of_business,
sfsform:policy_form = l_form, generic
                 
define file alt_plppersonal = access plppersonal,
                                 set plppersonal:policy_no = bopgeneral:policy_no,
                                     plppersonal:pol_year = bopgeneral:pol_year,
                                     plppersonal:end_sequence = bopgeneral:end_sequence, 
                                     plppersonal:prem_no  = bopgeneral:prem_no,
                                     plppersonal:build_no = bopgeneral:build_no, generic

define file alt_sfpname = access sfpname, 
                             set sfpname:policy_no = plppersonal:policy_no, exact

define string l_company_name = sfpname:name[1]

define file alt_scipscontrol = access scipscontrol, 
                                  set scipscontrol:company_id = bopgeneral:COMPANY_ID,
                                      scipscontrol:state      = bopgeneral:state,
                                      scipscontrol:lob_code   = alt_SFSLINE:LOB_CODE, exact

where bopgeneral:policy_no = sfpprint8:policy_no and
bopgeneral:pol_year = sfpprint8:pol_year and
bopgeneral:end_sequence = sfpprint8:end_sequence

LIST
/NOPAGEHEADINGS
/NOBANNER
/NOTOTALS
--/NOBLANKLINES
/DOMAIN="bopgeneral"
/INLINE
/NOHEADIngs
/noduplicates
--/pagelength=0

box/noblanklines/noheadings/duplicates/column=1
"LOCATION:   "/COLUMN=30
bopgeneral:PREM_NO/MASK="ZZZ9"/NEWLINE/column=42
"BUILDING:       "/COLUMN=30 
bopgeneral:BUILD_NO/MASK="ZZZ9"/NEWLINE=2/column=42

"LOCATION:"/column=1
if alt_sfplocation:policy_no = bopgeneral:policy_no and
   alt_sfplocation:prem_no  = bopgeneral:prem_no and
   alt_sfplocation:build_no = bopgeneral:build_no then
    {
    trun(alt_sfplocation:address) + " " + trun(alt_sfplocation:CITY) + ", " +
    trun(alt_sfplocation:str_state) + " " + trun(alt_sfplocation:str_zipcode)/column=20/newline
    }
else
    {
    "No location has been entered"/column=20/newline/noduplicates
    }
       
"BUSINESS DESCRIPTION:"/column=1
trun(bopgeneral:BUSINESS_DESC[1]) + " " + trun(bopgeneral:business_desc[2])/COLUMN=30/newline=2

"STATE:"/COLUMN=1
alt_sfsstate:description/column=15

"COUNTY:"/COlumn=30
ALT1_sfsstate:DESCRIPTION/newline

"CONSTRUCTION: "/column=1
bopgeneral:CONSTRUCTION/MASK="ZZZ"/COLUMN=20

"SPRINKLERS:"/COLUMN=30
bopgeneral:SPRINKLERS/COLUMN=52

"PROTECTION:"/COLUMN=65
bopgeneral:PROTECTION/NEWLINE/COLUMN=83

"TERRITORY: "/column=1
bopgeneral:TERRITORY/MASK="ZZZ"/COLUMN=20

"TYPE:"/COLUMN=30
bopgeneral:BOP_TYPE/COLUMN=52
BPSTYPE:DESCRIPTION

"TENANT:"/COLUMN=65
bopgeneral:TENANT/newline/COLUMN=84
                   
"YR CONSTRUCTION:"/COLUMN=1
bopgeneral:CONSTRUCTION_YEAR/COLUMN=19/MASK="ZZZZ"      
                             
"% OCCUPIED:"/column=30
bopgeneral:percent_occupied/column=50

"SINGLE OCCUPANCY:"/COLUMN=65
bopgeneral:SINGLE_OCCUPANCY/COLUmn=84/newline 
 
"CENTRAL ALARM %:"/column=1
if bopgeneral:alarm_factor <> 0 then
    bopgeneral:alarm_factor
else
    bopgeneral:ALARM_CREDIT[1]/COLUMN=21

if alt_sfsline:lob_code one of "CPORTFOLIO" then
    {
    "MULTIPLE LOCATION %:"/COLUMN=30
    alt_sfpmastsupp:LOCATION_CREDIT/COLUMN=51
    }
else
    {
    "MULTIPLE LOCATION %:"/COLUMN=30
    alt_sfpmaster:LOCATION_CREDIT/COLUMN=51
    }

"REINSURANCE:"/column=65
sfpname:reinsurance/column=84/newline
             
"LOSS FREE CREDITS:"/column=1
bopgeneral:loss_years/column=21 

"TERRITORY FACTOR:"/column=30
bopgeneral:rating_territory_factor/column=50/newline
 
"TERRITORY BUILDING:"/column=1
bopgeneral:territory_factors[1]/column=21

"TERRITORY PROPERTY:"/column=30
bopgeneral:territory_factors[2]/column=50

"TERRITORY EXPANDED:"/column=65
bopgeneral:territory_factors[3]/column=84/newline

"TIER:"/column=1
bopgeneral:tier/column=21

"SPECIAL ENHANCEMENT:"/column=30
switch(bopgeneral:special_enhancement)
  case 0  : "N"
  default : "Y"/column=52/newline

if bopgeneral:class_code > 0 then
    {
    "CLASS CODE:"/column=1
    bopgeneral:CLASS_CODE/MASK="ZZZZZ"/COLUMN=18
    bpsclass:alpha_look/newline/column=25
    }

"RATE NO:"/column=1
bopgeneral:RATE_NO/COLUMN=21/mask="ZZ"
"RATE GROUP:"/COLUMN=30
bopgeneral:RATE_GROUP/newline=2/mask="ZZ"/column=51

"DELETE LOSS OF INCOME:"/column=1
bopgeneral:delete_loss_income/newline/column=30
 
"LOSS OF INCOME TO EXTRA EXPENSE & RENTAL INCOME ONLY: "/column=1
bopgeneral:LIMIT_LOSS_INCOME/NEWLINE

"LOSS OF BUSINESS INCOME LIMIT: "/column=1
if bopgeneral:loss_income_limit = 0 and
   bopgeneral:delete_loss_income = "N" then  
    {
    "Included"/newline
    }
else  
    {
    bopgeneral:LOSS_INCOME_LIMIT/NEWLINE/mask="$$,$$$,$$$"
    }

if bopgeneral:wholesaler = "Y" then
    box/noblanklines/noheadings/duplicates 
    "WHOLESALER:"/column=1
    bopgeneral:wholesaler/column=24
    "% OF COVERAGE B LIMIT:"/column=37
    bopgeneral:wholesaler_percent/newline/column=57
    end box/newline
 
{
"OFF PREMISES:"/column=1          
bpsdefault:off_premises_a 
if bopgeneral:off_premises_limit = alt_sfpmaster:off_premises_limit or
   bopgeneral:off_premises_limit = 0 then
    {
    alt_sfpmaster:off_premises_limit/mask="$$$,$$$"/column=25
    alt_sfpmaster:off_premises_premium/mask="ZZ,ZZZ.99-"/column=32
    }
else 
    {
    bopgeneral:off_premises_limit/mask="$$$,$$$"/column=25
    bopgeneral:off_premises_premium/mask="ZZ,ZZZ.99-"/column=32
    }
}

"MECHANICAL BREAKDOWN:"/column=46
bopgeneral:MECHANICAL_LIMIT/column=68/mask="$$$,$$$"
bopgeneral:MECHANICAL_PREMIUM/column=75/mask="ZZ,ZZZ.99-"/newline
                                             
"DELETE HIRED/NONOWNED:"/column=1
if alt_sfsline:lob_code one of "CPORTFOLIO" then
    {
    alt_sfpmastsupp:delete_hired_nonowned/align=alt_sfpmaster:off_premises_limit
    if alt_sfpmastsupp:delete_hired_nonowned = "Y" then
        {
        alt_sfpmastsupp:hired_nonowned_premium/mask="ZZ,ZZZ.99-"/align=alt_sfpmaster:off_premises_premium
        }
    else
        {
        l_amount/mask="ZZ,ZZZ.99-"/align=alt_sfpmaster:off_premises_premium
        }
    }
else
    {
    alt_sfpmaster:delete_hired_nonowned/align=alt_sfpmaster:off_premises_limit
    if alt_sfpmaster:delete_hired_nonowned = "Y" then
        {
        alt_sfpmaster:hired_nonowned_premium/mask="ZZ,ZZZ.99-"/align=alt_sfpmaster:off_premises_premium
        }
    else
        {
        l_amount/mask="ZZ,ZZZ.99-"/align=alt_sfpmaster:off_premises_premium
        }
    }

"CUSTOMERS' PROPERTY:"/column=46
bopgeneral:CUSTOMER_PROP_LIMIT/mask="$$$,$$$"/align=bopgeneral:mechanical_limit
bopgeneral:CUSTOMER_PROP_PREMIUM/NEWLINE/mask="ZZ,ZZZ.99-"/align=bopgeneral:mechanical_premium

"EXTEND HIRED/NONOWNED:"/column=1
if alt_sfsline:lob_code one of "CPORTFOLIO" then
    {
    alt_sfpmastsupp:extend_hired_nonowned/duplicates/align=alt_sfpmaster:off_premises_limit
    if alt_sfpmastsupp:extend_hired_nonowned = "Y" then
        {
        alt_sfpmastsupp:Hired_nonowned_premium/mask="ZZ,ZZZ.99-"/align=alt_sfpmaster:off_premises_premium 
        }
    else
        {
        l_amount/mask="ZZ,ZZZ.99-"/align=alt_sfpmaster:off_premises_premium
        }
    }
else
    {
    alt_sfpmaster:extend_hired_nonowned/duplicates/align=alt_sfpmaster:off_premises_limit
    if alt_sfpmaster:extend_hired_nonowned = "Y" then
        {
        alt_sfpmaster:Hired_nonowned_premium/mask="ZZ,ZZZ.99-"/align=alt_sfpmaster:off_premises_premium 
        }
    else
        {
        l_amount/mask="ZZ,ZZZ.99-"/align=alt_sfpmaster:off_premises_premium
        }
    }
 
"SUPPLIES/MATERIALS:   "/column=46
bopgeneral:supplies_LIMIT/mask="$$$,$$$"/align=bopgeneral:mechanical_limit
bopgeneral:supplies_PREMIUM/NEWLINE/mask="ZZ,ZZZ.99-"/align=bopgeneral:mechanical_premium

"OUTDOOR SIGNS:"/column=1
bopgeneral:SIGNS_LIMIT/mask="$$$,$$$"/align=alt_sfpmaster:off_premises_limit
bopgeneral:SIGNS_PREMIUM/mask="ZZ,ZZZ.99-"/align=alt_sfpmaster:off_premises_premium
         
if sfpname:company_id <> "LEBINS" then
{
"POLLUTION CLEANUP:"/column=46
bopgeneral:POLLUTION_LIMIT/mask="$$$,$$$"/align=bopgeneral:mechanical_limit
bopgeneral:POLLUTION_PREMIUM/NEWLINE/mask="ZZ,ZZZ.99-"/align=bopgeneral:mechanical_premium
}
else
{
"DEBRIS REMOVAL:"/column=46
bopgeneral:DEBRIS_REMOVAL_LIMIT/mask="$$$,$$$"/align=bopgeneral:mechanical_limit
bopgeneral:POLLUTION_PREMIUM/NEWLINE/mask="ZZ,ZZZ.99-"/align=bopgeneral:mechanical_premium
}

"SIGNS DEDUCTIBLE:"/column=1
bopgeneral:SIGNS_DEDUCTIBLE[1]/mask="ZZ,ZZ9"/align=alt_sfpmaster:off_premises_limit
                                     
"INFLATION GUARD %:"/column=46
bopgeneral:auto_percent/align=bopgeneral:mechanical_limit
bopgeneral:auto_premium/mask="ZZ,ZZZ.99-"/newline/align=bopgeneral:mechanical_premium

"BUILDING CODE/LAW:"/column=1
bopgeneral:BUILDING_CODE_percent/align=alt_sfpmaster:off_premises_limit
bopgeneral:BUILDING_CODE_PREMIUM/mask="ZZ,ZZZ.99-"/align=alt_sfpmaster:off_premises_premium

"LIMIT WATER BACKUP:"/column=46
bopgeneral:water_backup_limited/newline/align=bopgeneral:mechanical_limit

"EXTERIOR GLASS:"/column=1
bopgeneral:GLASS_LIMIT/mask="ZZZ,ZZ9"/align=alt_sfpmaster:off_premises_limit 
if scipscontrol_alias:glass_square_feet = 0 then
  {
    "LF"
  }
else
  {
    "SQ FT"
  }
bopgeneral:GLASS_PREMIUM/mask="ZZ,ZZZ.99-"/align=alt_sfpmaster:off_premises_premium

"WATER BACKUP LIMIT:   "/column=46
if bopgeneral:water_backup_premium <> 0 then
    {
    bopgeneral:water_backup_limit/mask="$$$,$$$"/align=bopgeneral:mechanical_limit
    }  
else    
    {
    "      0"/align=bopgeneral:mechanical_limit
    }
bopgeneral:water_backup_premium/mask="ZZ,ZZZ.99-"/newline/align=bopgeneral:mechanical_premium  

"INCLUDE GLASS DEDUCTIBLE:"/column=1
bopgeneral:include_glass_deductible/align=alt_sfpmaster:off_premises_limit

"DELETE WATER BACKUP EXCL:"/column=46
bopgeneral:water_backup_exclusion/newline/align=bopgeneral:mechanical_limit

"BURGLARY/ROBBERY:"/column=1              
if bopgeneral:burglary_limit_on <> 0 then
    {            
    l_burglary_limits/align=bopgeneral:glass_limit
    bopgeneral:BURGLARY_PREMIUM/mask="ZZ,ZZZ.99-"/align=alt_sfpmaster:off_premises_premium
    }
else
    {
    "0"/align=alt_sfpmaster:off_premises_limit
    bopgeneral:burglary_premium/mask="ZZ,ZZZ.99-"/align=alt_sfpmaster:off_premises_premium 
    }

"FIRE LEGAL:"/column=46
bopgeneral:FIRE_LIMIT/mask="$$$,$$$,$$$"/align=bopgeneral:water_backup_exclusion 
bopgeneral:FIRE_PREMIUM/NEWLINE/mask="ZZ,ZZZ.99-"/align=bopgeneral:mechanical_premium

"ATTACH BU5001:"/column=1
bopgeneral:ATTACH_BU5001/align=alt_sfpmaster:off_premises_limit

"VALUABLE PAPERS:"/column=46
bopgeneral:VALUABLE_PAPERS_LIMIT/mask="$$$,$$$"/align=bopgeneral:mechanical_limit
bopgeneral:VALUABLE_PAPERS_PREMIUM/NEWLINE/mask="ZZ,ZZZ.99-"/align=bopgeneral:mechanical_premium

"MONEY/SECURITIES:"/column=1 
if l_money_limits <> "" then
    { 
    l_money_limits/align=alt_sfpmaster:off_premises_limit
    bopgeneral:MONEY_PREMIUM/mask="ZZ,ZZZ.99-"/align=alt_sfpmaster:off_premises_premium
    }
else
    {
    "0"/align=alt_sfpmaster:off_premises_limit
    bopgeneral:money_premium/mask="ZZ,ZZZ.99-"/align=alt_sfpmaster:off_premises_premium
    }

"WIND DEDUCTIBLE:"/column=46
bopgeneral:wind_deductible/mask="ZZ,ZZ9"/newline/align=bopgeneral:mechanical_limit

"ACCOUNTS RECEIVABLE:"/column=1
bopgeneral:AR_LIMIT/mask="$$$,$$$"/align=alt_sfpmaster:off_premises_limit
bopgeneral:AR_PREMIUM/mask="ZZ,ZZZ.99-"/align=alt_sfpmaster:off_premises_premium

"BOILERS:"/column=46
bopgeneral:boilers/align=bopgeneral:mechanical_limit
bopgeneral:boilers_premium/newline/mask="ZZ,ZZZ.99-"/align=bopgeneral:mechanical_premium

"HURRICANE DEDUCTIBLE:"/column=1
bopgeneral1:hurricane_deductible/mask="ZZZZ9%"/align=alt_sfpmaster:off_premises_limit
"OVERRIDE HURRICANE DEDUCTIBLE:"/column=46
bopgeneral1:override_hurricane_deductible/mask="ZZZZ9%"/align=bopgeneral:mechanical_premium/newline

if bopgeneral:receipts <> 0 then
    box/noblanklines/noheading
    "UTILITY SERVICES:"/column=1
     bopgeneral:UTILITY_LIMIT/mask="$$$,$$$"/align=alt_sfpmaster:off_premises_limit
     bopgeneral:UTILITY_PREMIUM/mask="ZZ,ZZZ.99-"/align=alt_sfpmaster:off_premises_premium 
    "RESTAURANT:"/column=46
    bopgeneral:receipts/align=bopgeneral:mechanical_limit
    bopgeneral:restaurant_enhancement/mask="ZZ,ZZZ.99-"/align=bopgeneral:mechanical_premium
    end box/newlines=2
else
if bopgeneral:signs_not_attached_limit <> 0 then
    box/noblanklines/noheading                                   
    "UTILITY SERVICES:"/column=1
     bopgeneral:UTILITY_LIMIT/mask="$$$,$$$"/align=alt_sfpmaster:off_premises_limit
     bopgeneral:UTILITY_PREMIUM/mask="ZZ,ZZZ.99-"/align=alt_sfpmaster:off_premises_premium 
    "SIGNS NOT ATTACHED:"/column=46
    bopgeneral:signs_not_attached_limit/mask="$$$,$$$"/align=bopgeneral:mechanical_limit
    bopgeneral:signs_not_attached_premium/mask="ZZ,ZZZ.99-"/align=bopgeneral:mechanical_premium 
    end box/newlines=2
else
    box/noblanklines/noheading
    "UTILITY SERVICES:"/column=1
     bopgeneral:UTILITY_LIMIT/mask="$$$,$$$"/align=alt_sfpmaster:off_premises_limit
     bopgeneral:UTILITY_PREMIUM/mask="ZZ,ZZZ.99-"/align=alt_sfpmaster:off_premises_premium 
     end box/newlines=2

if sfpname:company_id = "NWIC" and
   bopgeneral:state = 29 then
    box/noblanklines/noheading
    "CREDIT CARD/FORGERY:"/column=1
    bopgeneral1:credit_card/mask="$$$,$$$"/align=alt_sfpmaster:off_premises_limit
    bopgeneral1:credit_card_PREMIUM/mask="ZZ,ZZZ.99-"/align=alt_sfpmaster:off_premises_premium 
    "ELECTRONIC DATA:"/column=46
    bopgeneral1:electronic_data/mask="$$$,$$$"/align=bopgeneral:mechanical_limit
    bopgeneral1:electronic_data_premium/mask="ZZ,ZZZ.99-"/align=bopgeneral:mechanical_premium/newline
    "DEPENDENT PROPERTY:"/column=1
    bopgeneral1:dependent_property/mask="$$$,$$$"/align=alt_sfpmaster:off_premises_limit
    bopgeneral1:dependent_property_PREMIUM/mask="ZZ,ZZZ.99-"/align=alt_sfpmaster:off_premises_premium 
    "INTERRUPTION:"/column=46
    bopgeneral1:interruption/mask="$$$,$$$"/align=bopgeneral:mechanical_limit
    bopgeneral1:interruption_premium/mask="ZZ,ZZZ.99-"/align=bopgeneral:mechanical_premium 
    end box/newline=2

"LIABILITY FLAT CHARGE:"/column=1
bopgeneral:liability_charge/mask="ZZ,ZZZ.99"/align=alt_sfpmaster:off_premises_premium 

if sfscompany:special_territory_rating = 1 and
   bopgeneral:no_pools = 0 then
    {
    "TERRITORY CHARGE/CREDIT:"/column=46
    bopgeneral:territory_charge_credit/NEWLINE=2/mask="ZZ,ZZZ.99-"/align=bopgeneral:mechanical_premium                          
    }
else
    {
    "TERRITORY CHARGE/CREDIT:"/column=46
    bopgeneral:territory_charge_credit/NEWLINE/mask="ZZ,ZZZ.99-"/align=bopgeneral:mechanical_premium                          
    }

if sfpname:company_id = "FARMERS" then
    {
    "EPL LIMIT/DEDUCT:"/column=1
    str(sfpepl:epl_limit,"Z,ZZZ,ZZZ") + "/" + str(sfpepl:epl_deductible,"ZZZZZ")/align=bopgeneral:off_premises_limit
    sfpepl:epl_premium/mask="ZZ,ZZZ.99-"/align=bopgeneral:off_premises_premium

    "DEBRIS REMOVAL:"/column=46
     bopgeneral:DEBRIS_REMOVAL_LIMIT/mask="$$$,$$$"/align=alt_sfpmaster:off_premises_limit
     bopgeneral:DEBRIS_REMOVAL_PREMIUM/mask="ZZ,ZZZ.99-"/align=alt_sfpmaster:off_premises_premium /newline

if sfpepl:third_party = 1 then
    box/noblanklines/noheading
    "Third Party Option:"/column=1
    sfpepl:third_party_premium/mask="ZZ,ZZZ.99-"/align=bopgeneral:off_premises_premium
    if sfpepl:epl_limit < 500000 then
        box/noblanklines/noheading/column=1
        "EPL Percent:"/column=46
        sfpepl:epl_percent/mask="ZZ.ZZ%"/align=bopgeneral:mechanical_limit
        end box
    end box/newline=2
else
if sfpepl:epl_limit < 500000 then
    box/noblanklines/noheading
    "EPL Percent:"/column=1
    sfpepl:epl_percent/mask="ZZ.ZZ%"/align=bopgeneral:off_premises_limit
    end box/newline=2
    }

if bopgeneral:no_pools <> 0 then
    {
    "NO POOLS:"/column=1
    bopgeneral:no_pools/mask="ZZZZ"/align=alt_sfpmaster:off_premises_limit
    bopgeneral:pool_premium/mask="ZZ,ZZZ.99"/align=alt_sfpmaster:off_premises_premium/newline=2
    }

if sfscompany:optional_coverage_screen = 1 then
    {
    "FUEL PUMP CHARGE:"/column=46
    bopgeneral:fuel_pump_charge/mask="ZZ,ZZZ.99"/align=bopgeneral:mechanical_premium/newline=2
    }

if sfscompany:optional_coverage_screen not one of 1 and
   sfscompany:special_territory_rating not one of 1 then
    {
    ""/newline=2
    }

"LIMITS"/align=alt_sfpmaster:off_premises_limit
"PREMIUM"/align=alt_sfpmaster:off_premises_premium
"LIMITS"/align=bopgeneral:mechanical_limit
"PREMIUM"/NEWLINE/align=bopgeneral:mechanical_premium
if bopgeneral:liability_only = 0 then
{
"BUILDING LIMIT:"/column=1
bopgeneral:BUILDING_LIMIT/MASK="$$,$$$,$$$"/align=alt_sfpmaster:off_premises_limit
bopgeneral:BUILDING_PREMIUM/MASK="$$$,$$$.99"/align=alt_sfpmaster:off_premises_premium
}
else
{
"LIABILITY ONLY:"/column=1
bopgeneral:BUILDING_LIMIT/MASK="$$,$$$,$$$"/align=
alt_sfpmaster:off_premises_limit
bopgeneral:BUILDING_PREMIUM/MASK="$$$,$$$.99"/align=alt_sfpmaster:off_premises_premium
}

"PERSONAL PROPERTY:"/column=46
bopgeneral:PROPerty_LIMIT/MASK="$$,$$$,$$$"/align=bopgeneral:mechanical_limit
bopgeneral:PROPERTY_PREMIUM/MASK="$$$,$$$.99"/newline/align=bopgeneral:mechanical_premium

"OTHER STRUCTURES"/column=1
bopgeneral:other_structures/mask="$$,$$$,$$$"/align=alt_sfpmaster:off_premises_limit
 
"EXPANDED PREMIUM:"/column=46
bopgeneral:property_special/mask="$$$,$$$.99"/newline/align=bopgeneral:mechanical_premium/duplicates

"ACV BUILDING:"/column=1
bopgeneral:ACV_building/align=alt_sfpmaster:off_premises_limit
bopgeneral:ACV_building_PREMIUM/mask="$$$,$$$.99"/align=alt_sfpmaster:off_premises_premium

"ACV CONTENTS:"/column=46
bopgeneral:ACV_CONTENTS/align=bopgeneral:mechanical_limit
bopgeneral:ACV_CONTENTS_PREMIUM/NEWLINE=4/mask="$$$,$$$.99"/align=bopgeneral:mechanical_premium
end box
                                       
followed by
 
--if alt_sfpend:prem_no <> 0 then   
    box/noheadings/noblanklines/column=1 
    "LOC"/column=1
    "BUILD"/COLUMN=9
    "FORM/EDITION"/column=25
    "PREMIUM"/newline/column=75
    end box                     

followed by
if ((alt_sfpend1:lob_end_code one of "X" and
   sfpname:line_of_business one of 99) or
   sfpname:line_of_business not one of 99) then
  {
box/noheadings/noblanklines /column=1
alt_sfpend1:prem_no/duplicates/column=1/mask="ZZZ9"
alt_sfpend1:build_no/duplicates/column=9/mask="ZZZ9" 
alt_sfpend1:code + " " + alt_sfpend1:description[1,50]/column=20/duplicates
alt_sfpend1:premium/mask="ZZ,ZZZ.99-"/duplicates/align=bopgeneral:mechanical_premium
end box
  }
followed by 
if ((alt_sfpend:lob_end_code one of "X" and
    sfpname:line_of_business one of 99) or
   sfpname:line_of_business not one of 99) then
  {
box/noheadings/noblanklines /column=1
alt_sfpend:prem_no/duplicates/column=1/mask="ZZZ9"
alt_sfpend:build_no/duplicates/column=9/mask="ZZZ9" 
alt_sfpend:code + " " + alt_sfpend:description[1,50]/column=20/duplicates
alt_sfpend:premium/mask="ZZ,ZZZ.99-"/duplicates/align=bopgeneral:mechanical_premium
end box/newlines=2
  }
followed by 
                    
if alt_sfpsupp:prem_no <> 0 then
    box/noblanklines/noheadings/column=1
    "LOC"/column=1/duplicates
    "BUILD"/COLUMN=9
    "ADDITIONAL INTEREST"/newline/column=25
    end box

followed by
    
if alt_sfpsupp:prem_no <> 0 then
box/noblanklines/noheadings
alt_sfpsupp:prem_no/duplicates/column= 1
alt_sfpsupp:build_no/duplicates/column=9
if alt_sfpsupp:mortgage_id <> "" then
    {
    trun(alt_sfsmort:name[1]) + " " +
    trun(alt_sfsmort:address[1]) + " " +
    trun(alt_sfsmort:CITY) + ", " +
    trun(alt_sfsmort:str_state) + " " +
    trun(alt_sfsmort:str_ZIPCODE)/column=20
    }
else
    {  
    trun(alt_sfpsupp:NAME[1]) + " " +
    trun(alt_sfpsupp:address[1]) + " " +
    trun(alt_sfpsupp:CITY) + ", " +
    trun(alt_sfpsupp:str_state) + " " +
    trun(alt_sfpsupp:str_ZIPCODE)/column=20
    }    
end box/newlines
                     
followed by  
           
if sfpmisc:policy_no = bopgeneral:policy_no then
{        
"MISCELLANEOUS"/newline=2
"DESCRIPTION"/column=18
"PREMIUM"/column=50/newline=2
sfpmisc:description/column=11
sfpmisc:premium/column=52/mask="ZZZZZ-"
}
 
sorted by bopgeneral:policy_no/newpage 
          bopgeneral:prem_no
          bopgeneral:build_no

end of report                             
""/newline=2
"DATE PRINTED:"/column=1
todaysdate/mask="MM/DD/YYYY"
"PRINTED BY:"/column=40
username/newline
if sfpname:company_id not one of "FLEMINGTON" then
    box/noblanklines/noheading
    "UNDERWRITER:"/column=1
    alt_sfscomun:name
    end box

top of report
l_company_name/column=30/newline=2
"POLICY NO:"/column=1
sfpname:POLICY_NO/column=12
sfpprint8:pol_year/column=25
sfpprint8:end_sequence/column=30/newline

include "renaeq2.inc"/newline/column=36
/*if sfpname:name[1] <> "" then
    {
    sfpname:name[1]/column=36/newline     
    }*/
IF sfpname:name[2] <> "" THEN
    {
    sfpname:name[2]/COLUMN=36/NEWLINE
    }
IF sfpname:name[3] <> "" THEN
    {
    sfpname:name[3]/COLUMN=36/NEWLINE
    }
IF sfpname:address[1] <> "" THEN
    {
    sfpname:address[1]/COLUMN=36/NEWLINE
    }
if sfpname:address[2] <> "" then
    {
    sfpname:address[2]/column=36/newline
    }
if sfpname:address[3] <> "" then
    {
    sfpname:address[3]/column=36/newline
    }
box/noblanklines/noheadings/squeeze/column=36
sfpname:city + ","
sfpname:str_state
sfpname:str_zipcode
end box/newlines=2

"EFF DATE:"/column=1
sfpname:EFF_DATE/column=12
"EXP DATE:"/column=25
sfpname:EXP_DATE/column=35
"TRANS DATE:"/column=50
sfpname:TRANS_DATE/NEWLiNE=2/column=65

"LOB:         "/column=1           
alt1_sfsline:description/column=25

"FOB:"/COLUMN=50
if alt_sfpmaster:fob_description = "" then
    alt_sfsfob:description/column=70
else
    alt_sfpmaster:fob_description/newline

"FORM:        "/COLUMN=1
alt_sfsform:description/column=25

"BILL PLAN:"/COLUMN=50
switch(sfpname:bill_plan)
  CASE "AC" : "ACCOUNT CURRENT"
  CASE "DB" : "DIRECT BILL"
  DEFAULT   : "UNKNOWN"/NEWLINE/column=70

"PAYMENT PLAN:"/COLUMN=1
arspayplan:description/column=25

"LIABILITY:"/COLUMN=50
alt_sfsliability:limit/newline/mask="$$,$$$,$$$"/column=70

if alt_sfpmastsupp:policy_no = bopgeneral:policy_no then
    {
    "GENERAL AGGREGATE:"/column=1
    alt_sfpmastsupp:GENERAL_AGGREGATE/column=25/mask="$$,$$$,$$$"

    "MEDICAL PAYMENTS:"/column=50
    alt_sfpmastsupp:MEDical_LIMIT/column=70/mask="$$,$$$,$$$"/newline

    "PERSONAL ADD INJURY:"/column=1
    alt_sfpmastsupp:PERSonal_ADD_INJURY/column=25/mask="$$,$$$,$$$"

    "DEDUCTIBLE:  "/COLUMN=50
    alt_sfpmastsupp:DEDUCTIBLE/mask="$$$$$"/column=74/newline
    }
else
    {
    "GENERAL AGGREGATE:"/column=1
    alt_sfpmaster:GENERAL_AGGREGATE/column=25/mask="$$,$$$,$$$"

    "MEDICAL PAYMENTS:"/column=50
    alt_sfpmaster:MEDical_LIMIT/column=70/mask="$$,$$$,$$$"/newline

    "PERSONAL ADD INJURY:"/column=1
    alt_sfpmaster:PERSonal_ADD_INJURY/column=25/mask="$$,$$$,$$$"

    "DEDUCTIBLE:  "/COLUMN=50
    alt_sfpmaster:DEDUCTIBLE/mask="$$$$$"/column=74/newline
    }

"PREVIOUS POLICY:"/COLUMN=1
trun(alt1_sfsline:alpha) + trun(l_policy_string)/column=25

if alt_sfpmastsupp:irpm <> 0 then
    {
    "IRPM:"/COLUMN=50
    if alt_sfpmastsupp:irpm > 0 then
        {
        alt_sfpmastsupp:IRPM/column=77/newline/mask="ZZ%"
        }                 
    else
    if alt_sfpmastsupp:irpm < 0 then
       {
       alt_sfpmastsupp:irpm/column=77/newline/mask="-ZZ%"
       }
    else
        {
        ""/newline
        }
    }
else
    {
    "IRPM:"/COLUMN=50
    if alt_sfpmaster:irpm > 0 then
        {
        alt_sfpmaster:IRPM/column=77/newline/mask="ZZ%"
        }                 
    else
    if alt_sfpmaster:irpm < 0 then
       {
       alt_sfpmaster:irpm/column=77/newline/mask="-ZZ%"
       }
    else
        {
        ""/newline
        }
    }

"SPECIAL COMM:"/COLUMN=1
sfpname:SPEC_COMM_rate/mask="ZZ.ZZ"/duplicates/column=25
SWITCH(alt_sfpmaster:SPLIT_COMM)
  CASE "A" : " - AGCY ONLY"
  CASE "B" : " - AGCY/CO  "
  DEFAULT  : ""/newline=3

if sfpmastsupp:policy_no <> 0 then
    box/noblanklines/noheading
    "TOTAL:"/column=10
    alt_sfpmastsupp:total[1]/column=30/mask="$$,$$$,$$$.99"/newline
    "BOILER & MACHINERY:"/column=10
    alt_sfpmastsupp:total[2]/column=30/mask="$$,$$$,$$$.99"/newline
    "EPL:"/column=10
    alt_sfpmaster:total[9]/column=30/mask="$$,$$$,$$$.99"/newline
    if alt_sfpmastsupp:total[19] <> 0 then                              
        {
        "IRPM TOTAL:"/column=10
        if bpsdefault:display_irpm_difference = 1 then
            box/noblanklines/noheading 
            ((alt_sfpmastsupp:total[19] - alt_sfpmastsupp:terrorism_premium) -
            alt_sfpmastsupp:total[7] -
            alt_sfpmastsupp:total[18])/column=9/mask="$$,$$$,$$$.99-"
            end box/newline
        else
            box/noblanklines/noheading 
            alt_sfpmastsupp:total[19] - alt_sfpmastsupp:terrorism_premium/column=9/mask="$$,$$$,$$$.99-"
            end box/newline
        }                             

    "TERRORISM:"/column=10
    alt_sfpmastsupp:terrorism_premium/column=30/mask="$$,$$$,$$$.99"/newline
    if alt_sfpmastsupp:total[7] <> 0 then
        {
        "IDENTITY THEFT:"/column=10
        alt_sfpmastsupp:total[7]/column=30/mask="$$,$$$,$$$.99"/newline
        }
    if alt_scipscontrol:total_premium_wording[1] <> "" then
        {
        uppercase(alt_scipscontrol:total_premium_wording)/column=10
        }
    else
        {
        "FINAL TOTAL:"/column=10
        }
    if alt_sfpmastsupp:total[19] <> 0 then
        {
        alt_sfpmastsupp:total[19]/column=30/mask="$$,$$$,$$$.99"
        }
    else
        {
        alt_sfpmastsupp:total[18]/column=30/mask="$$,$$$,$$$.99"
        }
    IF alt_sfpmastsupp:minimum_prem_applies = "Y" then
        "M.P."/newline
    else
        ""                 
    if alt_scipscontrol:total_amount_due_wording[1] <> "" then
        {
        uppercase(alt_scipscontrol:surcharge_wording)/column=10
        alt_sfpmastsupp:surcharge/column=30/mask="$$,$$$,$$$.99"/newline
        if sfpname:reinsurance = "Y" then
            box/noblanklines/noheading
            "REINSURANCE:"/column=20
            sfpreins:premium/column=30/mask="$$,$$$,$$$.99"
            end box/newlines
        else
            box/noblanklines/noheadings
            ""/newline
            end box

        uppercase(alt_scipscontrol:total_amount_due_wording[1])/column=10
        if alt_sfpmastsupp:total[19] <> 0 then
            {
            alt_sfpmastsupp:total[19] +
            alt_sfpmastsupp:surcharge/column=30/mask="$$,$$$,$$$.99"/newline
            }
        else
            {
            alt_sfpmastsupp:total[18] +
            alt_sfpmastsupp:surcharge/column=30/mask="$$,$$$,$$$.99"/newline
            }
        }
    else
        {
        uppercase(alt_scipscontrol:surcharge_wording)/column=10
        alt_sfpmastsupp:surcharge/column=30/mask="$$,$$$,$$$.99"/newline
        if sfpname:reinsurance = "Y" then
            box/noblanklines/noheading
            "REINSURANCE:"/column=20
            sfpreins:premium/column=30/mask="$$,$$$,$$$.99"
            end box/newline
        else
            box/noblanklines/noheadings
            ""/newline
            end box/newline
        }  
    end box/newline=2
else
if sfpname:company_id = "FARMERS" then
    box/noblanklines/noheading
    "TOTAL:"/column=10
    alt_sfpmaster:total[17]/column=30/mask="$$,$$$,$$$.99"/newline
--    alt_sfpmaster:total[1] - alt_sfpmaster:total[8]/column=30/mask="$$,$$$,$$$.99"/newline
    if alt_sfpmaster:total[19] <> 0 then                              
        {
        "IRPM TOTAL:"/column=10
        if bpsdefault:display_irpm_difference = 1 then
            box/noblanklines/noheading 
            ((alt_sfpmaster:total[19] - alt_sfpmaster:terrorism_premium) -
            alt_sfpmaster:total[7] - alt_sfpmaster:total[8] -
            alt_sfpmaster:total[18])/column=9/mask="$$,$$$,$$$.99-"
            end box/newline
        else
            box/noblanklines/noheading 
            alt_sfpmaster:total[19] - alt_sfpmaster:terrorism_premium -
            alt_sfpmaster:total[7] - alt_sfpmaster:total[8]/column=9/mask="$$,$$$,$$$.99-"
            end box/newline
        }                             
    "BOILER & MACHINERY:"/column=10
    alt_sfpmaster:total[2]/column=30/mask="$$,$$$,$$$.99"/newline
    "EPL:"/column=10
    alt_sfpmaster:total[9]/column=30/mask="$$,$$$,$$$.99"/newline
    "EPL EXTENDED:"/column=10
    alt_sfpmaster:total[10]/column=30/newline--/mask="$$,$$$,$$$.99"/newline
    "TERRORISM:"/column=10
    alt_sfpmaster:terrorism_premium/column=30/mask="$$,$$$,$$$.99"/newline
    if alt_sfpmaster:total[7] <> 0 then
        {
        "IDENTITY THEFT:"/column=10
        alt_sfpmaster:total[7]/column=30/mask="$$,$$$,$$$.99"/newline
        }
    if alt_scipscontrol:total_premium_wording[1] <> "" then
        {
        uppercase(alt_scipscontrol:total_premium_wording)/column=10
        }
    else
        {
        "FINAL TOTAL:"/column=10
        }
    if alt_sfpmaster:total[19] <> 0 then
        {
        alt_sfpmaster:total[19]/column=30/mask="$$,$$$,$$$.99"
        }
    else
        {
        alt_sfpmaster:total[18]/column=30/mask="$$,$$$,$$$.99"
        }
    IF alt_sfpmaster:minimum_prem_applies = "Y" then
        "M.P."/newline
    else
        ""                 
    if alt_scipscontrol:total_amount_due_wording[1] <> "" then
        {
        uppercase(alt_scipscontrol:surcharge_wording)/column=10
        alt_sfpmaster:surcharge/column=30/mask="$$,$$$,$$$.99"/newline
        if sfpname:reinsurance = "Y" then
            box/noblanklines/noheading
            "REINSURANCE:"/column=20
            sfpreins:premium/column=30/mask="$$,$$$,$$$.99"
            end box/newlines
        else
            box/noblanklines/noheadings
            ""/newline
            end box

        uppercase(alt_scipscontrol:total_amount_due_wording[1])/column=10
        if alt_sfpmaster:total[19] <> 0 then
            {
            alt_sfpmaster:total[19] +
            alt_sfpmaster:surcharge/column=30/mask="$$,$$$,$$$.99"/newline
            }
        else
            {
            alt_sfpmaster:total[18] +
            alt_sfpmaster:surcharge/column=30/mask="$$,$$$,$$$.99"/newline
            }
        }
    else
        {
        uppercase(alt_scipscontrol:surcharge_wording)/column=10
        alt_sfpmaster:surcharge/column=30/mask="$$,$$$,$$$.99"/newline
        if sfpname:reinsurance = "Y" then
            box/noblanklines/noheading
            "REINSURANCE:"/column=20
            sfpreins:premium/column=30/mask="$$,$$$,$$$.99"
            end box/newline
        else
            box/noblanklines/noheadings
            ""/newline
            end box/newline
        }  
    end box/newline=2
else
if sfpname:company_id <> "FARMERS" then
    box/noblanklines/noheading
    "TOTAL:"/column=10
    alt_sfpmaster:total[1] - alt_sfpmaster:total[8]/column=30/mask="$$,$$$,$$$.99"/newline
    "BOILER & MACHINERY:"/column=10
    alt_sfpmaster:total[2]/column=30/mask="$$,$$$,$$$.99"/newline
    "EPL:"/column=10
    alt_sfpmaster:total[9]/column=30/mask="$$,$$$,$$$.99"/newline
    "EPL EXTENDED:"/column=10
    alt_sfpmaster:total[10]/column=30/newline--/mask="$$,$$$,$$$.99"/newline
    if alt_sfpmaster:total[19] <> 0 then                              
        {
        "IRPM TOTAL:"/column=10
        if bpsdefault:display_irpm_difference = 1 then
            box/noblanklines/noheading 
            ((alt_sfpmaster:total[19] - alt_sfpmaster:terrorism_premium) -
            alt_sfpmaster:total[7] - alt_sfpmaster:total[8] -
            alt_sfpmaster:total[18])/column=9/mask="$$,$$$,$$$.99-"
            end box/newline
        else
            box/noblanklines/noheading 
            alt_sfpmaster:total[19] - alt_sfpmaster:terrorism_premium -
            alt_sfpmaster:total[7] - alt_sfpmaster:total[8]/column=9/mask="$$,$$$,$$$.99-"
            end box/newline
        }                             
    "SPECIAL ENHANCEMENT:"/column=10
    alt_sfpmaster:total[8]/column=30/mask="$$,$$$,$$$.99"/newline
    "TERRORISM:"/column=10
    alt_sfpmaster:terrorism_premium/column=30/mask="$$,$$$,$$$.99"/newline
    if alt_sfpmaster:total[7] <> 0 then
        {
        "IDENTITY THEFT:"/column=10
        alt_sfpmaster:total[7]/column=30/mask="$$,$$$,$$$.99"/newline
        }
    if alt_scipscontrol:total_premium_wording[1] <> "" then
        {
        uppercase(alt_scipscontrol:total_premium_wording)/column=10
        }
    else
        {
        "FINAL TOTAL:"/column=10
        }
    if alt_sfpmaster:total[19] <> 0 then
        {
        alt_sfpmaster:total[19]/column=30/mask="$$,$$$,$$$.99"
        }
    else
        {
        alt_sfpmaster:total[18]/column=30/mask="$$,$$$,$$$.99"
        }
    IF alt_sfpmaster:minimum_prem_applies = "Y" then
        "M.P."/newline
    else
        ""                 
    if alt_scipscontrol:total_amount_due_wording[1] <> "" then
        {
        uppercase(alt_scipscontrol:surcharge_wording)/column=10
        alt_sfpmaster:surcharge/column=30/mask="$$,$$$,$$$.99"/newline
        if sfpname:reinsurance = "Y" then
            box/noblanklines/noheading
            "REINSURANCE:"/column=20
            sfpreins:premium/column=30/mask="$$,$$$,$$$.99"
            end box/newlines
        else
            box/noblanklines/noheadings
            ""/newline
            end box

        uppercase(alt_scipscontrol:total_amount_due_wording[1])/column=10
        if alt_sfpmaster:total[19] <> 0 then
            {
            alt_sfpmaster:total[19] +
            alt_sfpmaster:surcharge/column=30/mask="$$,$$$,$$$.99"/newline
            }
        else
            {
            alt_sfpmaster:total[18] +
            alt_sfpmaster:surcharge/column=30/mask="$$,$$$,$$$.99"/newline
            }
        }
    else
        {
        uppercase(alt_scipscontrol:surcharge_wording)/column=10
        alt_sfpmaster:surcharge/column=30/mask="$$,$$$,$$$.99"/newline
        if sfpname:reinsurance = "Y" then
            box/noblanklines/noheading
            "REINSURANCE:"/column=20
            sfpreins:premium/column=30/mask="$$,$$$,$$$.99"
            end box/newline
        else
            box/noblanklines/noheadings
            ""/newline
            end box/newline
        }  
    end box/newline=2

box/noblanklines/noheadings/column=1
alt_sfsAGENT:NAME[1] + " - " + str(alt_sfsagent:agent_no)/COLUMN=1/newline
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
alt_sfsagent:TELEPHONE[1]/mask="(999) 999-9999"/COLUMN=1/newline
end box                   
end box
