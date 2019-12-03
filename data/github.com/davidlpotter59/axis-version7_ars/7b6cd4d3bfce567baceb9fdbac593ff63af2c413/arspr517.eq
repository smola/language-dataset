/* arspr517.eq

   February 3, 2002

   scips.com

   prints d/b outstanding receivables by Policy Number - Eff Date Aging - earned
                                                         
*/  
description 
Direct Bill Outstanding Receivalbes by Policy Number - Eff Date Aging - Earned Only;

define string l_report_title[60]=
"Direct Bill Aged Receivables - Effective Date - Earned"

define date l_ending_date = parameter/cls/prompt="Enter As of Date <NL>"

/* calc on 1/24 logic */
define number l_months_left = month(arsbilling:trans_exp) - 
                              month(l_ending_date)                              
define number l_years_left  = year(arsbilling:trans_exp) - 
                              year(l_ending_date)
define number l_months      = l_months_left + (l_years_left * 12)
define number l_term        = 12
define number l_24_factor=switch(l_months)
case 1        : 0.0417
case 2        : 0.1250
case 3        : 0.2083
case 4        : 0.2917
case 5        : 0.3750
case 6        : 0.4583
case 7        : 0.5417
case 8        : 0.6250
case 9        : 0.7083
case 10       : 0.7917
case 11       : 0.8750
case 12       : 0.9583
default       : 0.00 -- this should catch someones eye that there is an error

define signed ascii number l_net_due = arsbilling:installment_amount - (
arsbilling:total_amount_paid + arsbilling:write_off_amount)

define signed ascii number l_direct_unearned = l_24_factor * 
l_net_due/decimalplaces=2         

define signed ascii number l_direct_earned = l_net_due - l_direct_unearned
/decimalplaces=2

define unsigned ascii number l_days_past = (l_ending_date - arsbilling:trans_eff)

define signed ascii number l_days_mod = ((l_days_past div 31)) + 1
                     
define signed ascii number l_billed = if arsbilling:status = "B" then
l_direct_earned 
else
0.00

define signed ascii number l_unbilled = if arsbilling:status <> "B" then
l_direct_earned
else
0.00

define signed ascii number l_installment_charge = if arsbilling:trans_code 
one of 18, 28 then l_billed else
0.00

define signed ascii number l_premium_fee = if arsbilling:trans_code one of 19
,29 then
l_billed
else
0.00

define signed ascii number l_total = l_billed + l_unbilled 

define signed ascii number l_billed_comm = l_billed * (arsbilling:comm_rate 
* 0.01) * 1.00 -- 1.00 to force rounding

define signed ascii number l_unbilled_comm = l_unbilled * (
arsbilling:comm_rate * 0.01) * 1.00 -- 1.00 to force rounding

define signed ascii number l_current = if l_days_mod =< 1 then
l_billed
else
0.00

define signed ascii number l_31_60 = if l_days_mod = 2 then
l_billed 
else
0.00

define signed ascii number l_61_90 = if l_days_mod = 3 then
l_billed
else
0.00

define signed ascii number l_91_plus = if l_days_mod = 4 then
l_billed 
else
0.00

define string l_printed_by ="Printed by: " + trun(username)

define string l_report_no = "ARSPR517"  

where arsbilling:status <> "P" and
(arsbilling:due_date <= l_ending_date and
 arsbilling:trans_eff <= l_ending_date)

list
/nobanner
/domain="arsbilling"
/pagewidth=250
/nopageheadings
/nodetail

box/noblanklines 
if l_net_due <> 0 then
{
    arsbilling:policy_no/column=1
    arsbilling:trans_date/column=12
    arsbilling:trans_eff/column=24
    l_installment_charge/heading="Installment-Charges"/column=36
    l_premium_fee/heading="Premium-Fees"/column=56
    l_unbilled/heading="Total-Unbilled"/column=76
--    l_unbilled_comm/heading="Unbilled-Commission"/column=96
--    l_billed_comm/heading="Billed-Commission"/column=116
    l_current/heading="0 to 30-Days Past"/column=96
    l_31_60/heading="31 to 60-Days Past"/column=116
    l_61_90/heading="61 to 90-Days Past"/column=136
    l_91_plus/heading="91+ Days-Past" /column=156
    l_billed/heading="Total-Billed"/column=176
}
end box
            
sorted by arsbilling:policy_no 

top of page
include "report1.pro"             
l_printed_by/column=1/noheading
l_ending_date/heading="As of "/column=105/newline=2/mask="M(15) D(2), Y(4)"

end of arsbilling:policy_no
box/noblanklines
if total[l_net_due, arsbilling:policy_no] <> 0 then
{
    arsbilling:policy_no/noheading/column=1
    arsbilling:trans_date/noheading/column=12
    arsbilling:trans_eff/noheading/column=24
    total[l_installment_charge,arsbilling:policy_no]/column=36
    total[l_premium_fee,arsbilling:policy_no]/column=56
    total[l_unbilled,arsbilling:policy_no]/column=76
--    total[l_unbilled_comm,arsbilling:policy_no]/column=96
--    total[l_billed_comm,arsbilling:policy_no]/column=116
    total[l_current,arsbilling:policy_no]/column=96
    total[l_31_60,arsbilling:policy_no]/column=116
    total[l_61_90,arsbilling:policy_no]/col=136
    total[l_91_plus,arsbilling:policy_no]/column=156
    total[l_billed,arsbilling:policy_no]/column=176
}
end box
