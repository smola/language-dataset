/*  arspr01a.eq

    scips.com

    September 11, 2001
    copied from arspr001.eq - september 11, 2001

    program to print direct bill invoices
    prints using pcl5 and cq programming
    can not be viewed in standard cq
    ALL COPIES - uses arsfinder2 instead of arsfinder

    changed 10/11/2001 to pull from tray 2 of an HP
    8100 series laser jet

Company List
============
Farmers Mutual Insurance Company of Salem County
White Hall Mutual Insurance Company
Golden Bear Insurance Company
SCIPS.com - DEMO systems

april 22, 2001 -- added logic to print payments received since last bill
July 14, 2001 -- added logic to reverse arspayor:name[1] on the invoice
*/

include "startend.inc"               
define string l_installment_literal[20]=switch(arsbilling:billing_ctr)
/* case 1  : "First Installment     "
case 2  : "Second Installment    "
case 3  : "Third Installment     "
case 4  : "Forth Installment     "
case 5  : "Fifth Installment     "
case 6  : "Sixth Installment     "
case 7  : "Seventh Installment   "
case 8  : "Eighth Installment    "
case 9  : "Nineth Installment    "
case 10 : "Tenth Installment     "
case 11 : "Eleventh Installment  "
case 12 : "Twelfth Installment   "
case 13 : "Thirteenth Installment" */
case 99 : " ** Installment **    "
default : "    Installment       "

define signed ascii number l_total_due_total = arsfinder2:annual_premium 

define string l_transaction_type[20]=if arsbilling:payment_plan = 1 then
"Annual Payment" else
if arsbilling:payment_plan <> 1 then 
l_installment_literal

define string I_name[50]=sfpname:name[1]/toggle;
define string I_name_3[50]=arspayor:name[1]/toggle;
define string l_agent_zipcode = str(sfsagent:zipcode,99999-9999)

include "renaeq1.inc"                       
include "renaeq3.inc" --aspayor:name[1] reversing for invoice bottom

define signed ascii number l_total_premium = if arsbilling:status = "B" then
arsbilling:installment_amount - 
arsbilling:total_amount_paid/decimalplaces=2

define signed ascii number l_total_past_due = 
 if arsbilling:status = "B" and
    arsfinder2:due_date < l_starting_date then 
    arsbilling:installment_amount - arsbilling:total_amount_paid  
else
    0.00

define signed ascii number l_current_total_due = 
--if arsfinder2:due_date => l_starting_date and 
--   arsfinder2:due_date <= l_ending_date and
 if arsbilling:status = "B" then 
    arsbilling:installment_amount - 
    arsbilling:total_amount_paid  
else
    0.00

define signed ascii number l_total_due =
    l_current_total_due + l_total_past_due 

define signed ascii number l_current_total_due_copy = if
arsfinder2:copy_ctr = 1 then 
l_current_total_due 
else
0.00/decimalplaces=2

define signed ascii number l_total_past_due_copy = if
arsfinder2:copy_ctr = 1 then
l_total_past_due
else
0.00/decimalplaces=2

define signed ascii number l_total_due_copy = if
arsfinder2:copy_ctr = 1 then
l_total_due
else
0.00/decimalplaces=2

/* message variables */
define signed ascii number l_message_premium[9]=0/decimalplaces=2
define unsigned ascii number l_message_installment[4]=0/decimalplaces=0
define unsigned ascii number l_message_annual_installment[4]=0/decimalplaces=
0

define string l_message_1a[70] = trun(replace(arscontrol:invoice_message_array[1],
"^",str(l_total_due_total,"-$ZZ,ZZZ.99")))
define string l_message_2a[70] = trun(replace(arscontrol:invoice_message_array[2],
"^",str(l_total_due_total,"-$ZZ,ZZZ.99")))
define string l_message_3a[70] = trun(replace(arscontrol:invoice_message_array[3],
"^",str(l_total_due_total,"-$ZZ,ZZZ.99")))
define string l_message_4a[70] = trun(replace(arscontrol:invoice_message_array[4],
"^",str(l_total_due_total,"-$ZZ,ZZZ.99")))
define string l_message_5a[70] = trun(replace(arscontrol:invoice_message_array[5],
"^",str(l_total_due_total,"-$ZZ,ZZZ.99")))
define string l_message_6a[70] = trun(replace(arscontrol:invoice_message_array[6],
"^",str(l_total_due_total,"-$ZZ,ZZZ.99")))

define string l_message_1b[70] = replace(l_message_1a,
"@",str(arspayplan:installment_charge_rate,"$ZZ"))
define string l_message_2b[70] = replace(l_message_2a,
"@",str(arspayplan:installment_charge_rate,"$ZZ"))
define string l_message_3b[70] = replace(l_message_3a,
"@",str(arspayplan:installment_charge_rate,"$ZZ"))
define string l_message_4b[70] = replace(l_message_4a,
"@",str(arspayplan:installment_charge_rate,"$ZZ"))
define string l_message_5b[70] = replace(l_message_5a,
"@",str(arspayplan:installment_charge_rate,"$ZZ"))
define string l_message_6b[70] = replace(l_message_6a,
"@",str(arspayplan:installment_charge_rate,"$ZZ"))

define string l_message_1[70] = l_message_1b
define string l_message_2[70] = l_message_2b
define string l_message_3[70] = l_message_3b
define string l_message_4[70] = if arspayplan:number_of_payments = 1 or
arsfinder2:billing_ctr > 1 then 
" " 
else l_message_4b
define string l_message_5[70] = if arspayplan:number_of_payments = 1 or 
arsfinder2:billing_ctr > 1 then 
" "
else l_message_5b
define string l_message_6[70] = if arspayplan:number_of_payments = 1 or 
arsbilling:billing_ctr > 1 then 
" "
else l_message_6b

define signed ascii number l_total_ann_premium = if arsfinder2:trans_code <> 18 
and arsfinder2:trans_code <> 28 then
arsbilling:installment_amount 
else
0.00/decimalplaces=2

include "fonts.var"       
where arsfinder2:policy_no = 810100817
list
/nobanner
/domain="arsfinder2"
/nodefaults 
/nopageheadings 
/nodetail 
/notitle 
/pagelength=0
/pagewidth=255        
--/maxpages=30
      
sorted by 
          arsfinder2:policy_no/newpage 
          arsfinder2:copy_ctr /newpage
          arsfinder2:due_date         
          arsfinder2:trans_code
           
top of report 
l_reset      
l_simplex 
l_tray_2 -- added 10/11/2001

top of page    
/* print the Invoice Literal here */
""/newline
l_arial_italic_bold_16
l_tray_2  -- added 10/11/2001

"<033>&a2160h120V"
"I N V O I C E"

"<033>&a4700h80V"
l_arial_bold
arsfinder2:copy_literal/noheading
""/newline
"<033>&a+0h-120V"

/*  draw verticle lines */
-- changed uly 15, 2001 from 2960
"<033>&a360h120V<033>*c5a2950b0P" -- left verticle line, entire length
"<033>&a5460h120V<033>*c5a2950b0P" -- right verticle line, entire length
                       
/* draw horizontal lines to complete box */
"<033>&a360h120V<033>*c5100h5v0P"  -- top horizontal line
--"<033>&a360h7460V<033>*c5100h5v0P" -- bottom horizontal line
"<033>&a360h7200V<033>*c5100h5v0P" -- bottom horizontal line April 1, 2001

"<033>&a360h1260V<033>*c5100h5v0P" -- 1st line from top
"<033>&a360h1440V<033>*c5100h5v0P" -- 2nd line from top
"<033>&a360h1620V<033>*c5100h5v0P" -- 3rd line from top
"<033>&a360h5400V<033>*c5100h5v0P" -- 4th line from top

""/newline -- clear CQCS' buffer here

"<033>&a360h4590V<033>*c180h5v0P" -- fold tick mark

/* position small verticle lines */
"<033>&a1060h1440V<033>*c5a75b0P" -- 1st verticle line on top
--"<033>&a900h1440V<033>*c5a75b0P"  -- 1st verticle line on top
"<033>&a1800h1440V<033>*c5a75b0P" -- 2nd verticle line on top
"<033>&a3680h1440V<033>*c5a75b0P" -- 3rd verticle line on top
"<033>&a4660h1440V<033>*c5a75b0P" -- 4th verticle line on top

""/newline -- clear CQCS' buffer here

"<033>&a3132h5760V<033>*c2320h5v0P" -- bottom of pay this amount box
"<033>&a3132h5400V<033>*c5a150boP"  -- left hand line of pay this amount box

""/newline -- clear CQCS' buffer here

/* start of fixed literals */        
l_arial
--"<033>&a360h1230V"
"<033>&a3420h1110V"
"Policy Type:"/newline
"<033>&a360h1410V"
--"<033>&a2160h+0V"
"Run Date:"
"<033>&a2620h+0V"
"Payment Plan:"

""/newline -- clear CQCS' buffer here
"<033>&a360h1590V"
"Trans Date"
"<033>&a+180h+0V"
"Trans Eff"
"<033>&a+940h+0V"
"Description"
"<033>&a+900h+0V"
"Due Date"  
"<033>&a+360h+0V"
"Amount Due"      

-- 15% shading (15g)
--"<033>&a360h1440V<033>*c2125a75b15g2P"

-- 5% shading (5g)
--"<033>&a360h1440V<033>*c2125a75b5g2P" 

-- 10% shading (10g)
"<033>&a360h1440V<033>*c2125a75b10g2P"

l_arial_bold_14
"<033>&a3200h5640V"
"Pay This Amount:" 

l_arial 
""/newline -- clear CQCS' buffer here

/* begin arscontrol literals */

"<033>&a560h5200V"
arscontrol:invoice_payto/noheading

"<033>&a2880h+0V"
arscontrol:invoice_company/noheading 

"<033>&a2880h+120V"
arscontrol:invoice_slogan/noheading 

"<033>&a360h5520V"
arscontrol:invoice_seperator/noheading 

""/newline -- clear CQCS' buffer here

"<033>&a2880h5940V"
arscontrol:invoice_payment_options/noheading 
                                      
l_arial_italic_12 
"<033>&a2880h+120V"
arscontrol:invoice_closing/noheading 
l_arial  

""/newline  -- clear CQCS' buffer here

/* this is the message lines, if any.  These were put here
   instead of in their actual order since this information
   may not be inserted. If no value is put in the field in
   arscontrol then this will not print  
*/

if arscontrol:invoice_message <> "" then
   {
      "<033>&a720h4320V"
      l_message_1/noheading 
      ""/newline
      "<033>&a+0h-120V"
      "<033>&a720h+120V"
      l_message_2/noheading
      ""/newline
      "<033>&a+0h-120V"
      "<033>&a720h+120V"
      l_message_3/noheading
      ""/newline
      "<033>&a+0h-120V"
      "<033>&a720h+120V"
      l_message_4/noheading
      ""/newline
      "<033>&a+0h-120V"
      "<033>&a720h+120V"
      l_message_5/noheading
      ""/newline
      "<033>&a+0h-120V"
      "<033>&a720h+120V"
      l_message_6/noheading
  }

/* print the company that is in the arscontrol file */

""/newline

l_arial 
"<033>&a360h5640V"
sfscompany:name[1]
"<033>&a360h+120V"
if sfscompany:name[2] <> "" then
{
  trun(sfscompany:name[2])/newline
  "<033>&a360h+0V"
}
if sfscompany:name[3] <> "" then
{
  trun(sfscompany:name[3])/newline
  "<033>&a360h+0V"
}                         
if sfscompany:address[1] <> "" then
{ 
  "<033>&a360h+0V"  
  trun(sfscompany:address[1])/newline
  "<033>&a360h+0V" 
}
if sfscompany:address[2] <> "" then 
{
 trun( sfscompany:address[2])/newline 
  "<033>&a360h+0V"
}
if sfscompany:address[3] <> "" then
{
  trun(sfscompany:address[3])/newline 
  "<033>&a360h+0V"
}          

""/newline 
"<033>&a360h-120V"
trun(sfscompany:city) + ", " + sfscompany:str_state 
sfscompany:zipcode/mask="99999-9999"/newline

""/newline -- clear CQCS' buffer now

/*  this is the end of the "NON" data portion of the invoice" */

top of arsfinder2:copy_ctr 

/* print insured's name and address */

l_arial_bold
"<033>&a460h240V"
"Insured"
l_arial 
""/newline
"<033>&a+0h-120V"
"<033>&a360h+120V"
trun(sfsline:alpha + str(arsbilling:policy_no))/noheading

--l_arial_8            
""/newline
"<033>&a360h+0V"
i_rev_name/noheading 
  
""/newline
"<033>&a+0h-120V"

if sfpname:name[2] <> "" then 
{                 
    ""/newline
    "<033>&a360h+0V"
    sfpname:name[2]/noheading 
}                 

""/newline
"<033>&a+0h-120V"

if sfpname:name[3] <> "" then
{
    ""/newline
    "<033>&a360h+0V"
    sfpname:name[3]
}

""/newline
"<033>&a+0h-120V"

if sfpname:address[1] <> "" then
{
    ""/newline
    "<033>&a360h+0V"
    sfpname:address[1]
}

""/newline
"<033>&a+0h-120V"

if sfpname:address[2] <> "" then
{
    ""/newline
    "<033>&a360h+0V"
    sfpname:address[2]
}

""/newline
"<033>&a+0h-120V"

if sfpname:address[3] <> "" then
{
    ""/newline
    "<033>&a360h+0V"
    sfpname:address[3]
}

""/newline
"<033>&a360h+0V"

trun(sfpname:city + ", " + sfpname:str_state)/noheading 
--if sfpname:zipcode[1,4]=0000 then
--    sfpname:zipcode[5,9]
--else
    sfpname:zipcode/mask="99999-9999"

/* end of Policy Name and Address Information */

""/newline  -- clear CQCS' buffer here

/*  Print Agents name, address and Phone Number */

l_arial_bold
"<033>&a3520h240V"
"Agent: "            
sfsagent:agent_no/mask="ZZZZ"
l_arial
""/newline
"<033>&a+0h-120V"

"<033>&a3420h+120V"


sfsagent:name[1]/noheading

"<033>&a3420h+120V"

if sfsagent:name[2] <> "" then
{
    sfsagent:name[2]/noheading 
}
else
{
    sfsagent:address[1]/noheading 
}

"<033>&a3420h+120V"
if sfsagent:name[2] <> "" then
{
    sfsagent:address[1]/noheading 
}
else
{
    trun(sfsagent:city + ", " + sfsagent:str_state) + " " + 
sfsagent:str_zipcode
}

"<033>&a3420h+120V"
if sfsagent:name[2] <> "" then
{
    trun(sfsagent:city + ", " + sfsagent:str_state) + " " + 
sfsagent:str_zipcode
}
else
{   
   sfsagent:telephone[1]/noheading 
}

"<033>&a3420h+120V"
if sfsagent:name[2] <> "" then
{
    sfsagent:telephone[1]/noheading 
}
                   
/* end of Agent name and address */

""/newline  -- clear CQCS' buffer now
            
/* various other information */

""/newline
l_arial 
--"<033>&a972h1410V" 
--"<033>&a972h1230V"
"<033>&a3420h1230V"
sfsline:description/noheading 

--"<033>&a2650h+0V"
"<033>&a972h1410V"
todaysdate/mask="MM/DD/YYYY"/noheading 

"<033>&a+1900h+0V"
arspayplan:description/noheading 
             
""/newline --  clear CQCS' buffer here
"<033>&a360h1540V"

if arsfinder2:previous_payment_amount[1] <> 0 then
{
-- courier
--"<033>(s0p12h12v0s0b3T" -- changed 06/03/2001
l_letter_gothic_12
"<033>&a360h+180V"
arsfinder2:previous_payment_check_date
"<033>&a1750h+0V"
"Payment Received - Thank you!"/newline
"<033>&a+0h-120V"
"<033>&a4636h+0V"
arsfinder2:previous_payment_amount/width=11/mask="$ZZ,ZZZ.99-"
}

-- *******************************************
-- ***  added this end of stmt 3/14/2001   ***
-- ***  each prscode description should    ***
-- ***  print independent of eachother     ***
-- *******************************************

end of arsfinder2:trans_code 
-- courier
-- l_courier -- changed 06/03/2001 
l_letter_gothic_12

"<033>&a360h+60V"
-- august 29, 2001 dlp do not print 0 amounts due
if total[l_total_premium, arsfinder2:trans_code] > 0 then
{
arsbilling:trans_date/noheading/mask="MM/DD/YYYY"
"<033>&a+10h+0V"
arsbilling:trans_eff/noheading/mask="MM/DD/YYYY"
"<033>&a1750h+0V"

if arsfinder2:trans_code <> 18 and
   arsfinder2:trans_code <> 19 then 
   {l_transaction_type}
else
   {arscode:description/noheading}

"<033>&a3780h+0V"
arsfinder2:due_date/noheading/mask="MM/DD/YYYY"
"<033>&a4636h+0V"

total[l_total_premium,arsfinder2:trans_code]/width=11/noheading/mask="$ZZ,ZZZ.99-"
/newline 
--arsbilling:installment_amount/noheading/mask="ZZZ,ZZZ.99-"/newline
}

""/newline  -- clear CQCS' buffer here
"<033>&a360h-300V"
-- *******************************

end of arsfinder2:due_date 
--""/newline 
--"<033>&a360h1740V"
"<033>&a360h+90V"
/* arsbilling:trans_date/noheading/mask="MM/DD/YYYY"
"<033>&a+180h+0V"
arsbilling:trans_eff/noheading/mask="MM/DD/YYYY"
"<033>&a+180h+0V"
*/
--prscode:description/noheading                  
"<033>&a1500h+0V"

-- august 29, 2001 dlp do not print 0 amount due

if total[l_total_premium, arsfinder2:due_date] > 0 then
{
"Total for Due Date"
"<033>&a3780h+0V"
arsfinder2:due_date/noheading/mask="MM/DD/YYYY"
"<033>&a4636h+0V"                        
-- *******************************************************
-- ***  changed this line from arsbilling:policy_no to ***
-- ***  arsfinder2:due_date, it is afterall the end of ***
-- ***  due date stmt not end of policy                ***
-- *******************************************************
total[l_total_premium,arsfinder2:due_date]/noheading/mask="$ZZ,ZZZ.99-"
--arsbilling:installment_amount/noheading/mask="ZZZ,ZZZ.99-"/newline
}

""/newline  -- clear CQCS' buffer here
"<033>&a360h-120V"

end of arsfinder2:copy_ctr 

-- l_arial_bold_12  -- changed 06/03/2001
l_letter_gothic_bold_12


"<033>&a2380h+240V"
-- august 29, 2001 dlp do not print 0 amount due
if total[l_total_due, arsfinder2:copy_ctr] <> 0 then
{
"Total Due "
"<033>&a4636h+0V"
total[l_total_due,arsfinder2:copy_ctr]/noheading/mask="$ZZ,ZZZ.99-"
--total[arsbilling:installment_amount,arsfinder2:copy_ctr]/noheading/mask=
--"$$Z,ZZZ.99-"
"<033>&a2380h+360V"
}

l_arial     

""/newline -- clear CQCS' buffer here

/* invoice polname name information 
   print insured's name and address */

l_arial 
           
/* july 22, 2001 - changed the 1200h to 480h - dlp */
/* july 22, 2001 - added one more line between policy number 
   and the name +240V instead of +120V */

"<033>&a480h6290V"
 
trun(sfsline:alpha) + str(arspayor:policy_no)/noheading
"<033>&a480h+240V"

--trun(arspayor:name[1])
-- changed 07/14/2001 - to reverse the payor name 
TRUN(i_name_3[(POS("=",I_NAME_3)+1),
     LEN(I_NAME_3)])+" "+TRUN(I_NAME_3[0,(POS("=",I_NAME_3)-1)])

""/newline
"<033>&a480h-30V"

if arspayor:name[2] <> "" then
{
"<033>&a480h+0V"
trun(arspayor:name[2])
""/newline
"<033>&a480h-30V"
}
 
if arspayor:name[3] <> "" then
{
"<033>&a480h+0V"
trun(arspayor:name[3])
""/newline
"<033>&a480h-30V"
}

if arspayor:address[1] <> "" then
{      
"<033>&a480h+0V"
trun(arspayor:address[1])
""/newline
"<033>&a480h-30V"
}
           
""/newline
"<033>&a480h-120V"

if arspayor:address[2] <> "" then
{
"<033>&a480h+0V"
trun(arspayor:address[2])
""/newline
"<033>&a480h-30V"
}

if arspayor:address[3] <> "" then
{
"<033>&a480h+0V"
trun(arspayor:address[3])
""/newline
"<033>&a480h-30V"
}
""/newline
"<033>&a480h-120V"
trun(arspayor:city) + ", " + arspayor:str_state 
arspayor:zipcode/mask="99999-9999" 
                                                      
/* end of Policy Name and Address Information */

"<033>&a360h6300V" -- placing this here to put the cursor back to where it
                   -- started from

""/newline  -- clear CQCS' buffer here

/* print the amount due */
            
l_arial_bold_14 
""/newline
"<033>&a4560h5640V"
total[l_total_due,arsfinder2:copy_ctr]/mask="$ZZ,ZZZ.99-"/noheading 

""/newline  --  clear CQCS' buffer here

/*  this prints a totals page */
end of report
""/newpage 
"<033>&a360h120V<033>*c5a2950b0P" -- left verticle line, entire length
"<033>&a5460h120V<033>*c5a2950b0P" -- right verticle line, entire length
                       
/* draw horizontal lines to complete box */
"<033>&a360h120V<033>*c5100h5v0P"  -- top horizontal line
--"<033>&a360h7460V<033>*c5100h5v0P" -- bottom horizontal line
"<033>&a360h7200V<033>*c5100h5v0P" -- bottom horizontal line April 1, 2001

"<033>&a360h1260V<033>*c5100h5v0P" -- 1st line from top
"<033>&a360h1440V<033>*c5100h5v0P" -- 2nd line from top
"<033>&a360h1620V<033>*c5100h5v0P" -- 3rd line from top
"<033>&a360h5400V<033>*c5100h5v0P" -- 4th line from top

""/newline -- clear CQCS' buffer here

"<033>&a360h4590V<033>*c180h5v0P" -- fold tick mark

/* position small verticle lines */
"<033>&a1060h1440V<033>*c5a75b0P" -- 1st verticle line on top
--"<033>&a900h1440V<033>*c5a75b0P"  -- 1st verticle line on top
"<033>&a1800h1440V<033>*c5a75b0P" -- 2nd verticle line on top
"<033>&a3680h1440V<033>*c5a75b0P" -- 3rd verticle line on top
"<033>&a4660h1440V<033>*c5a75b0P" -- 4th verticle line on top

""/newline -- clear CQCS' buffer here

"<033>&a3132h5760V<033>*c2320h5v0P" -- bottom of pay this amount box
"<033>&a3132h5400V<033>*c5a150boP"  -- left hand line of pay this amount box

""/newline -- clear CQCS' buffer here
                   
l_letter_gothic_bold_12
"<033>&a1440h2520V"
"Totals to be balanced against arspr018"
""/newline
"<033>&a1440h+120V"
"Total Current Amount Due"
"<033>&a4100h+0V"
total[l_current_total_due_copy]/mask="$$,$$$,$$$.99-"/noheading 
""/newline
"<033>&a1440h+120V"                                        
"Total Amount Past Due"
"<033>&a4100h+0V"
total[l_total_past_due_copy]/mask="$$,$$$,$$$.99-"/noheading 
""/newline
"<033>&a1440h+120V"                                     
"Total Amount Due"
"<033>&a4100h+0V"
total[l_total_due_copy]/mask="$$,$$$,$$$.99-"/noheading
""/newline
l_letter_gothic_12
"<033>&a1440h+240V"
l_starting_date/heading="From "/mask="MM/DD/YYYY"
l_ending_date/heading=" - "/mask="MM/DD/YYYY"
""/newline                                   
"<033>&a1440h+120V"
username/heading="Printed By"
