/* cpppr400ab.eq
  
   scips.com

   March 28, 2003

   crime worksheet
*/
                                                           
where cppcrime:policy_no = sfpprint9:policy_no and
      cppcrime:pol_year = sfpprint9:pol_year and
      cppcrime:end_sequence = sfpprint9:end_sequence and
      cppcrime:prem_no = sfpprint9:prem_no and
      cppcrime:build_no = sfpprint9:build_no and
      cppcrime:crime_form one of "A", "B"
list
/domain="cppcrime"
/nobanner
/nopageheadings
/nototals
/duplicates               
/noheadings

""/newline=2     
if cppcrime:crime_form one of "A", "B" then
    {                                
    if cppcrime:crime_form one of "A" then
        {
        "EMPLOYEE DISHONESTY"/newline=2/column=1
        }
    else 
        {
        if cppcrime:crime_form one of "B" then
            "FORGERY OR ALTERATIONS"/newline=2/column=1
        }
    "Limit:"
    cppcrime:limit[1]/newline/column=15
    "Deductible:"
    cppcrime:deductible/newline/column=20 
    "No Employees:"
    cppcrime:employees/newline=2/column=20
     
    "Charge:"
    cppcrime:employee_dishonesty_charge[1]/column=23/mask="Z99.99"/newline
    "Each Addl:"
    cppcrime:employee_dishonesty_charge[3]/column=10
    "*"/column=15
    cppcrime:employees_over/column=17
    "="
    cppcrime:employee_dishonesty_charge[2]/mask="Z99.99"/newline
    "Deductible:"/column=40
    cppcrime:deductible_factor/newline/column=56
    "Forgery Factor:"/column=40
    cppcrime:forgery_factor/newline
 
    "Premium:"
    cppcrime:crime_premium/column=19/newline

    "Co Deviation:"/column=1
    if cpsdefault:deviation_number_not_factor = 1 and
       cppcrime1:company_deviation not one of 1.00 then
        {
        (1 + (cppcrime1:company_deviation * 0.01))/column=19
        }
    else      
        {
        cppcrime1:company_deviation/column=22
        }
    ""/newline

    "PMA Factor:"/column=1
    if cpsdefault:deviation_number_not_factor = 1 and
       cppcrime1:package_mod not one of 1.00 then
        {
        (1 + (cppcrime1:package_mod * 0.01))/column=19
        }
    else 
        {
        cppcrime1:package_mod/column=22
        }
    ""/newline

    "Total Premium:"
    cppcrime1:premium/column=19/newline/mask="ZZZ,ZZZ.99"
    }

sorted by cppcrime:prem_no
          cppcrime:build_no                                               
