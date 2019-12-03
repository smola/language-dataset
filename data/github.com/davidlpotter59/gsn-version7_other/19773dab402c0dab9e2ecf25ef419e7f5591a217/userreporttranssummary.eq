/* userreporttranssummary.eq

   scips.com

   April 5, 2002

   report listing what rater did what policy by trans date
*/

description Rater Policy Listing ;

define date l_starting_date = parameter/prompt="Enter Starting Date: "
            l_ending_date = parameter/prompt="Enter Ending Date: "

define file alt_sfpmaster = access sfpmaster, set
sfpmaster:policy_no= sfpname:policy_no,
sfpmaster:pol_year = sfpname:pol_year,
sfpmaster:end_sequence= sfpname:end_sequence 
                  
where sfpname:trans_date >= l_starting_date and
      sfpname:trans_date <= l_ending_date
list
/domain="sfpname"
/nobanner                
/nodetail

sfpname:policy_no
sfpname:pol_year
sfpname:end_sequence       
sfpmaster:trans_code
sfpname:convert_date
sfpname:convert_time

sorted by sfpname:user_id/newpage
          sfpmaster:trans_code
          sfpmaster:policy_no
         
end of sfpname:user_id
""/newline
count[sfpname:policy_no]/heading="Total Number Of Policies By User"
                                       
end of sfpmaster:trans_code
""/newline
box/noblanklines/noheadings/squeeze 
"Total Number Of Policies For Trans Code" 
sfpmaster:trans_code/noheading/mask="ZZZZ:"
end box
count[sfpname:policy_no]

top of page
sfpname:user_id/heading="Rater"/newline

end of report                                              
""/newline
count[sfpname:policy_no]/heading="Total Number Of Policies"
