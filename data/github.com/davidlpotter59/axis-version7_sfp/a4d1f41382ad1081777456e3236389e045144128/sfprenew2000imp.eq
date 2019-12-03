/* sfprenew2000imp.eq

   November 5, 2007

   scips.com

   description==program to select policies to be renewed 

   this program will select the policies that have been made manual within  
   the 120 days of the expiration date or has within 120 days have failed   
   for a loss amount or # of losses.  Once these have been selected then the 
   file will be saved as a flat file and is used as input to the
   next program, which is a batch program, sfprenew200.  This program
   will do the error checking, by location, as well as by policy

*/      

find sfprenewsus with 
     sfprenewsus:code = "P" and
     sfsline:lob_code = "PINLAND"                -- all passed 
     
