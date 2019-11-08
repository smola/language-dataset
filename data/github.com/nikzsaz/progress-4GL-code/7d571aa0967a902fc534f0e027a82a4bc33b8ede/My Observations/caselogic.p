define var v-deci as deci no-undo init 42.9.
define var acceptval as logi no-undo init false.
      
/** whatever be the condition it will come inside**/
case true:
    when acceptval and (v-deci >= 20 and v-deci <= 60)
    then do:
        disp "value is between 20 and 60".
     end.
     when v-deci > 20 and v-deci < 60
     then do:
        disp "acceptval is set to false".
     end.
     when v-deci > 60
     then do:
        disp "value is greater than 60".
     end.
     otherwise do:
        disp "none of the condition matches".
     end.
end case. 
