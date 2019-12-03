%%{
  machine fsmatevt;

  fin = "\n" | "\r";
  sp = " ";
  num  = [0-9]*;
  tlf  = '+'?xdigit+;
  txt  = [0-9A-Z/]*;

  cpinrdy   = "READY" @{ pepe("RDY"); };
  cpinpin   = "SIM PIN";
  cpinpuk   = "SIM PUK";
  cpinnrdy  = "NOT READY" @{ pepe("NRDY"); }; 
  cpinphpin = "PH_SIM PIN";
  cpinphpuk = "PH_SIM PUK";
  cpinpin2  = "SIM PIN2";
  cpinpuk2  = "SIM PUK2";
  cpin = "+CPIN:" sp* (cpinrdy|cpinpin|cpinpuk|cpinnrdy|cpinphpin|cpinphpuk|cpinpin2|cpinpuk2);

  # +CLIP: <number>,<type>,,[,[<alpha>][,<CLI validity>]]
  clip = "+CLIP:" sp* '\"' tlf '\"' "," digit+ ",\"\",," '\"' ascii* '\"' "," digit ;

  # +CRING: <type>
  ring = "RING" sp* fin        @{ llamadaEntrante(); };
  cring= "+CRING:" sp* tlf fin @{ llamadaEntrante(); };

  # +CSQ: <rssi>,<ber>
  csq  = "+CSQ:" sp* num "," num;

  # +CREG: <n>,<stat>
  # +CREG: <n>,<stat>[,<lac>,<ci>]
  creg = "+CREG:" sp* num "," num ( "," xdigit+ "," xdigit+ )?;


  # +CRC: <mode>
  crc  = "+CRC:" sp* ("0"|"1") sp*;


#  evt = (cpin | clip | csq | creg | ring | cring) fin*;
#  main:= evt+;

  evt = (
        ( "+CME ERROR:" sp* digit+ >iniparam %finparam ) %{errorCME();}
     |  ( "+CMS ERROR:" sp* digit+ >iniparam %finparam ) %{errorCME();}
     | "+CGURC:" sp* (digit+ >iniparam %finparam) %{llamadaEntrante();}
     | "+CPIN:"  sp* (cpinrdy|cpinpin|cpinpuk|cpinnrdy|cpinphpin|cpinphpuk|cpinpin2|cpinpuk2)
     | "+CLIP:"  sp* '\"' tlf >iniparam %finparam '\"' "," digit+ ",\"\",," '\"' ascii* '\"' "," digit %{llamadaEntrante();}
     | "RING"    sp* %{ pepe("FIN RING");}
     | "+CRING:" sp* [0-9A-Za-z_ ]+ %{ pepe("FIN RING2");}
     | ("+CSQ:"|"+CSQN:")   sp* digit+ "," digit+
     | "+CREG:"  sp* (digit ",")? (digit >iniparam %finparam) ( "," xdigit+ "," xdigit+ )? %{llamadaEntrante();}
     | "+CRC:"   sp* ('0'|'1') sp* fin
     | "+CBAND:" sp* ([0-9A-Za-z_ ]+ >iniparam %finparam) %{ pepe("CBAND");}
     | "+CMGS:"  sp* (digit+ >iniparam %finparam) ("," digit+ )?  %{ pepe("Mensaje enviado id"); //Revisar esto}
     | "+CMTI:"  sp* '\"' alnum+ '\"' "," digit+




# Atendemos los OK para pasar al proceso que esté en marcha el evento
# Habrá que atender también el de error para hacer lo mismo,
#     | "OK" sp* %{recibidoOK();}

  )** fin*;


}%%
