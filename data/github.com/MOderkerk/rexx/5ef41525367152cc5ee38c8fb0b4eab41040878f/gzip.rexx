/* REXX */
trace 'E'
error_no = 0
CALL LOG "Lade Datei vom Host zu USS"

CALL COMPRESS "./01"

if RESULT > 0 then do
  error_no=3501
  CALL LOG "Abend Code auf 3501 gestellt"
end
exit error_no

LOG: procedure
 parse arg msg
 say msg
return 0

COMPRESS: procedure
 parse arg inputfile
 CALL LOG " Starte Komprimierung  von Datei " inputfile
 CALL "gzip -9 " inputfile
 CALL LOG " Komprimierung abgeschlossen mit RC " rc
 if rc = 0 then do
   CALL "gzip -t " inputfile'.gz'
   CALL LOG " Archiv getestet . Status " rc
  end
return rc
