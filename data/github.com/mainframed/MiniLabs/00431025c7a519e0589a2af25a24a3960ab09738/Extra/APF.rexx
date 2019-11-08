/* REXX */
/* Thanks to FILE221 on CBT Tape for mapping this out */
/* Lists APF Authorized datasets */
numeric digits 20 /* need this for D2X/C2D  */
CVT = C2D(STORAGE(10,4)) /* CVT Pointer */
CVTAUTHL = C2D(STORAGE(D2X(CVT + 484),4))
IF CVTAUTHL = C2D('7FFFF001'x) THEN DO
 /* The APF Table is Dynamic, not Static */
 CVTECVT = C2D(STORAGE(D2X(CVT + 140),4))
 ECVTCSVT = C2D(STORAGE(D2X(CVTECVT + 228),4))
 APF_PTR = c2d(storage(d2x(ECVTCSVT+12),4))
 CUR = c2d(storage(d2x(APF_PTR+8),4))
 LAST = c2d(storage(d2x(APF_PTR+12),4))
 DO FOREVER
  DATASET = storage(d2x(CUR+24),44)
  IF SUBSTR(DATASET,1,1) \= '00'x THEN DO /* Its not deleted */
   VOL_SMS = storage(d2x(CUR+4),1)
   IF bitand(VOL_SMS,'80'x) = '80'x then VOLUME = 'SMS   '
   ELSE VOLUME = STORAGE(D2X(CUR+68),6)
   /* Here we list the found items */
   SAY strip(VOLUME) STRIP(DATASET)
  END
  IF CUR = LAST THEN LEAVE
  ELSE CUR = C2D(STORAGE(D2X(CUR+8),4))
 END /* Do */
END /* End of Dynamic */
