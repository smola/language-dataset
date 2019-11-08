/* REXX - Number1                         */
/* Sample rexx to test Brightside         */
/* Argument required in format aaaammdd   */
/* timestamp - 2019-08-19T14:30:15.289000 */
arg bdate
user = userid()
call check_bdate
call load_uk
call find_uk
/* [dxr] 
call load_us  
call find_us 
 [dxr] */
exit

/* calls --------------------------------------------------*/
check_bdate:
  if bdate = '' then do
     say 'Empty date, please write in format aaaammdd'
     exit(8)
  end
  bd_year  = substr(bdate,1,4)
  bd_month = substr(bdate,5,2)
  bd_day   = substr(bdate,7,2)

  /* We assume there is date validation */
return

load_uk:
   uk_master = user||'.N1UK.MASTER'
   uk_work   = user||'.N1UK.WORK'
   x =  sysdsn("'"uk_master"'")
   if x =translate('ok') then do
      "alloc da('"uk_master"') fi(ukmaster) shr reu"
      "execio * diskr ukmaster (stem ukmaster. finis)"
      "free fi(ukmaster)"

      /* clean & sort the masterfile */
      cont = 0
      drop uklist.
      junk = outtrap(line.)
      "delete '"uk_work"'"
      junk = outtrap('off')
      "alloc new space(1 5) cyl fi(ukwork) dsorg(ps) recfm(f b) ",
      "lrecl(125) blksize(0) reu da('"uk_work"')"
      do i = 1 to ukmaster.0
         if index(ukmaster.i,';') = 0 then iterate
         if index(ukmaster.i,'DATE;TITLE') > 0 then iterate
         parse var ukmaster.i day    '/' ,
                              month  '/' ,
                              year   ';' ,
                              title  ';' ,
                              artist ';' ,
                              .
         cont = cont + 1
         uklist.cont = year||month||day||';'||title||';'||artist
      end /* do */
      "execio * diskw ukwork (stem uklist. finis)"
      "free fi(ukwork)"

      /* sort */
      drop sysin.
      sysin.1 =" SORT FIELDS=(1,8,CH,A)"

      "alloc fi(sysin)  lrecl(80) blksize(0) recfm(f b) ",
             "cylinders space(1,1) unit(sysda) new reu"
      "execio * diskw sysin  (stem sysin. finis)"

      "alloc dd(sysout) new reu unit(sysda) recfm(f b) lrecl(80)"

      "alloc fi(sortin)  da('"uk_work"') shr reuse"
      "alloc fi(sortout) da('"uk_work"') shr reuse"

      address linkmvs iceman
      "free fi(sysout sortin sortout sysin)"
   end
   else do
      say 'No master UK file'
   end /* if x */
return

load_us:
   us_master = user||'.N1US.MASTER'
   us_work   = user||'.N1US.WORK'
   x =  sysdsn("'"us_master"'")
   if x =translate('ok') then do
      "alloc da('"us_master"') fi(usmaster) shr reu"
      "execio * diskr usmaster (stem usmaster. finis)"
      "free fi(usmaster)"

      /* clean & sort the masterfile */
      cont = 0
      drop uslist.
      junk = outtrap(line.)
      "delete '"us_work"'"
      junk = outtrap('off')
      "alloc new space(1 5) cyl fi(uswork) dsorg(ps) recfm(f b) ",
      "lrecl(125) blksize(0) reu da('"us_work"')"
      do i = 1 to usmaster.0
         parse var usmaster.i        '"' ,
                              title  '", ',
                              artist ';' ,
                              month  '/' ,
                              day    '/' ,
                              year   ';' ,
                              .
         day   = right(day,2,'0')
         month = right(month,2,'0')
         cont = cont + 1
         uslist.cont = year||month||day||';'||title||';'||artist
      end /* do */
      "execio * diskw uswork (stem uslist. finis)"
      "free fi(uswork)"

      /* sort */
      drop sysin.
      sysin.1 =" SORT FIELDS=(1,8,CH,A)"

      "alloc fi(sysin)  lrecl(80) blksize(0) recfm(f b) ",
             "cylinders space(1,1) unit(sysda) new reu"
      "execio * diskw sysin  (stem sysin. finis)"

      "alloc dd(sysout) new reu unit(sysda) recfm(f b) lrecl(80)"

      "alloc fi(sortin)  da('"us_work"') shr reuse"
      "alloc fi(sortout) da('"us_work"') shr reuse"

      address linkmvs iceman
      "free fi(sysout sortin sortout sysin)"
   end
   else do
      say 'No master US file'
   end /* if x */
return

find_uk:
   drop uklist.
   uk_work   = user||'.N1UK.WORK'
   "alloc da('"uk_work"') fi(ukwork) shr reu"
   "execio * diskr ukwork (stem uklist. finis)"
   "free fi(ukwork)"
   if substr(uklist.1,1,8) <= bdate then do
      do i = 2 to uklist.0
         aux = substr(uklist.i,1,8)
         if aux >= bdate then do
            j = i-1
            parse var uklist.j date ';' title ';' artist
            say '-------------------------------'
            say 'On 'bd_year'/'bd_month'/'bd_day' #1 in UK was:'
            say '-------------------------------'
            say 'UK_Title : ' title
            say 'UK_Artist: ' artist
            leave
         end
      end /* do i */
   end 
   else do 
      say 'NULL'
   end /* if substr */
   junk = outtrap(line.)
   "delete '"uk_work"'"
   junk = outtrap('off')
return

find_us:
   drop uslist.
   us_work   = user||'.N1US.WORK'
   "alloc da('"us_work"') fi(uswork) shr reu"
   "execio * diskr uswork (stem uslist. finis)"
   "free fi(uswork)"
   if substr(uslist.1,1,8) <= bdate then do
      do i = 2 to uslist.0
         aux = substr(uslist.i,1,8)
         if aux >= bdate then do
            j = i-1
            parse var uslist.j date ';' title ';' artist
            say '-------------------------------'
            say 'On 'bd_year'/'bd_month'/'bd_day' #1 in US was:'
            say '-------------------------------'
            say 'US_Title : ' title
            say 'US_Artist: ' artist
            leave
         end
      end /* do i */
   end 
   else do 
      say 'NULL'
   end /* if substr */
   junk = outtrap(line.)
   "delete '"us_work"'"
   junk = outtrap('off')
return
