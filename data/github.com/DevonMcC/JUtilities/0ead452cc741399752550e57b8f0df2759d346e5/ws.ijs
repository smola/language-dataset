NB.* WS.ijs: (partially) emulate APL WS - save variables to
NB. files.  Works for arbitrary namespaces but does not
NB. handle scripts.  Last updated 20180513 19:27.

NB.* DIRFL: Name of directory file which maps file and variable names.
NB.* lookup: return varname given filename or vice versa.
NB.* delVar: delete variable on file.
NB.* fexist: 1 if named file exists, 0 otherwise.
NB.* jfi: just files in dir listing.
NB.* nameExists: 1 if name exists, 0 otherwise.
NB.* getTempDir: get name of temporary directory.
NB.* crTmpFl: create new, randomly named file in temp dir.
NB.* fileRename: rename file after minimal checking.
NB.* renameVarFile: rename variable file from old to new name.
NB.* renameVar: rename variable from old to new name.
NB.* synchDirFlToActualFls: ensure vars dir only refers to existing files.
NB.* DEFDIR: Default directory with variables on file.
NB. Change default to current dir - 20120903 13:53.
NB.* USAGE: brief explanation on how to use some top-level functions.
NB.* loadAllVars: load all variables into base namespace from dir
NB.* saveAllVars: save all vars in base namespace to dir specified.
NB.* fileVar: put variable named by y (e.g. "xxx") into file (e.g.
NB.* unfileVar: get variable named by y from file; inverse of fileVar.
NB.* unfile1Var: retrieve value of variable from file and instantiate in
NB.* getFileDir: get file-directory from directory y or create if none.
NB.* putFileDir: put file-directory x into directory y
NB.* baseNum: return number y in base with digits x : positive
NB.    Definitions from "fldir" repeated here for self-sufficiency.
NB.* endSlash: ensure terminal slash char.
NB.* v2f: Vector to File: write vector of lines to file as lines.
NB.* renameFls: rename file (1{y) in dir (0{y) to 2{y.

coclass 'WS'
require 'strings'

NB.* DIRFL: Name of directory file which maps file and variable names.
DIRFL=: '#dir.dir'

NB.* lookup: return varname given filename or vice versa: returns 1 or 0 for success
NB. or failure; [index into directory file if success;] explanatory text.
lookup=: 4 : 0
   (endSlash 1!:43'') lookup y
:
   'dirloc fldir'=. getFileDir x
   if. 0 e. $fldir do. 0;'No directory at ','.',~x return. end.
   if. '.DAT'-:toupper _4{.y do.        NB. Lookup var for filename
       if. (<toupper y) e. 0{fldir do.
           1;wh;(1{fldir){~wh=. (0{fldir)i.<toupper y
       else. 0;'File "',y,'" not found at ','.',~dirloc end.
   else.
       if. (<y) e. 1{fldir do.
           1;wh;(0{fldir){~wh=. (1{fldir)i.<y
       else. 0;'Var "',y,'" not found at ','.',~dirloc end.
   end.
)

NB.* delVar: delete variable on file.
delVar=: 4 : 0
   (endSlash 1!:43'') delVar y
:
   'dirloc fldir'=. getFileDir x
   if. 0 e. $fldir do. 0;'No directory at ','.',~x return. end.
   if. nameExists y do. 4!:55 <y end.
   if. >0{whnm=. x lookup y do.
       if. ferase dirloc,>(<0,>1{whnm){fldir do.
           ((<<1{whnm){"1 fldir) putFileDir dirloc  NB. Put back directory.
           1
       else. 0;'Unable to erase "','".',~dirloc,>(<0,>1{whnm){fldir end.
   else. 0;'Not found: "',y,'".' end.
)

NB. Next few locally dupe common or standard fns to make this stand-alone.
NB.* fexist: 1 if named file exists, 0 otherwise.
fexist=: (1:@(1!:4) ::0:@((([:< 8 u:>) ::])&>)@(<^:(L.=0:)))
jfi=: (]#~[:-.'d'e.&>4{"1])             NB.* jfi: just files in dir listing.
NB.* nameExists: 1 if name exists, 0 otherwise.
nameExists=: 0:"_ <: [: 4!:0 <^:(L. = 0:)

NB.* getTempDir: get name of temporary directory assuming in USERFOLDER_j_.
getTempDir=: 3 : 0
   if. nameExists 'UserFolders_j_' do. UF=. UserFolders_j_
   else. UF=. USERFOLDERS_j_ end.
   PATHSEP_j_,~>1{(UF,a:;PATHSEP_j_,'Temp'){~({."1 UF)i.<'Temp'
)

NB.* crTmpFl: create new, randomly named file in temp dir.
crTmpFl=: 3 : 0
   ALPHA=. 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ'
   suff=. ,>(0=#y){y;'tmp'
   td=. getTempDir ''
   while. fexist flnm=. td,suff,~'.',~ALPHA{~?8$#ALPHA do. end.
   flnm
NB.EG batfl=. crTmpFl 'bat'
)

NB.* fileRename: rename file after minimal checking.
fileRename=: 3 : 0
   'dirnm oldnm newnm'=. y
   if. fexist newnm do.
       0;'Cannot rename "',oldnm,'" to existing name "',newnm,'".'
   else.
       cmds=. renameFls (oldnm,~endSlash dirnm);oldnm;newnm
   end.
NB.EG rc=. fileRename 'C:\Temp\';'log01.dat';'log02.dat'
)

NB.* renameVarFile: rename variable file from old to new name.
renameVarFile=: 4 : 0
   (endSlash 1!:43'') renameFile y
:
   'dirloc fldir'=. getFileDir x
   'oldnm newnm'=. y
   if. 0 e. $fldir do. 0;'No directory in ','.',~x return. end.
   if. (<oldnm) e. 0{fldir do.     NB. Is file name on file?
       if. (<newnm) e. 0{fldir do. NB. New name already on file?
           0;'Cannot name file "',oldnm,'" to existing name "',newnm,'".'
           return.
       else.
           wh=. (0{fldir) i. <oldnm          NB. Where name is
           fldir=. (<newnm) (<0,wh)}fldir    NB. Replace with new name,
           if. 0~:>{.rc=. fileRename dirloc;oldnm;newnm do.
               1;y [ fldir putFileDir dirloc NB.  put back directory.
           else. 0;rc end.                   NB. or don't if rename failed.
       end.
   else. 0;<'File not found: ',oldnm
   end.
)

NB.* renameVar: rename variable from old to new name.
renameVar=: 4 : 0
   (endSlash 1!:43'') renameVar y
:
   'dirloc fldir'=. getFileDir x
   'oldnm newnm'=. y
   if. 0 e. $fldir do. 0;'No directory in ','.',~x return. end.
   if. (<oldnm) e. 1{fldir do.     NB. Is var name on file?
       if. (<newnm) e. 1{fldir do. NB. New name already on file?
           0;'Cannot name var "',oldnm,'" to existing name "',newnm,'".'
           return.
       else.
           wh=. (1{fldir) i. <oldnm          NB. Where name is
           fldir=. (<newnm) (<1,wh)}fldir    NB. Replace with new name and
           fldir putFileDir dirloc           NB.  put back directory.
           1;y
       end.
   else. 0;<'Var not found: ',oldnm
   end.
)

NB.* synchDirFlToActualFls: trim dir file to refer only to existing files.
synchDirFlToActualFls=: 3 : 0
   'dd fldd'=. getFileDir y
   aclfls=. toupper&.>{."1 jfi dir dd,'*.DAT'
   missing=. aclfls-.0{fldd        NB. Actual files not in our dir file.
   fldd=. fldd#"1~aclfls e.~0{fldd
   fldd putFileDir dd
   missing
NB.EG notindexed=. synchDirFlToActualFls 'D:\WUTemp\VarsDir'
)

DEFDIR=: getTempDir ''   NB.* DEFDIR: Default directory with variables on file.

NB.* USAGE: brief explanation on how to use some top-level functions.
USAGE=: 0 : 0
The 2 top-level functions are "saveAllVars" and "loadAllVars".

   saveAllVars_WS_ ''
saves all variables from the base namespace to the temp directory.

   loadAllVars_WS_ ''
restores these variables to the base namespace.

The directory to which files are written, along with the
directory file named by "DIRFL", is the right argument of
"saveAllVars" and "loadAllVars".  The default directory is named
by "DEFDIR" and defaults to the temp directory (returned by
"getTempDir".)

Individual variables may be written to or read from file with
"fileVar" and "unfileVar", respectively.  For example, to save
the global variable "lotsaNums" to directory "C:\Temp":
   'C:\Temp' fileVar_WS_ 'lotsaNums'

To subsequently read the variable back from file:
   'C:\Temp' unfileVar_WS_ 'lotsaNums'

These utilities rely on the global name of the directory file in DIRFL_WS_,
which is '#dir.dir' by default.
)

saveWS1=: 3 : 0
   allnames=: ; <@((3 : 'nl__y i.4') ,&.> '_'&,@(,&'_')&.>)"(0) 18!:1 i.2
   (; <@(> , '=:' , 5!:5 , (13 10{a.)"_)"0 ] (#~nameExists&>)allnames) 1!:2 boxopen y
)

loadWS1=: 3 : 0
   0!:0 boxopen y
)

saveFunctionsRH=: 0 : 0
NB. To save everything, done in 3 lines:

nl_z_=: 4!:1
allnames=: ; <@((3 : 'nl__y i.4') ,&.> '_'&,@(,&'_')&.>)"(0) 18!:1 i.2
(; <@(> , '=:' , 5!:5 , (13 10{a.)"_)"0 allnames) 1!:2 <'activews'

NB. To get everything back, done in 1 line:
0!:0 <'activews'
)

dontUseKeyfiles=: 0 : 0
require'keyfiles'
saveWS2=: (<@:[ keycreate) (3!:1@:(5!:1)@:] keywrite ,)"0  ;@:((], '_' , [ , '_'"_) L: 0 verb : '<nl__y $~0'"0)@:conl bind ''
loadWS2=: < (] 4 : ('(x) =: y 5!:0';'1') :: 0:&>(3!:2&.>)@:keyread"1@:,.) keydir)

saveFunctionsDB=: 0 : 0
   require'keyfiles'
   save    =: (<@:[ keycreate) (3!:1@:(5!:1)@:] keywrite ,)"0  ;@:((], '_' , [ , '_'"_) L: 0 verb : '<nl__y $~0'"0)@:conl bind ''
   load     =: < (] 4 : ('(x) =: y 5!:0';'1') :: 0:&>(3!:2&.>)@:keyread"1@:,.) keydir

NB. Examples of use:
   f           =:  jpath'~temp\X.jkf'
   FNAF_floop_ =: 'my original value'
   save f

   FNAF_floop_ =: 'restore my value!'
   load f
   FNAF_floop_
my original value
)

NB.* varExists: tell if variable named y exists in specified file vars dir x
varExists=: 3 : 0
   (endSlash 1!:43'') varExists y
:
   'dirloc fldir'=. getFileDir x
   rc=. 0
   if. -.0 e. $fldir do.
       rc=. (<y)e. 1{fldir
   end.
   rc
)

loadAllVars=: 3 : 0
NB.* loadAllVars: load all variables into base namespace from dir
NB. specified (default is temp dir).
   if. 0=#y do. unfileVar ''
   else. y unfileVar ''
   end.
NB.EG nms=. loadAllVars '\temp\foo'
)

NB.* saveAllVars: save all vars in namespace x to dir specified.
saveAllVars_WS_=: 4 : 0
   'base' saveAllVars y
:
   coclass x [ svcoc_WS_=. coname''
   nmlst_WS_=. <;._1 ' ',dsp_base_,(names 0),"1 ' '
   nmlst_WS_=. nmlst_WS_,&.><'_',x,'_'
   if. -.dirExists_base_ y do.               NB. Wait for dir to
       6!:3,1 [ rc=. 1!:5 <y end.            NB.  be created.
   nmFlnms=. (<y) fileVar_WS_&.>nmlst_WS_    NB. List files saved to.
   coclass svcoc_WS_
NB.EG fileNames=. 'someNamespace' saveAllVars '\temp\foo'
)

fileVar=: 3 : 0
NB.* fileVar: put variable named by y (e.g. "xxx") into file (e.g.
NB. "xxx.dat") under directory x .  The directory must have a directory
NB. file that matches file names to variable names.  We have to do this
NB. because in OSs that are case-insensitive, we need to distinguish
NB. distinct J names that map to same OS file name, e.g. 'aa' and 'AA'.
   (endSlash 1!:43'') fileVar y
:
   if. ' '~:{.0$y do. 0;<'Right argument must be variable name.'
       return. end.
   if. 1<#$y do. 0;<'Right argument must be single variable name.'
       return. end.
   y=. ,y
   if. -.nameExists y do. 0;<'No such variable "',(,":y),'"' return.
   end.                                 NB. Var must exist.
   alph=. '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ_'
   'dir fldir'=. getFileDir x
   if. (<y) e. 1{fldir do.             NB. Already have this on file?
       wh=. (1{fldir) i. <y            NB. If so, look up its filename.
       flnm=. >wh{0{fldir
   else.                                NB. If not, create a filename.
       len=. #y                        NB. Use all chars of name if possible
       flnm=. toupper len{.y
       flnm=. flnm,'.DAT'
NB. Ensure is uncataloged and unused filename.
       while. ((<flnm) e. 0{fldir) +. fexist dir,flnm do.
           flnm=. toupper len{.y
           flnm=. flnm,(alph baseNum ?2147483647),'.DAT'
       end.
       (fldir,.flnm;<y) putFileDir dir NB. put back directory.
   end.
   try. (3!:1 (".y,'__')) 1!:2 <dir,flnm    NB. var->file
   catch. (3!:1 (".y)) 1!:2 <dir,flnm    NB. var->file
   end.
   1;<flnm
NB.EG (<'C:\Temp\') fileVar&.>'hdr';<'dbmat' [ hdr=: hdr [ dbmat=: dbmat
)

unfileVar=: 3 : 0
NB.* unfileVar: get variable named by y from file; inverse of fileVar.
NB. x is directory in which to find FILEDIR and files of vars.
   (endSlash 1!:43'') unfileVar y
:
   'dirloc fldir'=. getFileDir x
   if. 0 e. $fldir do. 0;'No directory in ','.',~x return. end.
   flnm=. ''
   if. 0=#y do.                        NB. All names if '' specified
       rc=. unfile1Var &.> (<<dirloc),&.><"1 |:fldir
   else.
       if. (<y) e. 1{fldir do.          NB. Is this var name on file?
           wh=. (1{fldir) i. <y         NB. If so, where in directory?
           rc=. unfile1Var dirloc;wh{"1 fldir     NB. Instantiate it.
       else. rc=. 0;<'Var not found: ',y
       end.
   end.
   rc
NB.EG rc=. '\GIR\Data\TC030703' unfileVar_WS_ ''
NB.EG (<1!:43'') unfileVar_WS_&.>'var1';'var2'
)

getFileVarVal=: 3 : 0
NB.* getFileVarVal: get value of variable named by y from file but do NOT
NB. instantiate variable itself; x is directory with var directory and
NB. files of var values.
   (endSlash 1!:43'') unfileVar y
:
   'dirloc fldir'=. getFileDir x
   flnm=. ''
   if. 0 e. $fldir do. 0;'No directory in ','.',~x return. end.
   if. (<y) e. 1{fldir do.         NB. Is this var name on file?
       wh=. (1{fldir) i. <y        NB. If so, look up its filename.
       if. fexist flnm=. dirloc,>wh{0{fldir do.
           1;<3!:2 (1!:1 <flnm)
        else.
           0;<'File not found: ','.',~flnm
       end.
   else. 0;<'Var ',y,' not found in file dir ','.',~x
   end.
NB.EG var=. '\Data\Pxs\' getFileVarVal_WS_ 'Pxs020101_021231'
)

unfile1Var=: 3 : 0
NB.* unfile1Var: retrieve value of variable from file and instantiate in
NB. base locale given filedir entry y : directory, file name, var name.
   'dirloc flnm varnm'=. y
   flnm=. dirloc, flnm
   if. fexist flnm do.
       (varnm)=: 3!:2 (1!:1 <flnm)
       1;<y
    else.
       0;<'File not found: ',flnm
   end.
)

getFileDir=: 3 : 0
NB.* getFileDir: get file-directory from directory y or create if none.
   dirloc=. y,('\'~:{:y)#'\'
   dfn=. dirloc,DIRFL                   NB. Directory-File Name
   if. fexist dfn do.
       fldir=. a:-.~deb&.>f2v dfn
       fldir=. |:><;._1&.>' ',&.>fldir  NB. FILENAME Varname
       fldir=. (toupper&.>0{fldir) 0}fldir
   else. fldir=. 2 0$<''                NB. start empty file directory
   end.
   dirloc;<fldir
)

putFileDir=: 4 : 0
NB.* putFileDir: put file-directory x into directory y
   dir=. y,('\'~:{:y)#'\'
   dfn=. dir,'#dir.dir'                 NB. Directory-File Name
   (}:&.>,&.>/x,&.>' ') v2f dfn        NB. put back directory.
)

NB. --------- General utilities follow ---------------
dsp=: deb"1@dltb"1                                     NB. Remove excess spaces.
isNum=: 1 4 8 16 64 128 1024 4096 8192 16384 e.~ 3!:0  NB. 1 if numeric arg
dirExists=: 3 : '0~:#(1!:0)@< (-''/\''+./ . e. {:y)}.y'  NB. 0: dir does not exist

NB.* f2v: File to Vector: read file -> vector of lines.
f2v=: 3 : 'vec=. l2v 1!:1 <y'

v2f=: 4 : 0
NB.* v2f: Vector to File: write vector of lines to file as lines.
   if. -.nameExists 'EOL' do. EOL=. LF end.
   (;x,&.><EOL) 1!:2 <y
NB.EG ('line 1';'line 2';<'line 3') v2f 'C:\test.tmp'
)

NB.* l2v: Lines to Vec: convert lines terminated by LF to vector elements.
l2v=: 3 : '<;._1 LF,y-.CR'

baseNum=: 4 : 0
NB.* baseNum: return number y in base with digits x.
   len=. #alph=. x
   assert. ((0 < ]) *. ] = <.) num=. y  NB. positive integers only.
   if. isNum num do. alph{~(digits$len)#:num [ digits=. >:<.len^.1>.num
   else. len#.alph i. num end.   NB. Assume "number" is char - convert to base 10 number.
NB.EG '0123456789ABCDEF' baseNum 255
NB.EG '0123456789ABCDEF' baseNum 'FF'
)

NB.* jfi: just files from dir list.
jfi=: ] #~ [: -. 'd' e.&> 4 {"1 ]
NB.* endSlash: ensure terminal slash char.
endSlash=: ] , '/' #~ '/' ~: {:
v2f=: 4 : 0
NB.* v2f: Vector to File: write vector of lines to file as lines.
   if. -.nameExists 'EOL' do. EOL=. LF end.
   (;x,&.><EOL) 1!:2 <y
NB.EG ('line 1';'line 2';<'line 3') v2f 'C:\test.tmp'
)
NB. dir=: 1!:0@<
NB.* renameFls: rename file (1{y) in dir (0{y) to 2{y.
renameFls=: 3 : 0
   'flexp frstr tostr'=. y    NB. File selection expression, from & to strings
   fls=. {."1 jfi dir flexp   NB. eg. 'E:\amisc\pix\s90*.jpg'
   srcdir=. ('\' e. flexp)#flexp{.~flexp i: '\'
   cmds=. (flexp{.~>:flexp i. ':');'cd "',srcdir,'"'   NB. Move to disk, dir
NB. e.g. 'frstr tostr'=. 's90X03';'s90X02'   NB. Name change by replacement
   newnms=. (<frstr;tostr)stringreplace&.>fls
   cmds=. cmds,(<'ren "'),&.>fls,&.>(<'" "'),&.>newnms,&.>'"'
   cmds v2f batflnm=. srcdir,'renmFls.bat'
   shell batflnm
   shell ('del ',batflnm),~;(<' && '),~&.>2{.cmds
   cmds
NB.EG cmds=. renameFls 'E:\amisc\pix\s90*.jpg';'s90X03';'s90X02'
)

cocurrent 'base'
coinsert 'WS'
coinsert_WS_ 'base'

NB. Initiated by Devon H. McCormick, 20030711.
