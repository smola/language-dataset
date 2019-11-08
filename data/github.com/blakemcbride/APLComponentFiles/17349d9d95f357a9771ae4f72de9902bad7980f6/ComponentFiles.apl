#!/usr/local/bin/apl --script
 ⍝⍝⍝⍝⍝⍝⍝⍝⍝⍝⍝⍝⍝⍝⍝⍝⍝⍝⍝⍝⍝⍝⍝⍝⍝⍝⍝⍝⍝⍝⍝⍝⍝⍝⍝⍝⍝⍝⍝⍝⍝⍝⍝⍝⍝⍝⍝⍝
⍝
⍝ ComponentFiles 2018-01-06  20:04:57 (GMT-5)
 ⍝⍝⍝⍝⍝⍝⍝⍝⍝⍝⍝⍝⍝⍝⍝⍝⍝⍝⍝⍝⍝⍝⍝⍝⍝⍝⍝⍝⍝⍝⍝⍝⍝⍝⍝⍝⍝⍝⍝⍝⍝⍝⍝⍝⍝⍝⍝⍝

∇cid←data CF_APPEND fh;r;tbl;⎕IO;i;n;data2
 ⎕IO←1
 ⎕ES((~(fh←''⍴fh)∊_CF_MAP[;1])∨(1≠⍴,fh)∨1<≡fh)/'INVALID FILE HANDLE'
 ⎕ES(0=⎕NC'data')/'MISSING DATA'
 cid←_CF_MAP[i←''⍴(fh=_CF_MAP[;1])/⍳1↑⍴_CF_MAP;3]
 tbl←⊃_CF_MAP[i;2]
 data2←14 ⎕CR data
 n←0
 LOOP:→(2000<⍴data2)/BIG∆NEW
 '→TRY∆AGAIN'⎕EA'r←(''insert into '',tbl,'' (ikey, sdata) values (?, ?);'')SQL∆Exec[_CF_DB] cid data2'
 _CF_MAP[i;3]←cid+1
 →0
 BIG∆NEW:'→TRY∆AGAIN'⎕EA'r←(''insert into '',tbl,'' (ikey, ldata) values (?, ?);'')SQL∆Exec[_CF_DB] cid data2'
 _CF_MAP[i;3]←cid+1
 →0
 ⍝ It is possible that some other application appended a new component and our max is out-of-sync.
 ⍝ Update component ID and try again.
 TRY∆AGAIN:⎕ES(_CF_MAX_TRIES<n←n+1)/'APPEND ERROR'
 cid←_CF_NEXT tbl
 →LOOP
∇

∇r←CF_CLOSE fhl;⎕IO
 ⎕IO←1
 ⎕ES((∨/~(,fhl)∊_CF_MAP[;1])∨(0≠1↑0⍴fhl)∨1<≡fhl)/'INVALID FILE HANDLE'
 _CF_MAP←(~_CF_MAP[;1]∊fhl)⌿_CF_MAP
 r←0 0⍴0
∇

∇r←CF_CREATE fnm;cmd
 ⎕ES((0=⍴,fnm)∨(' '≠1↑0⍴fnm)∨(1<⍴⍴fnm)∨1<≡fnm)/'INVALID FILE NAME'
 ⎕ES(CF_FILEEXISTS fnm)/'FILE ALREADY EXISTS'
 cmd←'CREATE TABLE ',fnm,' ('
 cmd←cmd,'ikey INTEGER NOT NULL PRIMARY KEY, '
 cmd←cmd,'sdata CHARACTER VARYING(2000),'
 cmd←cmd,'ldata text);'
 '⎕ES''CREATE TABLE ERROR'''⎕EA'r←cmd SQL∆Exec[_CF_DB] '''''
 r←'insert into _apl_component_files (file_name) values (?);' SQL∆Exec[_CF_DB] ⊂fnm
 r←_CF_ADD fnm
∇

∇type CF_DBCONNECT params
 ⍎(0=⎕NC 'SQL')/'⎕ES(''SQL''≢''lib_sql.so''⎕FX''SQL'')/''Error loading SQL library'''
 '⎕ES''DBCONNECT ERROR'''⎕EA'_CF_DB←type SQL∆Connect params'
 _CF_MAP←0 3⍴0
 _CF_MAX_TRIES←30
∇

∇type CF_DBCREATE params;cmd;db
 ⍎(0=⎕NC 'SQL')/'⎕ES(''SQL''≢''lib_sql.so''⎕FX''SQL'')/''Error loading SQL library'''
 '⎕ES''DBCONNECT ERROR'''⎕EA'db←type SQL∆Connect params'
 cmd←'create table _apl_component_files ('
 cmd←cmd,'file_name character varying (80) not null unique);'
 '⎕ES''DBCREATE ERROR'''⎕EA'cmd←cmd SQL∆Exec[db]'''''
 _CF_DB←db
 _CF_MAP←0 3⍴0
 _CF_MAX_TRIES←30
∇

∇CF_DBDISCONNECT;r;db
 db←_CF_DB
 r←⎕EX '_CF_DB'
 r←⎕EX '_CF_MAP'
 r←⎕EX '_CF_MAX_TRIES'
 '⎕ES''DISCONNECT ERROR'''⎕EA'r←SQL∆Disconnect db'
∇

∇z←CF_ERASE fnm
 ⎕ES((' '≠1↑0⍴fnm)∨(1<⍴⍴fnm)∨1<≡fnm)/'INVALID FILE NAME'
 '⎕ES''ERASE TABLE ERROR'''⎕EA'z←(''drop table '',fnm) SQL∆Exec[_CF_DB]'''''
 z←'delete from _apl_component_files where file_name=?;' SQL∆Exec[_CF_DB] ⊂fnm
 _CF_DELETE fnm
 z←0 0⍴0
∇

∇r←CF_FILEEXISTS fnm
 ⎕ES((0=⍴,fnm)∨(' '≠1↑0⍴fnm)∨(1<⍴⍴fnm)∨1<≡fnm)/'INVALID FILE NAME'
 r←(⊂,fnm)∊CF_FILES
∇

∇files←CF_FILES
 ⍝ Returns a list of component files
 files←'select file_name from _apl_component_files;' SQL∆Select[_CF_DB]''
∇

∇fhl←CF_INUSE
 fhl←_CF_MAP[;⎕IO]
∇

∇FL←CF_LIST A;w;lst
 ⎕ES((0≠+/⍴,A)∨(1≠⍴⍴A)∨1≠≡A)/'INVALID ARGUMENT'
 FL←(0,w←⌈/0,∊⍴¨lst←CF_FILES)⍴''
 lst←{FL←FL⍪w↑⍵}¨lst
∇

∇cid←CF_NEXT fh;⎕IO
 ⎕IO←1
 ⎕ES((~(fh←''⍴fh)∊_CF_MAP[;1])∨(1≠⍴,fh)∨1<≡fh)/'INVALID FILE HANDLE'
 cid←_CF_MAP[''⍴(fh=_CF_MAP[;1])/⍳1↑⍴_CF_MAP;3]
∇

∇r←fh CF_OPEN fnm;⎕IO
 ⎕IO←1
 ⎕ES((0=⍴,fnm)∨(' '≠1↑0⍴fnm)∨(1<⍴⍴fnm)∨1<≡fnm)/'INVALID FILE NAME'
 ⎕ES(~CF_FILEEXISTS fnm)/'FILE DOES NOT EXIST'
 ⍎(2≠⎕NC 'fh')/'fh←1+⌈/0,_CF_MAP[;1]'
 ⎕ES(((fh←''⍴fh)∊_CF_MAP[;1])∨(1≠⍴,fh)∨1<≡fh)/'INVALID FILE HANDLE'
 r←fh _CF_ADD fnm
∇

∇r←CF_READ cfi;⎕IO;i
 ⎕IO←1
 ⎕ES((1≠⍴⍴cfi)∨2≠⍴,cfi)/'INVALID FILE HANDLE, COMPONENT ID'
 ⎕ES((~cfi[1]∊_CF_MAP[;1])∨0≠≡cfi[1])/'INVALID FILE HANDLE'
 ⎕ES((0≠≡cfi[2])∨0≠1↑0⍴cfi[2])/'INVALID COMPONENT ID'
 ⎕ES((_CF_MAP[i←''⍴(cfi[1]=_CF_MAP[;1])/⍳1↑⍴_CF_MAP;3]≤cfi[2])∨cfi[2]<1)/'INVALID COMPONENT ID'
 r←('select sdata, ldata from ',(⊃_CF_MAP[i;2]),' where ikey=?') SQL∆Select[_CF_DB] cfi[2]
 ⎕ES(1=⍴⍴r)/'COMPONENT NOT FOUND'
 r←¯14 ⎕CR (⊃r[1]),⊃(r←,r)[2]
∇

∇z←nfnm CF_RENAME fnm;i
 ⎕ES(0=⎕NC'nfnm')/'MISSING ARGUMENT'
 ⎕ES((0=⍴,fnm)∨(' '≠1↑0⍴fnm)∨(1<⍴⍴fnm)∨1<≡fnm)/'INVALID ORIGINAL FILE NAME'
 ⎕ES((0=⍴,nfnm)∨(' '≠1↑0⍴nfnm)∨(1<⍴⍴nfnm)∨1<≡nfnm)/'INVALID NEW FILE NAME'
 ⎕ES(~CF_FILEEXISTS fnm)/'ORIGINAL FILE NAME DOES NOT EXIST'
 ⎕ES(CF_FILEEXISTS nfnm)/'NEW FILE NAME ALREADY EXISTS'
 '⎕ES''RENAME ERROR'''⎕EA'z←(''alter table '',fnm,'' rename to '',nfnm,'';'') SQL∆Exec[_CF_DB]'''''
 '⎕ES''RENAME ERROR'''⎕EA'z←''update _apl_component_files set file_name=? where file_name=?;'' SQL∆Exec[_CF_DB] nfnm fnm'
 ⍎(0≠i←_CF_FIND fnm)/'_CF_MAP[i;2]←⊂nfnm'
 z←0 0⍴0
∇

∇z←data CF_WRITE cfi;r;tbl;⎕IO;i;data2
 ⎕IO←1
 ⎕ES(0=⎕NC'data')/'MISSING DATA'
 ⎕ES((1≠⍴⍴cfi)∨2≠⍴,cfi)/'INVALID FILE HANDLE, COMPONENT ID'
 ⎕ES((~cfi[1]∊_CF_MAP[;1])∨0≠≡cfi[1])/'INVALID FILE HANDLE'
 ⎕ES((0≠≡cfi[2])∨0≠1↑0⍴cfi[2])/'INVALID COMPONENT ID'
 ⍝ The following line handles the case of another program updating the table
 ⍎(_CF_MAP[i←''⍴(cfi[1]=_CF_MAP[;1])/⍳1↑⍴_CF_MAP;3]≤cfi[2])/'_CF_MAP[i;3]←_CF_NEXT⊃_CF_MAP[i;2]'
 ⎕ES((_CF_MAP[i;3]≤cfi[2])∨cfi[2]<1)/'INVALID COMPONENT ID'
 z←0 0⍴0
 data2←14 ⎕CR data
 r←('select ikey from ',(tbl←⊃_CF_MAP[i;2]),' where ikey=?;') SQL∆Select[_CF_DB]cfi[2]
 →((1=⍴⍴r)∧0=1↑⍴r)/NEW
 →(2000<⍴data2)/BIG∆CHANGE
 '⎕ES''WRITE ERROR'''⎕EA'r←(''update '',tbl,'' set sdata=?,ldata=null where ikey=?;'')SQL∆Exec[_CF_DB] data2 cfi[2]'
 →0
 BIG∆CHANGE: '⎕ES''WRITE ERROR'''⎕EA'r←(''update '',tbl,'' set sdata=null,ldata=? where ikey=?;'')SQL∆Exec[_CF_DB] data2 cfi[2]'
 →0
 ⍝ INSERT code used to support ability to delete components in the middle
 NEW:→(2000<⍴data2)/BIG∆NEW
 '⎕ES''WRITE ERROR'''⎕EA'←(''insert into '',tbl,'' (ikey, sdata) values (?, ?);'')SQL∆Exec[_CF_DB] cfi[2] data2'
 →0
 BIG∆NEW:'⎕ES''WRITE ERROR'''⎕EA'r←(''insert into '',tbl,'' (ikey, ldata) values (?, ?);'')SQL∆Exec[_CF_DB] cfi[2] data2'
∇

∇Z←SQL∆Begin Y
 Z←SQL[5] Y
∇

∇Z←SQL∆Commit Y
 Z←SQL[6] Y
∇

∇Z←X SQL∆Connect Y
 Z←X SQL[1] Y
∇

∇Z←SQL∆Disconnect Y
 Z←SQL[2] Y
∇

∇Z←X SQL∆Exec[db] Y
 Z←X SQL[4,db] Y
∇

∇Z←SQL∆Rollback Y
 Z←SQL[7] Y
∇

∇Z←X SQL∆Select[db] Y
 Z←X SQL[3,db] Y
∇

∇Z←SQL∆Tables Y
 Z←SQL[8] Y
∇

∇Z←X (F SQL∆WithTransaction FINDDB) Y;result
 SQL∆Begin FINDDB
 →(0≠⎕NC 'X')/dyadic
 result ← '→rollback' ⎕EA 'F Y'
 →commit
 dyadic:
 result ← '→rollback' ⎕EA 'X F Y'
 commit:
 SQL∆Commit FINDDB
 Z ← result
 →end
 rollback:
 SQL∆Rollback FINDDB
 ⎕ES "Transaction rolled back"
 end:
∇

∇r←fh _CF_ADD fnm
 ⍎(2≠⎕NC 'fh')/'fh←1+⌈/0,_CF_MAP[;⎕IO]'
 _CF_MAP←_CF_MAP⍪(r←fh),(⊂fnm),_CF_NEXT fnm
∇

∇_CF_DELETE fnm;i
 →(0=1↑0⍴fnm)/HANDLE
 →(0=i←_CF_FIND fnm)/0
 _CF_MAP←(i≠⍳1↑⍴_CF_MAP)⌿_CF_MAP
 →0
 HANDLE: _CF_MAP←(fnm=_CF_MAP[;⎕IO])⌿_CF_MAP
∇

∇i←_CF_FIND fnm
 ⍝ returns the row number into _CF_MAP, 0 if unfound
 i←''⍴((⊂fnm)≡¨_CF_MAP[;⎕IO+1])/⍳1↑⍴_CF_MAP
∇

∇cid←_CF_NEXT fnm
 cid←1+''⍴1↑,⊃('select max(ikey) from ',fnm,';') SQL∆Select[_CF_DB]''
∇

⎕CT←1E¯13

⎕FC←(,⎕UCS 46 44 8902 48 95 175)

⎕IO←1

⎕L←0

⎕LX←' ' ⍝ proto 1
  ⎕LX←0⍴⎕LX ⍝ proto 2

⎕PP←10

⎕PR←,' '

⎕PS←0 0

⎕PW←80

⎕R←0

⎕RL←1

⎕TZ←¯5

⎕X←0

