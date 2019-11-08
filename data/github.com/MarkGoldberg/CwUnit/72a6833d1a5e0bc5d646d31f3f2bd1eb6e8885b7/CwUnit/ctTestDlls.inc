
!Region Notices 
! ================================================================================
! Notice : Copyright (C) 2014, Mark Goldberg
!          Distributed under LGPLv3 (http://www.gnu.org/licenses/lgpl.html)
!
!    This file is part of CwUnit (https://github.com/MarkGoldberg/CwUnit)
!
!    CwUnit is free software: you can redistribute it and/or modify
!    it under the terms of the GNU General Public License as published by
!    the Free Software Foundation, either version 3 of the License, or
!    (at your option) any later version.
!
!    CwUnit is distributed in the hope that it will be useful,
!    but WITHOUT ANY WARRANTY; without even the implied warranty of
!    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!    GNU General Public License for more details.
!
!    You should have received a copy of the GNU General Public License
!    along with CwUnit.  If not, see <http://www.gnu.org/licenses/>.
! ================================================================================
!EndRegion Notices 

   INCLUDE(     'ctQueue.inc'),ONCE
   INCLUDE('ctFileHelper.inc'),ONCE
   INCLUDE(  'ctAllTests.inc'),ONCE
   INCLUDE(   'DataTypes.equ'),ONCE
   INCLUDE(     'ICwUnit.int'),ONCE
   INCLUDE(      'errors.clw'),ONCE
   INCLUDE('ctResultSets.inc'),ONCE 
   INCLUDE(   'ctLoggers.inc'),ONCE


gtTestDLLBase GROUP,TYPE      !designed for injecting into ctResultSets
FileHelper       &ctFileHelper
CwUnit           &ICwUnit
hLib             HMODULE
LoadError        LONG
				  END
gtTestDLL	  GROUP(gtTestDLLBase),TYPE  
AllTests         &ctAllTests
				  END

qtTestDLL     QUEUE(gtTestDLL),TYPE
			     END 



ctTestDLLs CLASS(ctQueue),TYPE,MODULE('ctTestDLLs.clw'),LINK('ctTestDLLs.clw')
Q                             &qtTestDLL
ResultSets                    &ctResultSets
Loggers                       &ctLoggers,PRIVATE !Injected
LoggersInjected               BOOL,PRIVATE
!----------
CONSTRUCT                      PROCEDURE()
DESTRUCT                       PROCEDURE()
Del                            PROCEDURE,DERIVED
Description                    PROCEDURE(),STRING,DERIVED
Init                           PROCEDURE(*ctLoggers Loggers)

Add                            PROCEDURE(STRING FileName)

LoadAllTests                   PROCEDURE()
RunAllTests                    PROCEDURE()
UnLoadAllTests                 PROCEDURE()

LoadTests                      PROCEDURE(*gtTestDLL TestDLL)
RunTests                       PROCEDURE(*ctAllTests AllTests, LONG ResultSetID)
UnLoadTests                    PROCEDURE(*gtTestDLL TestDLL)

Get_ICwUnit                    PROCEDURE(*gtTestDLL TestDLL),BOOL,PROC !TRUE when ICwUnit was found and set
FreeLibrary                    PROCEDURE(*gtTestDLL TestDLL)
LogResults                     PROCEDURE(*gtTestMethodWithResults OneTest),VIRTUAL 
       END



