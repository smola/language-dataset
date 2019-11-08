! http://clarionedge.com, http://fushnisoft.com
! Licensed under the MIT license: https://github.com/fushnisoft/ClarionMetroWizard/blob/master/LICENSE.txt
_ce_TabList_  EQUATE(1)
  Omit('!!!Docs!!!')
! ============================
! ce_TabList Class
! ============================
!
! .. _class-ce_tablist:
!
! .. describe:: ce_TabList Class(ce_BaseWindowComponent),Implements(WindowComponent)
!
!   This class will take a Sheet control and convert it into list box representation of the TABs on the left.
!   It is designed with a fairly narrow target but as long as you stay within the guidelines it seems to work well :)
!
! Usage
! =====
!
! Just add a new instance of this class to a window, follow the requirements below and call the init method and you should be good to go.
!
! Requirements
! ============

! * A Sheet control with TAB location set to LEFT
! * The TAB width needs to be set manually rather than left at zero
! * The class will *not* convert nested Sheets into a tree structure.
! * There are probably a lot of hard coded values for position and size of elements.
!
!!!Docs!!!
  PRAGMA('project(#pragma link(header_grey.bmp))')

ce_tabQ_Type                    QUEUE,TYPE
name                           CSTRING(255)
cellStyle                      LONG
cellTooltip                    CSTRING(255)
                             END

  include('ce_BaseWindowComponent.inc'),ONCE
  include('ABWINDOW.INC'),ONCE
ce_TabList          Class(ce_BaseWindowComponent),Implements(WindowComponent),Type,Module('ce_TabList.Clw'),LINK('ce_TabList.Clw',1)
! Properties
Omit('!!!Docs!!!')

Class Properties
================

This document will not list all the public properties, take a look at the source code for them for now.

!!!Docs!!!

tabQ                  &ce_tabQ_Type
sheetFeq              SIGNED
listFeq               SIGNED
listLineHeightAdjustment  LONG
listValuesUpperCase   BYTE
listValuesBold        BYTE
lastTabFeq            SIGNED
boxFeq                SIGNED
boxMargin             LONG
backgroundImageFeq    SIGNED
backgroundImage       CSTRING(255)
lastChoice            LONG
! Methods
Construct             PROCEDURE()
Destruct              PROCEDURE() ,VIRTUAL
Init                  PROCEDURE(WindowManager pWM, SIGNED pSheetFeq, BYTE pSkipChecksAndOptions=FALSE, BYTE pHideCaption=FALSE) ,VIRTUAL 
SetListFrom           PROCEDURE() ,VIRTUAL
SetupNoSheet          PROCEDURE(BYTE pSkipChecksAndOptions=FALSE) ,VIRTUAL 
TakeAccepted          PROCEDURE() ,VIRTUAL
TakeNewSelection      PROCEDURE() ,VIRTUAL
TakeEvent             PROCEDURE() ,VIRTUAL
Replace               PROCEDURE(STRING pFrom, STRING pFind, STRING pReplace) ,STRING
                     END
!_EndOfInclude_
