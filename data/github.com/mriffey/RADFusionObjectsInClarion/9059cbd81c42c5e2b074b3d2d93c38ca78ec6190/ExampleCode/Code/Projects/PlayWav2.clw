! Copyright 2002 - RADFusion, Inc.
  MEMBER

  MAP
   INCLUDE('SNDAPI.CLW'),ONCE
  END

  INCLUDE('OOP7.CLW','PLAYWAVE'),ONCE

PlayWave.Play                 PROCEDURE ()

  CODE
  MESSAGE('I am Play!','Base Class',ICON:Exclamation)
  SndPlaySound(SELF.WaveName,1)
  RETURN

PlayWave.CallVirtual    PROCEDURE
! Call whatever virtual is currently replacing the dummy PARENT.Play
! method shown above.

  CODE
  MESSAGE('I am CallVirtual!','Base Class',ICON:Exclamation)
  SELF.Play ()                          !This calls either V1.Play or V2.Play depending on which
  MESSAGE('Back CallVirtual')           ! one called this method.  SELF can contain either V1 or V2
                                        ! so a base class is now calling "down" or "forward" to
                                        ! a child or derived class.

  














