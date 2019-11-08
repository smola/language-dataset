module Main


import iTasks
from StdEnv import undef



// Types ///////////////////////////////////////////////////////////////////////


:: Name :== String


:: DateOption =
  { day :: Date
  , time :: Time
  }


:: MeetingOption =
  { users :: [Name]
  , moment :: DateOption
  }


:: Table :== [MeetingOption]



// Helpers /////////////////////////////////////////////////////////////////////


elem :: a [a] -> Bool | iTask a
elem x ys =
  undef


users :: [Name]
users =
  [ "Rinus"
  , "Peter"
  , "Mart"
  , "Tim"
  ]



// Stores //////////////////////////////////////////////////////////////////////


initTable :: [DateOption] -> Table
initTable dates =
  undef


updateTable :: Name [DateOption] Table -> Table
updateTable name selected table =
  undef


getDate :: MeetingOption -> DateOption
getDate meeting = meeting.moment



// Tasks ///////////////////////////////////////////////////////////////////////


main :: Task MeetingOption
main =
  defineMeetingPurpose >>= \purpose ->
  selectDatesToPropose >>= \dates ->
  selectAttendencees >>= \others ->
  askOthers purpose dates others >>= \options ->
  selectMeetingDate purpose options >>= \chosen ->
  viewInformation "Date chosen:" [] chosen


defineMeetingPurpose :: Task String
defineMeetingPurpose =
  undef


selectDatesToPropose :: Task [DateOption]
selectDatesToPropose =
  undef


selectAttendencees :: Task [Name]
selectAttendencees =
  undef


askOthers :: String [DateOption] [Name] -> Task Table
askOthers purpose dates others =
  withShared (initTable dates) (\table ->
    undef
  )


ask :: String Name (Shared Table) -> Task [DateOption]
ask purpose name table =
  viewSharedInformation ( name, "Current Responses" ) [] table ||- (
    undef
  )


selectMeetingDate :: String Table -> Task MeetingOption
selectMeetingDate purpose table =
  undef



// Boilerplate /////////////////////////////////////////////////////////////////


derive class iTask DateOption, MeetingOption


Start :: *World -> *World
Start world = startEngine (main <<@ InWindow) world


(>>?) infixl 1 :: (Task a) [( String, a -> Bool, a -> Task b )] -> Task b | iTask a & iTask b
(>>?) task options = task >>* map trans options
where
  trans ( a, p, t ) = OnAction (Action a) (ifValue p t)


($=) infixr 2 :: (ReadWriteShared r w) (r -> w) -> Task w | iTask r & iTask w
($=) share fun = upd fun share
