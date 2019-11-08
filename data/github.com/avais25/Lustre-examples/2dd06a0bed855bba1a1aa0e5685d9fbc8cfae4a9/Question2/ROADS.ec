node ROADS
  (reset: bool)
returns
  (redH: bool;
  yellowH: bool;
  greenH: bool;
  redV: bool;
  yellowV: bool;
  greenV: bool);

var
  V16_count: bool;
  V42_red: bool;
  V43_yellow: bool;
  V44_green: bool;
  V53_red: bool;
  V54_yellow: bool;
  V55_green: bool;

let
  redH = (false -> (if V16_count then V42_red else (pre redH)));
  yellowH = (false -> (if V16_count then V43_yellow else (pre yellowH)));
  greenH = (true -> (if V16_count then V44_green else (pre greenH)));
  redV = (true -> (if V16_count then V53_red else (pre redV)));
  yellowV = (false -> (if V16_count then V54_yellow else (pre yellowV)));
  greenV = (false -> (if V16_count then V55_green else (pre greenV)));
  V16_count = (true -> (not (pre V16_count)));
  V42_red = (reset -> (pre V43_yellow));
  V43_yellow = (false -> (pre V44_green));
  V44_green = ((not reset) -> (pre V42_red));
  V53_red = ((not reset) -> (pre V54_yellow));
  V54_yellow = (false -> (pre V55_green));
  V55_green = (reset -> (pre V53_red));
tel

