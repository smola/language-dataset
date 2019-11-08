/*##############################################################################
## HPCC SYSTEMS software Copyright (C) 2018 HPCC Systems®.  All rights reserved.
############################################################################## */
/**
  * This module provides tests for the ModelOps2 module
  */
IMPORT $.^ as ML_Core;
IMPORT ML_Core.Types;
IMPORT ML_Core.ModelOps2;
Layout_Model2 := Types.Layout_Model2;

mod := DATASET([
                  {1, 1.1, [1, 5, 1, 1]},
                  {1, 1.2, [1, 5, 1, 2]},
                  {1, 1.3, [1, 5, 1, 3]},
                  {1, 2.1, [1, 5, 2, 1]},
                  {1, 2.2, [1, 5, 2, 2]},
                  {1, 2.3, [1, 5, 2, 3]},
                  {1, .16, [1, 6, 1, 1]},
                  {1, .25, [2, 5, 1, 1]}
                  ], Layout_Model2);
rslt1 := ModelOps2.ToNumericField(mod, [1,5]);

rslt2 := ModelOps2.FromNumericField(rslt1, [5,10]);

rslt3_1 := ModelOps2.GetItem(rslt2, [5, 10, 2, 3]);
rslt3_2 := ModelOps2.GetItem(rslt2, [5, 10, 2, 3], 2);  // Shouldn't find it -- no wi = 2

rslt4 := ModelOps2.SetItem(rslt2, 1, [5, 10, 2, 4], 2.4);

rslt5 := ModelOps2.Extract(rslt4, [5, 10, 2]);

rslt6 := ModelOps2.Insert(mod, rslt5, [1, 6, 1, 1]);

OUTPUT(mod, NAMED('Model'));
OUTPUT(rslt1, NAMED('ToNF'));
OUTPUT(rslt2, NAMED('FromNF'));
OUTPUT(rslt3_1, NAMED('GetItem'));
OUTPUT(rslt3_2, NAMED('GetItemNotFound'));
OUTPUT(rslt4, NAMED('SetItem'));
OUTPUT(rslt5, NAMED('Extract'));
OUTPUT(rslt6, NAMED('Insert'));
