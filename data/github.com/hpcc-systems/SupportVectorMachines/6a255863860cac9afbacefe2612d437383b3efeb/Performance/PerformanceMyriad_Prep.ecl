/*
################################################################################
## HPCC SYSTEMS software Copyright (C) 2016 HPCC Systems®.  All rights reserved.
################################################################################
*/
/**
  * Data and model preparation for performance testing of SupportVectorMachines
  * bundle.
  */

IMPORT PBblas;
IMPORT PBblas.internal as int;
IMPORT int.Types as iTypes;
IMPORT PBblas.Types;
IMPORT int.MatDims;
IMPORT PBblas.test as Tests;
IMPORT Tests.MakeTestMatrix as tm;
IMPORT ML_Core as ML;
IMPORT ML.Types as Core_Types;
IMPORT $.^ as SVM;

Layout_Cell := Types.Layout_Cell;
NumericField := Core_Types.NumericField;
Layout_Model := Core_Types.Layout_Model;

N := 1000;   // Number of rows in X
M := 20;     // Number of columns in X

density := 1.0; // 1.0 is fully dense. 0.0 is empty.

// Generate test data for X and Y

X_LC := tm.RandomMatrix(N, M, density, 1);
Y_LC := tm.RandomMatrix(N, 1, density, 1);
Y_min := MIN(Y_LC, v);
Y_max := MAX(Y_LC, v);

NumericField asNumericFieldX(Layout_Cell L) := TRANSFORM
  SELF.wi     := L.wi_id;
  SELF.id     := L.x;
  SELF.number := L.y;
  SELF.value  := L.v;
END;
NumericField asNumericFieldY(Layout_Cell L) := TRANSFORM
  SELF.wi     := L.wi_id;
  SELF.id     := L.x;
  SELF.number := L.y;
  SELF.value  := ROUND((L.v - Y_min) / (Y_max - Y_min)) * 2 - 1;
END;

X := PROJECT(X_LC, asNumericFieldX(LEFT));
Y := PROJECT(Y_LC, asNumericFieldY(LEFT));

NumericField make_myriad(NumericField d, UNSIGNED c) := TRANSFORM
  SELF.wi := c;
  SELF    := d;
END;

MyrStruct := RECORD
  DATASET(NumericField) X;
  DATASET(NumericField) Y;
  DATASET(Layout_Model) SVC;
  DATASET(Layout_Model) SVR;
END;

X_myr(INTEGER wi_cnt)   := NORMALIZE(X, wi_cnt, make_myriad(LEFT, COUNTER));
Y_myr(INTEGER wi_cnt)   := NORMALIZE(Y, wi_cnt, make_myriad(LEFT, COUNTER));
SVC_myr(INTEGER wi_cnt) := SVM.SVC().GetModel(X_myr(wi_cnt), ML.Discretize.ByRounding(Y_myr(wi_cnt)));
SVR_myr(INTEGER wi_cnt) := SVM.SVR(NAMED X := X_myr(wi_cnt), NAMED Y := Y_myr(wi_cnt)).GetModel();

OUTPUT(X_myr(1),,'GetModelPerfMyr_X_1.dat', OVERWRITE);
OUTPUT(X_myr(2),,'GetModelPerfMyr_X_2.dat', OVERWRITE);
OUTPUT(X_myr(4),,'GetModelPerfMyr_X_4.dat', OVERWRITE);
OUTPUT(X_myr(8),,'GetModelPerfMyr_X_8.dat', OVERWRITE);
OUTPUT(X_myr(16),,'GetModelPerfMyr_X_16.dat', OVERWRITE);
OUTPUT(X_myr(20),,'GetModelPerfMyr_X_20.dat', OVERWRITE);
OUTPUT(X_myr(21),,'GetModelPerfMyr_X_21.dat', OVERWRITE);
OUTPUT(X_myr(22),,'GetModelPerfMyr_X_22.dat', OVERWRITE);
OUTPUT(X_myr(25),,'GetModelPerfMyr_X_25.dat', OVERWRITE);
OUTPUT(X_myr(30),,'GetModelPerfMyr_X_30.dat', OVERWRITE);
OUTPUT(X_myr(40),,'GetModelPerfMyr_X_40.dat', OVERWRITE);
OUTPUT(X_myr(41),,'GetModelPerfMyr_X_41.dat', OVERWRITE);
OUTPUT(X_myr(50),,'GetModelPerfMyr_X_50.dat', OVERWRITE);
OUTPUT(X_myr(64),,'GetModelPerfMyr_X_64.dat', OVERWRITE);
OUTPUT(X_myr(256),,'GetModelPerfMyr_X_256.dat', OVERWRITE);
OUTPUT(X_myr(1024),,'GetModelPerfMyr_X_1024.dat', OVERWRITE);
OUTPUT(Y_myr(1),,'GetModelPerfMyr_Y_1.dat', OVERWRITE);
OUTPUT(Y_myr(2),,'GetModelPerfMyr_Y_2.dat', OVERWRITE);
OUTPUT(Y_myr(4),,'GetModelPerfMyr_Y_4.dat', OVERWRITE);
OUTPUT(Y_myr(8),,'GetModelPerfMyr_Y_8.dat', OVERWRITE);
OUTPUT(Y_myr(16),,'GetModelPerfMyr_Y_16.dat', OVERWRITE);
OUTPUT(Y_myr(20),,'GetModelPerfMyr_Y_20.dat', OVERWRITE);
OUTPUT(Y_myr(21),,'GetModelPerfMyr_Y_21.dat', OVERWRITE);
OUTPUT(Y_myr(22),,'GetModelPerfMyr_Y_22.dat', OVERWRITE);
OUTPUT(Y_myr(25),,'GetModelPerfMyr_Y_25.dat', OVERWRITE);
OUTPUT(Y_myr(30),,'GetModelPerfMyr_Y_30.dat', OVERWRITE);
OUTPUT(Y_myr(40),,'GetModelPerfMyr_Y_40.dat', OVERWRITE);
OUTPUT(Y_myr(41),,'GetModelPerfMyr_Y_41.dat', OVERWRITE);
OUTPUT(Y_myr(50),,'GetModelPerfMyr_Y_50.dat', OVERWRITE);
OUTPUT(Y_myr(64),,'GetModelPerfMyr_Y_64.dat', OVERWRITE);
OUTPUT(Y_myr(256),,'GetModelPerfMyr_Y_256.dat', OVERWRITE);
OUTPUT(Y_myr(1024),,'GetModelPerfMyr_Y_1024.dat', OVERWRITE);

OUTPUT(SVC_myr(1),,'PredictPerfMyr_SVC_1.dat', OVERWRITE);
OUTPUT(SVC_myr(2),,'PredictPerfMyr_SVC_2.dat', OVERWRITE);
OUTPUT(SVC_myr(4),,'PredictPerfMyr_SVC_4.dat', OVERWRITE);
OUTPUT(SVC_myr(8),,'PredictPerfMyr_SVC_8.dat', OVERWRITE);
OUTPUT(SVC_myr(16),,'PredictPerfMyr_SVC_16.dat', OVERWRITE);
OUTPUT(SVC_myr(20),,'PredictPerfMyr_SVC_20.dat', OVERWRITE);
OUTPUT(SVC_myr(21),,'PredictPerfMyr_SVC_21.dat', OVERWRITE);
OUTPUT(SVC_myr(22),,'PredictPerfMyr_SVC_22.dat', OVERWRITE);
OUTPUT(SVC_myr(25),,'PredictPerfMyr_SVC_25.dat', OVERWRITE);
OUTPUT(SVC_myr(30),,'PredictPerfMyr_SVC_30.dat', OVERWRITE);
OUTPUT(SVC_myr(40),,'PredictPerfMyr_SVC_40.dat', OVERWRITE);
OUTPUT(SVC_myr(41),,'PredictPerfMyr_SVC_41.dat', OVERWRITE);
OUTPUT(SVC_myr(50),,'PredictPerfMyr_SVC_50.dat', OVERWRITE);
OUTPUT(SVC_myr(64),,'PredictPerfMyr_SVC_64.dat', OVERWRITE);
OUTPUT(SVC_myr(256),,'PredictPerfMyr_SVC_256.dat', OVERWRITE);
OUTPUT(SVC_myr(1024),,'PredictPerfMyr_SVC_1024.dat', OVERWRITE);
OUTPUT(SVR_myr(1),,'PredictPerfMyr_SVR_1.dat', OVERWRITE);
OUTPUT(SVR_myr(2),,'PredictPerfMyr_SVR_2.dat', OVERWRITE);
OUTPUT(SVR_myr(4),,'PredictPerfMyr_SVR_4.dat', OVERWRITE);
OUTPUT(SVR_myr(8),,'PredictPerfMyr_SVR_8.dat', OVERWRITE);
OUTPUT(SVR_myr(16),,'PredictPerfMyr_SVR_16.dat', OVERWRITE);
OUTPUT(SVR_myr(20),,'PredictPerfMyr_SVR_20.dat', OVERWRITE);
OUTPUT(SVR_myr(21),,'PredictPerfMyr_SVR_21.dat', OVERWRITE);
OUTPUT(SVR_myr(22),,'PredictPerfMyr_SVR_22.dat', OVERWRITE);
OUTPUT(SVR_myr(25),,'PredictPerfMyr_SVR_25.dat', OVERWRITE);
OUTPUT(SVR_myr(30),,'PredictPerfMyr_SVR_30.dat', OVERWRITE);
OUTPUT(SVR_myr(40),,'PredictPerfMyr_SVR_40.dat', OVERWRITE);
OUTPUT(SVR_myr(41),,'PredictPerfMyr_SVR_41.dat', OVERWRITE);
OUTPUT(SVR_myr(50),,'PredictPerfMyr_SVR_50.dat', OVERWRITE);
OUTPUT(SVR_myr(64),,'PredictPerfMyr_SVR_64.dat', OVERWRITE);
OUTPUT(SVR_myr(256),,'PredictPerfMyr_SVR_256.dat', OVERWRITE);
OUTPUT(SVR_myr(1024),,'PredictPerfMyr_SVR_1024.dat', OVERWRITE);
