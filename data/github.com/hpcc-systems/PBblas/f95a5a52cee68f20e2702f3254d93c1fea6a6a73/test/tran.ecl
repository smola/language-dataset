/*##############################################################################
## HPCC SYSTEMS software Copyright (C) 2016 HPCC Systems®.  All rights reserved.
############################################################################## */
IMPORT $ as Tests;
IMPORT $.^ as PBblas;
IMPORT PBblas.internal as int;
IMPORT int.MatDims;
IMPORT PBblas.Types;
IMPORT Tests.MakeTestMatrix as tm;
IMPORT Tests.DiffReport as dr;

Layout_Cell := Types.Layout_Cell;

test_rec := RECORD
  STRING test;
  UNSIGNED errors;
  STRING details := '';
END;

N1 := 100;
M1 := 200;
Amat1 := tm.Matrix(N1, M1, 1.0, 1);
Amat2 := tm.Matrix(M1, N1, 1.0, 2);
Cmat1 := tm.Matrix(M1, N1, 1.0, 1);
Cmat2 := DATASET([], Layout_Cell);
alpha := -.9;
beta := 2.1;

// Work item 1 has A and C, Work item 2 only has A
newmat := PBblas.tran(alpha, Amat1+Amat2, beta, Cmat1);
new1 := newmat(wi_id = 1);
new2 := newmat(wi_id = 2);

// TEST 1 -- Validate matrix dimensions
dims_1 := MatDims.FromCells(new1)[1];
dims_2 := MatDims.FromCells(new2)[1];

err1_1 := IF(dims_1.m_rows != M1 OR dims_1.m_cols != N1, 1, 0);
err1_2 := IF(dims_2.m_rows != N1 OR dims_2.m_cols != M1, 1, 0);

test1 := DATASET([{'Test1_1 -- Verify Dimensions', err1_1},
					{'Test1_2 -- Verify Dimensions', err1_2}], test_rec);

// TEST 2 -- Validate Values
// Spot check a few values
tp1_x := 50;
tp1_y := 30;
tp2_x := 23;
tp2_y := 47;
Aval1_1  := Amat1(x=tp1_y AND y=tp1_x)[1].v;
Cval1_1  := Cmat1(x=tp1_x AND y=tp1_y)[1].v;
Aval1_2  := Amat1(x=tp2_y AND y=tp2_x)[1].v;
Cval1_2  := Cmat1(x=tp2_x AND y=tp2_y)[1].v;
Aval2_1  := Amat2(x=tp1_y AND y=tp1_x)[1].v;
Aval2_2  := Amat2(x=tp2_y AND y=tp2_x)[1].v;
expect1_1 := alpha * Aval1_1 + beta * Cval1_1;
expect1_2 := alpha * Aval1_2 + beta * Cval1_2;
expect2_1 := alpha * Aval2_1;  // No C
expect2_2 := alpha * Aval2_2;  // "
result1_1 := new1(x=tp1_x AND y=tp1_y)[1].v;
result1_2 := new1(x=tp2_x AND y=tp2_y)[1].v;
result2_1 := new2(x=tp1_x AND y=tp1_y)[1].v;
result2_2 := new2(x=tp2_x AND y=tp2_y)[1].v;
err2_1_1    := IF(result1_1 != expect1_1, 1, 0);
err2_1_2    := IF(result1_2 != expect1_2, 1, 0);
err2_2_1    := IF(result2_1 != expect2_1, 1, 0);
err2_2_2    := IF(result2_2 != expect2_2, 1, 0);
test2 := DATASET([  {'Test2_1 -- Verify Values', err2_1_1, 'Aval1_1 = ' + Aval1_1 + ', Cval1_1 = ' + Cval1_1 + ', exp1_1 = ' + expect1_1 + ', result1_1 = ' + result1_1},
					{'Test2_2 -- Verify Values', err2_1_2},
					{'Test2_3 -- Verify Values', err2_2_1},
					{'Test2_4 -- Verify Values', err2_2_2}], test_rec);

result := test1 + test2;

EXPORT tran := result;
