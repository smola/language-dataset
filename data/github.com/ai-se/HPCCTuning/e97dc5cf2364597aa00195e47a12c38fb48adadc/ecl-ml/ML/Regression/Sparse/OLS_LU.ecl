// Use LU factorization
IMPORT ML;
IMPORT ML.Types AS Types;
IMPORT Std.Str ;
IMPORT ML.mat as Mat;
NumericField := Types.NumericField;

EXPORT OLS_LU(DATASET(NumericField) X,DATASET(NumericField) Y)
:= MODULE(ML.Regression.Sparse.OLS(X,Y))
  // Use LU factorization
  mLU := Mat.Decomp.LU(Mat.Mul(mXt, mX));
  mL_ := Mat.Decomp.LComp(mLU);
  mU_ := Mat.Decomp.UComp(mLU);
  fsub := Mat.Decomp.f_sub(mL_,Mat.Mul(mXt, mY));
  rslt := Mat.Decomp.b_sub(mU_, fsub);
  SHARED DATASET(Mat.Types.Element) mBetas := rslt;
END;