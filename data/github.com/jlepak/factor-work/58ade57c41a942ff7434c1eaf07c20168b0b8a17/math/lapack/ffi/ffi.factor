! Copyright (C) 2011 Your name.
! See http://factorcode.org/license.txt for BSD license.
USING: alien.fortran kernel math.lapack.config namespaces ;
FROM: alien.libraries => deploy-library ;
IN: math.lapack.ffi

<<
"lapack" lapack-library lapack-fortran-abi [ get ] bi@
add-fortran-library

deploy-lapack? get [ "lapack" deploy-library ] when
>>

LIBRARY: lapack

! Single precision

! Cholesky decomposition
SUBROUTINE: SPOTRF
    ( CHARACTER*1 UPLO, INTEGER N, !REAL(*) A, INTEGER LDA, !INTEGER INFO ) ;

FUNCTOR: declare-lapack-routines ( BASE FTYPE -- )

ROUTINE  IS ${TAG}${BASE}
FTYPE IS 
VECTOR         IS ${TYPE}-blas-vector
XDOTU          IS ${C}DOTU
XDOTC          IS ${C}DOTC
XXNRM2         IS ${S}${C}NRM2
XXASUM         IS ${S}${C}ASUM

WHERE

M: VECTOR V.
    (prepare-dot) XDOTU ;
M: VECTOR V.conj
    (prepare-dot) XDOTC ;
M: VECTOR Vnorm
    (prepare-nrm2) XXNRM2 ;
M: VECTOR Vasum
    (prepare-nrm2) XXASUM ;

;FUNCTOR

SUBROUTINE: SGESV
    ( INTEGER N, INTEGER NRHS, !REAL(*) A, INTEGER LDA, !INTEGER(*) IPIV,
      !REAL(*) B, INTEGER LDB, !INTEGER INFO ) ;
