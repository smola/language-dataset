:Namespace Math

    ⎕IO←1
    ⎕ML←1                         ⍝ ∊ is enlist

    L←⎕av[3+⎕io]
    _←⍬
    _,←'MATH - a collection of mathematical functions.'
    _,←L,(⎕ucs 9472 9472 9472 9472)
    _,←L,'The functions in this workspace include complex arithmetic. Dyalog '
    _,←L,'represents complex numbers a+bi as aJb.'
    _,←L,''
    _,←L,'The workspace requires dynamic link libraries LAPACK.DLL and FFTW.DLL.'
    _,←L,'The interpreter searches for these DLLs in the following order:'
    _,←L,''
    _,←L,'      the directory containing the DYALOG.EXE executable (recommended),'
    _,←L,'      the current directory,'
    _,←L,'      the windows\system directory,'
    _,←L,'      the windows directory,'
    _,←L,'      a directory on the DOS PATH.'
    _,←L,''
    _,←L,73⍴'.'
    _,←L,'Eigen'
    _,←L,(⎕ucs 5/9472)
    _,←L,'Monadic function ∇Eigen∇ takes an n×n real or complex matrix and returns'
    _,←L,'an (n+1)×n result of Eigen: Values⍪⍉↑Vectors:'
    _,←L,''
    _,←L,'    ',(⎕ucs 9484,(3/9472),9516,(3/9472),9516,(3/9472),9516,(3/9472),9488)
    _,←L,'    ',(⎕ucs 9474),'  v a l u e s  ',(⎕ucs 9474 32 32 9472 9472),' Eigen values'
    _,←L,'    ',(⎕ucs 9500,(3/9472),9532,(3/9472),9532,(3/9472),9532,(3/9472),9508)
    _,←L,'    ',(⎕ucs 9474),' v ',(⎕ucs 9474),' v ',(⎕ucs 9474),' v ',(⎕ucs 9474),' v ',(⎕ucs 9474 32 32 9488)
    _,←L,'    ',(⎕ucs 9500),' e ',(⎕ucs 9532),' e ',(⎕ucs 9532),' e ',(⎕ucs 9532),' e ',(⎕ucs 9508 32 32 9474)
    _,←L,'    ',(⎕ucs 9474),' c ',(⎕ucs 9474),' c ',(⎕ucs 9474),' c ',(⎕ucs 9474),' c ',(⎕ucs 9474 32 32 9474)
    _,←L,'    ',(⎕ucs 9500),' t ',(⎕ucs 9532),' t ',(⎕ucs 9532),' t ',(⎕ucs 9532),' t ',(⎕ucs 9508 32 32 9500 9472),' Eigen vectors.'
    _,←L,'    ',(⎕ucs 9474),' o ',(⎕ucs 9474),' o ',(⎕ucs 9474),' o ',(⎕ucs 9474),' o ',(⎕ucs 9474 32 32 9474)
    _,←L,'    ',(⎕ucs 9500),' r ',(⎕ucs 9532),' r ',(⎕ucs 9532),' r ',(⎕ucs 9532),' r ',(⎕ucs 9508 32 32 9474)
    _,←L,'    ',(⎕ucs 9474),'   ',(⎕ucs 9474),'   ',(⎕ucs 9474),'   ',(⎕ucs 9474),'   ',(⎕ucs 9474 32 32 9496)
    _,←L,'    ',(⎕ucs 9492,(3/9472),9524,(3/9472),9524,(3/9472),9524,(3/9472),9496)
    _,←L,''
    _,←L,'∇Eigen∇ has been constructed from LAPACK (Linear Algebra Package) double-'
    _,←L,'precision C functions which are available as source code from'
    _,←L,'www.netlib.org/lapack'
    _,←L,''
    _,←L,'LAPACK.DLL contains these C functions. They can all be called individually'
    _,←L,'through ⎕NA. You need to examining each function''s parameters in the'
    _,←L,'corresponding *.C file (downloaded from the internet) in order to correctly'
    _,←L,'specify their result and argument types.'
    _,←L,''
    _,←L,'For example, look at the ⎕NA call for dgeev_ in Eigen. Compare this with'
    _,←L,'the parameters specified in file DGEEV.C . All the other double-precision'
    _,←L,'real and complex LAPACK functions can be called in this way using ⎕NA.'
    _,←L,''
    _,←L,'Trace the following line in order to see ∇Eigen∇ in action:'
    _,←L,''
    _,←L,'      test.eigen    ⍝ run and trace 10 times'
    _,←L,''
    _,←L,73⍴'.'
    _,←L,'Domino'
    _,←L,6⍴(⎕ucs 9472)
    _,←L,'∇Domino∇ is equivalent to APL''s primitive ⌹ function.'
    _,←L,''
    _,←L,'Trace the following line in order to see ∇Domino∇ in action:'
    _,←L,''
    _,←L,'      test.domino   ⍝ run and trace'
    _,←L,''
    _,←L,73⍴'.'
    _,←L,'Fourier'
    _,←L,7⍴(⎕ucs 9472)
    _,←L,'∇Fourier∇ takes a real or complex array right argument.'
    _,←L,'The left argument signifies:'
    _,←L,''
    _,←L,' 1: Fourier Transform (default).'
    _,←L,'¯1: Inverse Fourier Transform.'
    _,←L,''
    _,←L,'∇Fourier∇ has been constructed from FFTW (the Fastest Fourier Transform'
    _,←L,'in the World). The FFTW source code of C functions is available from'
    _,←L,'www.fftw.org'
    _,←L,''
    _,←L,'The FFTW.DLL contains all these functions. They can be called individually'
    _,←L,'through ⎕NA after examining each function''s parameters in the FFTW'
    _,←L,'documentation.'
    _,←L,''
    _,←L,'Check that'
    _,←L,''
    _,←L,'      {⍵=¯1 Fourier Fourier ⍵}↓?((5?5),2)⍴100'
    _,←L,''
    _,←L,'Trace the following line in order to see ∇Fourier∇ in action:'
    _,←L,''
    _,←L,'      test.fourier   ⍝ run and trace'
    _,←L,''
    _,←L,'Note that C cover functions dft and idft have been added to the DLL.'
    _,←L,''
    _,←L,'These functions are:'
    _,←L,''
    _,←L,'// dft.c discrete fourier transform'
    _,←L,''
    _,←L,'#include <fftw.h>'
    _,←L,''
    _,←L,'void dft(int *rank, const int *shape, double *data)'
    _,←L,,'{'
    _,←L,'   fftwnd_plan plan;'
    _,←L,'   plan = fftwnd_create_plan(*rank, shape, FFTW_FORWARD, FFTW_IN_PLACE);'
    _,←L,'   fftwnd_one(plan, (void*)data, 0);'
    _,←L,'   fftwnd_destroy_plan(plan);'
    _,←L,,'}'
    _,←L,''
    _,←L,'// idft.c inverse discrete fourier transform'
    _,←L,''
    _,←L,'#include <fftw.h>'
    _,←L,''
    _,←L,'void idft(int *rank, const int *shape, double *data)'
    _,←L,,'{'
    _,←L,'   fftwnd_plan plan;'
    _,←L,'   plan = fftwnd_create_plan(*rank, shape, FFTW_BACKWARD, FFTW_IN_PLACE);'
    _,←L,'   fftwnd_one(plan, (void*)data, 0);'
    _,←L,'   fftwnd_destroy_plan(plan);'
    _,←L,,'}'
    _,←L,''
    _,←L,'To see an example of these functions, type:'
    _,←L,''
    _,←L,'      test.eigen ⋄ test.domino ⋄ test.fourier'
    help←_

    ⎕ex¨ 'L' '_'

    Domino←⌹

    Join←{⍺⍪⍉n n⍴⍵}               ⍝ hang vectors below values
    Real←1289≠⎕dr                 ⍝ is mat real?
    Sym←{w≡+⍉w←⍵+(○+*)≢⎕CT←2*¯32} ⍝ offset coarse tolerance for max fuzz symmetric/hermetian check.
    J←{⍺+¯11○⍵}                   ⍝ J's j. verb
    V2J←J/¨                       ⍝ convert old vector notation (matrix of real-cmpx pairs) to new 1J2 notation
    J2V←9 11∘○¨                   ⍝ convert new 1J2 notation to old vector notation (matrix of real-cmpx pairs)
    Call←{(⍎⍺⍺ Assoc ⍵⍵)⍵}        ⍝ Call external fn ⍺⍺ with arg ⍺, associate with types ⍵⍵ if necessary
      Assoc←{3=⎕NC ⍺:⍺
          plat←⊃# ⎕WG'APLVersion'
          bits←¯2↑'32',⎕D∩⍨plat
          ext←'.so'/⍨'Linux'≡5↑plat
          call←'lapack',bits,ext,'|',⍺,' ',⍵
          0::'*** ERROR ',(⍕⎕EN),': ⎕NA''',call,''''
          ⎕NA call}

      Dsyev←{
    ⍝  JOBZ  UPLO  N  A  LDA  W  WORK  LWORK  INFO
          types←'<C1 <C1 <I4 =F8[] <I4 >F8[] >F8[] <I4 >I4'
          args←'V' 'L'n(,⍵)n n(¯1+3×n)(¯1+3×n)0
          2 1 3⊃¨⊂1 1 0 1/('dsyev_'Call types)args ⍝ call external fn.
      }

      Dgeev←{
   ⍝  JOBVL  JOBVR  N  A  LDA  WR  WI  VL  LDVL  VR  LDVR  WORK  LWORK  INFO
          types←'<C1 <C1 <I4 =F8[] <I4 >F8[] >F8[] >I4 <I4 >F8[] <I4 >F8[] <I4 >I4'
          args←'N' 'V'n(,⍉mat)n n n 0 1(n×n)n(4×n)(4×n)0
          (real cmpx vecs code)←0 1 1 0 1 0 1/('dgeev_'Call types)args ⍝ call external fn.
     
          vals←real J cmpx        ⍝ combine parts
          ∧/0=cmpx:real vecs code ⍝ all-real case
          vr←⍉n n⍴vecs            ⍝ build matrix
          eii←vr×⍤1⊢cmpx=0
          wi←cmpx,0                           ⍝ {(⍵≠0)^⍵=-1⌽⍵}WI,0
          pair←(wi≠0)∧wi=-1⌽wi                ⍝ problem if 3 same in a row!
          pair←pair/⍳⍴pair
          eii[;pair]←vr[;pair]J vr[;1+pair]
          eii[;1+pair]←vr[;pair]J-vr[;1+pair]
          vecs←⍉eii
          vals vecs code                      ⍝ return parts
      }

      Zheev←{
       ⍝  JOBZ  UPLO  N  A  LDA  W  WORK  LWORK  RWORK  INFO
          types←'<C1 <C1 <I4 =J16[] <I4 >F8[] >F8[] <I4 >F8[] >I4'
          args←'V' 'L'n(∊⍉mat)n n(¯2+4×n)(¯1+2×n)(¯2+3×n)0
          2 1 3⊃¨⊂1 1 0 0 1/('zheev_'Call types)args ⍝ call external fn.
      }

      Zgeev←{
       ⍝  JOBVL  JOBVR  N  A  LDA  W  VL  LDVL  VR  LDVR  WORK  LWORK  RWORK  INFO
          types←'<C1 <C1 <I4 =J16[] <I4 >J16[] <I4 <I4 >J16[] <I4 >F8[] <I4 >F8[] >I4'
          args←'N' 'V'n(∊⍉mat)n n 0 1(n×n)n(4×n)(2×n)(2×n)0
          0 1 1 0 0 1/('zgeev_'Call types)args ⍝ call external fn.
      }

    ∇ vv←Eigen mat;vecs;vals;code;n
      n←≢mat
     
      :If Real mat
          :If (Sym mat)∧2≠n ⍝ symmetric
              (vals vecs code)←Dsyev mat
     
          :Else ⍝ general
              (vals vecs code)←Dgeev mat
          :EndIf
     
      :Else ⍝ complex
          :If Sym mat ⍝ hermitian
              (vals vecs code)←Zheev mat
     
          :Else ⍝ general
              (vals vecs code)←Zgeev mat
          :EndIf
     
      :EndIf
      ⎕SIGNAL code/11
      vv←vals Join vecs
    ∇

    Fourier←{⍺←1 ⋄ ⍵ RunFn 'idft' 'dft'⊃⍨1+0⌈⍺}
    ∇ data←data RunFn func;rank;shape;count;plat;bits;ext ⍝ n-D Discrete Fourier Transformation
      (rank count)←(⍴,×/)shape←⍴data
      :If 1<count ⍝ non-scalar
          :If 3≠⎕NC func ⍝ associate external function if it does not exist
              plat←⊃# ⎕WG'APLVersion'
              bits←¯2↑'32',⎕D∩⍨plat
              ext←'.so'/⍨'Linux'≡5↑plat
              ⎕NA'fftw',bits,ext,'|',func,'<I4 <I4[] =J16[]'
          :EndIf
          data←(count*0.5)÷⍨shape⍴(⍎func)rank shape,⊂∊data ⍝ Normalized Fourier/Inverse Fourier transform
      :EndIf
    ∇

    ∇ Hnt←N Hermite T;J;facts ⍝  Hermite Polynomials
     ⍝eg          2  Hermite .3
     ⍝     (⎕IO-⍨⍳4) Hermite (⍳5)-2+⎕IO
     ⍝     (⎕IO-⍨⍳3) Hermite (⍳11)-5+⎕IO
     ⍝     (⎕IO-⍨⍳9) Hermite 10×{(((~⎕IO)+⍳⍵)-(⍵+1)÷2)÷(⍵-1)÷2}5
     
     ⍝  N={n:n ∊ Z+} , T={t:t ∊ R}
     
     ⍝                2   n ┌     2 ┐
     ⍝            n  t  d   │  -t   │
     ⍝ H (t)= (-1)  e   --- │ e     │
     ⍝  n                 n │       │
     ⍝                  dt  └       ┘
     ⍝             max
     ⍝            ____          n - 2j
     ⍝            \        j   2          n-2j
     ⍝ H (t)= n!  /    (¯1)   ---------- t
     ⍝  n         ¯¯¯¯        j!(n-2j)!
     ⍝            j=0
     
     ⍝ where max=n/2 if n even, max=(n-1)/2 if n odd
     
      N←,N ⋄ T←,T
      J←⎕IO-⍨⍳¨1+(N-2|N)÷2
      facts←(¯1*J)×(2*N-2×J)÷(!J)×!N-2×J
      Hnt←(!N)×[⎕IO]+⌿¨facts×[⎕IO]⍉T∘.*N-2×J
    ∇

    ∇ Lnt←N Laguerre T;J;facts ⍝  Laguerre Polynomials
     ⍝eg   2  Laguerre 1.3
     ⍝     (⎕IO-⍨⍳4) Laguerre (⍳60)÷10
     ⍝     (⍳9) Laguerre (⍳60)÷10
     
     ⍝  N={n:n ∊ Z+} , T={t:t ∊ R+}
     
     ⍝           t   n  ┌        ┐
     ⍝          e   d   │  n  -t │
     ⍝ L (t)=   --  --- │ t  e   │
     ⍝  n       n!    n │        │
     ⍝              dt  └        ┘
     
     ⍝            n
     ⍝          ____     j ┌   ┐
     ⍝          \    (¯1)  │ n │   j
     ⍝ L (t)=   /    ----  │   │  t
     ⍝  n       ¯¯¯¯  j!   │ j │
     ⍝          j=0        └   ┘
     
     
      N←,N ⋄ T←,T
      J←⎕IO-⍨⍳¨1+N
      facts←(¯1*J)×(J!N)÷(!J)
      Lnt←+⌿¨facts×[⎕IO]⍉T∘.*J
    ∇

    ∇ Pnx←N Legendre X;J;facts ⍝  Legendre Polynomials
     ⍝eg   2  Legendre .3 .4 .7
     ⍝     (⎕IO-⍨⍳4) Legendre 4÷⍨(⍳5)-2+⎕IO
     ⍝     (⎕IO-⍨⍳3) Legendre 10÷⍨(⍳11)-5+⎕IO
     ⍝  N={n:n ∊ Z+} , X={x:x ∊ [-1,+1]}
     ⍝                  n
     ⍝          1     d   ┌   2     n ┐
     ⍝ P (x)= ------  --- │ (x  - 1)  │
     ⍝  n      n        n └           ┘
     ⍝        2  n!   dx
     
     ⍝          max
     ⍝         ____
     ⍝         \        j       (2n - 2j)!          n-2j
     ⍝ P (x)=  /    (¯1)   --------------------    x
     ⍝  n      ¯¯¯¯         n
     ⍝         j=0         2  j! (n-j)! (n-2j)!
     
     ⍝ where max=n/2 if n even, and max=(n-1)/2 if n odd
      N←,N
      J←⎕IO-⍨⍳¨1+(N-2|N)÷2
      facts←(¯1*J)×(!(2×N)-2×J)÷(2*N)×(!J)×(!N-J)×!N-2×J
      Pnx←⍉+/¨(X∘.*N-2×J)×[1+⎕IO]facts
    ∇

    ∇ lx
      ⎕←''
      ⍝:If '64'≡¯2↑⊃⎕WG'APLVersion'
      ⍝    ⎕←'This workspace is not currently supported on 64-bit interpreters'
      ⍝:Else
      ⎕←'help'
      ⍝:EndIf
    ∇

    :namespace test

        ∇ ok←QA
          ok←⍬
          :Trap 0
              :For fn :In 'domino' 'eigen' 'fourier'
                  ok,←⍎fn
              :EndFor
          :Else
              '*** FAILED: math.test.',fn
              ok←0
          :EndTrap
          ok←∧/ok
        ∇

        ∇ ok←domino;⎕CT;⎕PP;n;i;j;diag;a;conj;symm;anti;herm;fuzz;Aij;Bia;Xja;LHS;RHS;x;real;⎕TRAP;⎕PATH;c0;c1;c2;c3;c4
     ⍝ 48 random matrices
          ok←1
          ⍝⎕PATH,←' ↑'
          ⎕TRAP←(11 12)'C' '→0'
          n←15
          :For c0 :In ⍳3
              j←?n
              i←j+?n     ⍝ i>j
              a←?n
              x←{                     ⍝ complex multiply
                  sign←2 2⍴1 ¯1 1 1
                  +/sign×0 1⊖(2↑⍺)∘.×2↑⍵
              }
              real←{1=≡,⍵}
              fuzz←{⎕CT←2*¯32 ⋄ (⍺+○1)⍺⍺ ⍵+○1}
              conj←{⍵×⊂1 ¯1}         ⍝ complex conjugate.
              anti←{⍵-⍉⍵}            ⍝ antisymmetrify
              symm←{⍵+⍉⍵}            ⍝ symmetrify
              herm←{real cmpx←⊂[1 2]↑⍵ ⋄ (symm real),¨anti cmpx}  ⍝ hermitianise.
              :For c1 :In 0 1
                  :For c2 :In 0 1
                      :For c3 :In 0 1
                          :For c4 :In 0 1
                              :If c1  ⍝ square          ⍝ solve Ax=b
                                  :If c2   ⍝ real
                                      Aij←(-n÷2)+?i i⍴n    ⍝ square real non-symmetric
                                      Bia←(-n÷2)+?i a⍴n
                                      :If c3   ⍝ symmetric
                                          :If c4        ⍝ not nec pos def
                          ⍝'square real symmetric'         ⍝ try DPOSV
                                              Aij+←⍉Aij                       ⍝ square real symmetric
                                          :Else
                          ⍝'square real positive definite'
                                              diag←i i⍴↑(i+1)↑¨0.1×i?100
                                              Aij←↑+.×/(⍉Aij)diag Aij
                                          :End
                                      :Else
                      ⍝'square real non-symmetric'            ⍝ DGESV
                                      :End
                                  :Else   ⍝ complex
                                      Aij←↓(-n÷2)+?i i 2⍴n   ⍝ square complex non-hermitian
                                      Bia←↓(-n÷2)+?i a 2⍴n
                                      :If c3    ⍝ hermitian
                                          :If c4    ⍝ not positive definite.
                          ⍝'square complex hermitian'         ⍝ ZPOSV
                                              Aij←herm Aij                       ⍝ square complex hermitian
                                          :Else
                          ⍝'square complex positive definite' ⍝ ZPOSV
                                              Aij←herm Aij                       ⍝ square complex hermitian
                                              diag←2↑¨i i⍴↑(i+1)↑¨0.1×i?100
                                              Aij←↑+.x/(⍉conj Aij)diag Aij
                                          :End
                                      :Else
                      ⍝'square complex non-hermitian'         ⍝ ZGESV
                                      :End
                                  :End
                              :Else     ⍝ non-square  ⍝ least squares fit Ax=b
                                  :If c2   ⍝ real
                  ⍝'non-square real general least squares'    ⍝ DGELS
                                      Aij←(-n÷2)+?i j⍴n
              ⍝need more coordination than just  Bia←(-n÷2)+?i a⍴n
                                      Xja←?j a⍴⍳100  ⍝ temp answer to get good fit
                                      Bia←Aij+.×Xja  ⍝ Ax=b has to be coherent
                                      ⎕EX'Xja'
                                  :Else   ⍝ complex
                  ⍝'non-square complex general least squares' ⍝ ZGELS
                                      Aij←↓(-n÷2)+?i j 2⍴n
                                      Xja←↓((-n÷2)+?j a 2⍴⍳n)÷n ⍝ temp
                                      Bia←Aij+.x Xja
                                      ⎕EX'Xja'
                                  :End
                              :End
                          :EndFor
                      :EndFor
                  :EndFor
              :EndFor
     ⍝------------------------------------------------------
              Xja←##.J2V(##.V2J Bia)##.Domino(##.V2J Aij)
     ⍝------------------------------------------------------
              :If real Xja
                  :Trap 11              ⍝ A might be 0
                      ok∧←∧/∊(##.V2J Xja)=fuzz(##.V2J Bia⌹Aij)  ⍝ compare with APL matrix divide
                  :End
              :Else  ⍝ complex
                  ok∧←∧/∊(##.V2J Bia)=fuzz(##.V2J Aij+.x Xja)
              :End
          :EndFor
        ∇


        ∇ ok←fourier;⎕PATH;⎕IO;x;fuzz;th;n;Xn;Yn;Zn;Kn;Ln;In;Inn;Enn;i;j  ⍝ Complex 1-D test of Fourier
        ⍝ runs 48 random matrices
          ok←1
          ⍝⎕PATH,←' ↑'
          ⎕IO←1
          x←{                         ⍝ complex multiply
              sign←2 2⍴1 ¯1 1 1
              +/sign×0 1⊖(2↑⍺)∘.×2↑⍵
          }
          fuzz←{⎕CT←2*¯32 ⋄ (⍺+○1)⍺⍺ ⍵+○1}
          :For i :In 8?200
              :For j :In 6?100
                  th←(⍳i)÷j ⍝ th←(⍳?200)÷?100
                  ⍝'' ⋄ 'n is ',⍕⍴th
                  Xn←2↑¨⊃+/,1 2∘.○(⊂th)×¨6?6 ⍝ try a step function like Xn←2↑¨(50⍴1),(50⍴2)
                  ⍝------------------------------
                  Kn←##.J2V ##.Fourier ##.V2J Xn
                  ⍝now test answer
                  n←⍴Xn
                  In←(⍳n)-⎕IO
                  ⍝Enn←*○0J¯2×In∘.×In÷n
                  Inn←○2×In∘.×In÷n
                  Enn←↓(2○Inn),[⎕IO+1.5](-1○Inn)
                  Ln←(÷0.5*⍨n)×Xn+.x Enn
                  ⍝'Does APL model give same answers?'
                  ok∧←∧/∊Ln=fuzz Kn ⍝ 'APL≡Fourier?'
                  ⍝------------------------------
                  Yn←##.J2V ¯1 ##.Fourier ##.V2J Kn
                  ⍝now test answer
                  ⍝'Does Inverse Fourier return original argument?'
                  ok∧←∧/∊Xn=fuzz Yn
                  ⍝Enn←*○0J2×In∘.×In÷n
                  Enn←↓(2○Inn),[⎕IO+1.5](1○Inn)
                  Zn←(÷0.5*⍨n)×Kn+.x Enn
                  ⍝'Does APL model of inverse give same answers?'
                  ok∧←∧/∊Zn=fuzz Yn ⍝ 'APL≡Inverse Fourier?'
                  ⍝------------------------------
              :EndFor
          :EndFor
        ∇

        ∇ ok←eigen;⎕CT;⎕PP;i;Aii;Eji;real;fuzz;⎕PATH;x;norm;conj;anti;symm;herm;diag;j;k;l
        ⍝ 48 random matrices
          ok←1
          ⍝⎕PATH,←' ↑'
          x←{                    ⍝ complex multiply
              sign←2 2⍴1 ¯1 1 1
              +/sign×0 1⊖(2↑⍺)∘.×2↑⍵
          }
          real←{1=≡,⍵}           ⍝ matrix is real.
          conj←{⍵×⊂1 ¯1}         ⍝ complex conjugate.
          anti←{⍵-⍉⍵}            ⍝ antisymmetrify
          symm←{⍵+⍉⍵}            ⍝ symmetrify
          herm←{real cmpx←⊂[1 2]↑⍵ ⋄ (symm real),¨anti cmpx}  ⍝ hermitianise.
          :For i :In ⍳6
              :For j :In 0 1
                  :For k :In 0 1
                      :For l :In 0 1
                          :If j  ⍝ real
                              Aii←(-i÷2)+?i i⍴i               ⍝ real non-symmetric
                              :If k   ⍝ symmetric
                                  :If l        ⍝ not nec pos def
                                      ⍝ 'real symmetric'
                                      Aii+←⍉Aii                   ⍝ real symmetric
                                  :Else
                                      ⍝ 'real positive definite'
                                      diag←i i⍴↑(i+1)↑¨0.1×i?100
                                      Aii←↑+.×/(⍉Aii)diag Aii
                                  :End
                              :Else
                                  ⍝ 'real non-symmetric'
                              :End
                          :Else      ⍝ complex
                              Aii←↓(-i÷2)+?i i 2⍴i
                              :If k   ⍝ hermitian
                                  :If l    ⍝ not nec positive definite.
                                      ⍝ 'complex hermitian'
                                      Aii←herm Aii            ⍝ complex  hermitian
                                  :Else
                                      ⍝ 'complex positive definite'
                                      Aii←herm Aii
                                      diag←2↑¨i i⍴↑(i+1)↑¨0.1×i?100
                                      Aii←↑+.x/(⍉conj Aii)diag Aii     ⍝ complex positive definite
                                  :End
                              :Else
                                  ⍝ 'complex non-hermitian'
                              :End
                          :End
                          :Trap 11
⍝-------------------------------------------------------------
                              Eji←##.J2V ##.Eigen ##.V2J Aii       ⍝N.B. should choose?  Eii←Eii××+/×,Eii
⍝-------------------------------------------------------------
                          ⍝⌊0.5+⊃↓Eji ⍝ check pos def case
                          :Else
                              ok←0 ⋄ 'failed' ⋄ Aii ⋄ →0        ⍝try DSYEV→DGEEV/ZHEEV→ZGEEV
                          :End
                          norm←{                         ⍝ Euclidean norm
                              1=≡,⍵:(+⌿⍵*2)*0.5          ⍝ real
                              (+⌿,[1 2]2 3 1⍉↑⍵*2)*0.5   ⍝ complex
                          }
                          fuzz←{⎕CT←2*¯32 ⋄ (⍺+○1)⍺⍺ ⍵+○1}
         
                          :If real Eji
                              ⍝'norm'(norm 1 0↓Eji)
                              ok∧←∧/∊(Aii+.×1 0↓Eji)=fuzz(Eji[1;]×[2]1 0↓Eji)
                          :Else
                              :If real Aii
                                  Aii←2↑¨Aii
                              :End
                              ⍝'euclidean norm'(norm 1 0↓Eji)
                              ok∧←∧/∊(Aii+.x 1 0↓Eji)=fuzz(i i⍴Eji[1;])x¨1 0↓Eji
                          :End
                      :EndFor
                  :EndFor
              :EndFor
          :EndFor
        ∇
    :endnamespace

:EndNamespace
