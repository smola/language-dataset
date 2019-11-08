{-# OPTIONS --cubical --safe #-}

module DTLC.Base where

open import Cubical.Foundations.Prelude hiding (Σ; _,_)
open import Cubical.Foundations.Function


--------------------------------------------------------------------------
-- Adapted from: Type Theory in Type Theory using Quotient Inductive Types
--               by Thorsten Altenkirch and Ambrus Kaposi
-- See: http://www.cs.nott.ac.uk/~psztxa/publ/tt-in-tt.pdf
--
-- Note: We have changed a number of names relative to this paper, in the
--  interest of clarity. In particular, Tms is now _~>_, with the
--  arguments reversed. Thus, _∘_ (which we call _∘*_) also is reversed
--  with respect to [AK16]. Furthermore, we renamed π₁ to tail*, π₂
--  to head*, and app to unlam to make their functions more clear.
--------------------------------------------------------------------------


infixl 6 _,_
infixl 6 _,*_
infixr 5 _∘*_

PathPSyntax = PathP
syntax PathPSyntax A x y = x ≡[ A ]≡ y
infix 2 PathPSyntax



-----------------
-- Declarations
-----------------

-- We will mutually inductively define:

-- The type of contexts
data Ctx : Type₀

-- The type of types
data Typ : Ctx -> Type₀

-- The type of judgements x : τ ⊣ Γ ("x has type τ in context Γ")
Term : (Γ : Ctx) -> Typ Γ -> Type₀
syntax Term Γ τ = τ ⊣ Γ

-- The type of transformations / generalized subsitutions between contexts
_~>_ : Ctx -> Ctx -> Type₀


-- ...in fact, we define terms and transformations together:

data TmQV : Type₀ where
  TmV : TmQV
  TrV : TmQV

TmQArg : TmQV -> Ctx -> Type₀
TmQArg TmV Γ = Typ Γ
TmQArg TrV _ = Ctx

data TmQ : (a : TmQV) -> (Γ : Ctx) -> TmQArg a Γ -> Type₀

Term = TmQ TmV
_~>_ = TmQ TrV


-- Some forward declarations:
id* : ∀ {Γ} -> Γ ~> Γ
_∘*_ : ∀ {Γ Δ Σ} -> (Δ ~> Σ) -> (Γ ~> Δ) -> Γ ~> Σ
_[_]T : ∀ {Γ Δ} (τ : Typ Γ) (f : Γ ~> Δ) -> Typ Δ
_[_] : ∀ {Γ Δ τ} (x : τ ⊣ Γ) (f : Γ ~> Δ) -> (τ [ f ]T) ⊣ Δ



----------------
-- Definitions
----------------

data Ctx where
  ∙ : Ctx
  _,_ : (Γ : Ctx) -> Typ Γ -> Ctx


-- Unlike Altenkirch-Kaposi, we have a universe of types U:
data Typ where
  U : ∀ {Γ} -> Typ Γ
  type : ∀ {Γ} (τ : U ⊣ Γ) -> Typ Γ


-- Subsitution on types is defined in terms of substition on terms!
U [ f ]T = U
(type τ) [ f ]T = type (τ [ f ])

-- Substitution on types is a functor -- definitions are below
[id]T : ∀ {Γ} (τ : Typ Γ) -> τ [ id* ]T ≡ τ
[][]T : ∀ {Γ Δ Σ} (g : Δ ~> Σ) (f : Γ ~> Δ) (τ : Typ Γ)
        -> τ [ f ]T [ g ]T ≡ τ [ g ∘* f ]T


-- Having these is quite useful for readibility:

coeTm : ∀ {Γ} {x y : Typ Γ} -> x ≡ y -> Term Γ x -> Term Γ y
coeTm p = transport (cong (Term _) p)

coeTmTy : ∀ {Γ} {τ σ : Term Γ U} -> τ ≡ σ -> Term Γ (type τ) -> Term Γ (type σ)
coeTmTy p = transport (cong (Term _ ∘ type) p)


-- We define terms and transformations together
data TmQ where

  -- Ctx and ~> form a category
  -- Note: these are the definitions of id* and ∘* -- see below
  id*C : ∀ {Γ} -> Γ ~> Γ
  _∘*C_ : ∀ {Γ Δ Σ} -> (Δ ~> Σ) -> (Γ ~> Δ) -> Γ ~> Σ
  
  id*-l : ∀ {Γ Δ} (f : Γ ~> Δ) -> id* ∘* f ≡ f
  id*-r : ∀ {Γ Δ} (f : Γ ~> Δ) -> f ∘* id* ≡ f
  assoc : ∀ {Γ Δ Σ P} (h : Σ ~> P) (g : Δ ~> Σ) (f : Γ ~> Δ)
          -> (h ∘* g) ∘* f ≡ h ∘* (g ∘* f)

  -- ~> has a list structure on its first argument
  ∙* : ∀ {Δ} -> ∙ ~> Δ
  _,*_ : ∀ {Γ Δ τ} (f : Γ ~> Δ) (x : Term Δ (τ [ f ]T)) -> (Γ , τ) ~> Δ
  
  tail* : ∀ {Γ Δ τ} (f : (Γ , τ) ~> Δ) -> Γ ~> Δ
  head* : ∀ {Γ Δ τ} (f : (Γ , τ) ~> Δ) -> (τ [ tail* f ]T) ⊣ Δ

  ,*-∘* : ∀ {Γ Δ Σ τ} (g : Δ ~> Σ) (f : Γ ~> Δ) (x : Term Δ (τ [ f ]T))
          -> g ∘* (f ,* x) ≡ (g ∘* f) ,* coeTm ([][]T g f τ) (x [ g ])

  -- Computation rules for tail* and head*
  tail*-β : ∀ {Γ Δ τ} (f : Γ ~> Δ) (x : Term Δ (τ [ f ]T)) -> tail* (f ,* x) ≡ f
  head*-β : ∀ {Γ Δ τ} (f : Γ ~> Δ) (x : Term Δ (τ [ f ]T))
            -> head* (f ,* x) ≡[ (λ i → (τ [ tail*-β f x i ]T) ⊣ Δ) ]≡ x

  -- Uniqueness rules for ~>
  ∙*-η : ∀ {Δ} (f : ∙ ~> Δ) -> f ≡ ∙*
  ,*-η : ∀ {Γ Δ τ} (f : (Γ , τ) ~> Δ) -> (tail* f ,* head* f) ≡ f

  -- A useful combinator, defined in terms of the above
  _↑_ : ∀ {Γ Δ} (f : Γ ~> Δ) (τ : Typ Γ) -> (Γ , τ) ~> (Δ , τ [ f ]T)
  ↑-β : ∀ {Γ Δ} (f : Γ ~> Δ) (τ : Typ Γ) -> (Γ , τ) ~> (Δ , τ [ f ]T)
        -> f ↑ τ ≡ (tail* id* ∘* f) ,* coeTm ([][]T (tail* id*) f τ) (head* id*)


  -- The definition of substitution on terms _[_]
  _[_]C : ∀ {Γ Δ τ} (x : τ ⊣ Γ) (f : Γ ~> Δ) -> (τ [ f ]T) ⊣ Δ

  -- Substitution on terms is a functor
  [id] : ∀ {Γ τ} (x : τ ⊣ Γ) -> x [ id* ] ≡[ (λ i → ([id]T τ i) ⊣ Γ) ]≡ x
  [][] : ∀ {Γ Δ Σ τ} (g : Δ ~> Σ) (f : Γ ~> Δ) (x : τ ⊣ Γ)
         -> x [ f ] [ g ] ≡[ (λ i → ([][]T g f τ i) ⊣ Σ) ]≡ x [ g ∘* f ]

  -- Pi types and their computation rule for substition
  Π : ∀ {Γ} (τ : Typ Γ) (σ : Typ (Γ , τ)) -> U ⊣ Γ
  Π[] : ∀ {Γ Δ} (τ : Typ Γ) (σ : Typ (Γ , τ)) (f : Γ ~> Δ)
        -> (Π τ σ) [ f ] ≡ Π (τ [ f ]T) (σ [ f ↑ τ ]T)

  -- lambdas and their computation rule for substitition
  lam : ∀ {Γ τ σ} (y : σ ⊣ (Γ , τ)) -> type (Π τ σ) ⊣ Γ
  lam[] : ∀ {Γ Δ τ σ} (y : σ ⊣ (Γ , τ)) (f : Γ ~> Δ)
          -> lam y [ f ] ≡[ (λ i → type (Π[] τ σ f i) ⊣ Δ) ]≡ lam (y [ f ↑ τ ])

  -- Pi type elim, whose computation rule for substition is derivable using Π-η
  -- (Note: application is defined in terms of this below)
  unlam : ∀ {Γ τ σ} (ℓ : type (Π τ σ) ⊣ Γ) -> σ ⊣ (Γ , τ)

  -- Computation and uniqueness for Pi types
  Π-β : ∀ {Γ τ σ} (y : σ ⊣ (Γ , τ))      -> unlam (lam y) ≡ y
  Π-η : ∀ {Γ τ σ} (ℓ : type (Π τ σ) ⊣ Γ) -> lam (unlam ℓ) ≡ ℓ

-- The missing substitution computation rule for unlam, derivable because we have definitional η!
unlam[] : ∀ {Γ Δ τ σ} (ℓ : type (Π τ σ) ⊣ Γ) (f : Γ ~> Δ)
          -> unlam ℓ [ f ↑ τ ] ≡ unlam (coeTmTy (Π[] τ σ f) (ℓ [ f ]))
unlam[] {Γ} {Δ} {τ} {σ} ℓ f =
              (unlam ℓ) [ f ↑ τ ]                     ≡[ i ]⟨ Π-β ((unlam ℓ) [ f ↑ τ ]) (~ i) ⟩
  unlam (lam ((unlam ℓ) [ f ↑ τ ]))                   ≡[ i ]⟨ unlam (fromPathP (lam[] (unlam ℓ) f) (~ i)) ⟩
  unlam (coeTmTy (Π[] τ σ f) ((lam (unlam ℓ)) [ f ])) ≡[ i ]⟨ unlam (coeTmTy (Π[] τ σ f) ((Π-η ℓ i) [ f ])) ⟩
  unlam (coeTmTy (Π[] τ σ f) (ℓ [ f ]))               ∎

-- Missing definitions from above
[id]T U = refl
[id]T (type τ) = cong type ([id] τ)
[][]T g f U = refl
[][]T g f (type τ) = cong type ([][] g f τ)
id* = id*C
_∘*_ = _∘*C_
_[_] = _[_]C



--------------------
-- Derived Notions
--------------------

-- Weakening on contexts
wkn : ∀ {Γ τ} -> Γ ~> (Γ , τ)
wkn = tail* id*

-- A single substition at the top of a context
<_> : ∀ {Γ τ} (x : τ ⊣ Γ) -> (Γ , τ) ~> Γ
< x > = id* ,* coeTm (sym ([id]T _)) x

-- Using substition, we can now write the usual definition of application
_$_ : ∀ {Γ τ σ} (ℓ : (type (Π τ σ)) ⊣ Γ) (x : τ ⊣ Γ) -> (σ [ < x > ]T) ⊣ Γ
ℓ $ x = unlam ℓ [ < x > ]
infixl 4 _$_

-- Variables (as de Bruijn indices) are also derived!

var₀ : ∀ {Γ τ} -> (τ [ wkn ]T) ⊣ (Γ , τ)
var₀ = head* id*

var₊ : ∀ {Γ τ σ} -> τ ⊣ Γ -> (τ [ wkn ]T) ⊣ (Γ , σ)
var₊ x = x [ wkn ]

var₁ : ∀ {Γ τ σ} -> (τ [ wkn ]T [ wkn ]T) ⊣ ((Γ , τ) , σ)
var₁ = var₊ var₀

var₂ : ∀ {Γ τ σ ρ} -> (τ [ wkn ]T [ wkn ]T [ wkn ]T) ⊣ (((Γ , τ) , σ) , ρ)
var₂ = var₊ var₁

-- The other classical operations on contexts:

ctr : ∀ {Γ τ} -> (Γ , τ , τ [ wkn ]T) ~> (Γ , τ)
ctr {Γ} {τ} = id* ,* coeTm (sym p) var₀
  where p : τ [ wkn ]T [ id* ]T ≡ τ [ tail* id* ]T
        p = 
          τ [ wkn ]T [ id* ]T ≡⟨ [][]T id* wkn _ ⟩
          τ [ id* ∘* wkn ]T   ≡⟨ cong (τ [_]T) (id*-l wkn) ⟩
          τ [ tail* id* ]T ∎

-- exg : ∀ {Γ τ σ} -> (Γ , τ , σ [ wkn ]T) ~> (Γ , σ , τ [ wkn ]T)
-- exg {Γ} {τ} {σ} = wkn ∘* wkn ,*
--                   coeTm ([][]T wkn wkn τ) var₀ ,*
--                   coeTm (sym p) (coeTm ([][]T wkn wkn σ) var₁)
--   where p : σ [ wkn ]T [ (wkn ∘* wkn) ,* coeTm ([][]T wkn wkn τ) var₀ ]T ≡ σ [ wkn ∘* wkn ]T
--         p = begin
--           σ [ wkn ]T [ (wkn ∘* wkn) ,* coeTm ([][]T wkn wkn τ) var₀ ]T ≡⟨ [][]T _ _ σ ⟩
--           σ [ ((wkn ∘* wkn) ,* coeTm ([][]T wkn wkn τ) var₀) ∘* wkn ]T ≡⟨ cong (σ [_]T) {!!} ⟩
--           σ [ wkn ∘* wkn ]T ∎
--         -- p' : (wkn ∘* wkn) ,* coeTm ([][]T wkn wkn τ) var₁ ≡ wkn
--         -- p' = begin
--         --   (wkn ∘* wkn) ,* coeTm ([][]T wkn wkn τ) var₁ ≡⟨ sym (,*-∘* wkn wkn var₀) ⟩
--         --   wkn ∘* (wkn ,* var₀) ≡⟨ cong (wkn ∘*_) (,*-η id*) ⟩
--         --   wkn ∘* id*           ≡⟨ id*-r wkn ⟩
--         --   wkn ∎



--------------------------------------
-- Experimental: Encoding more types
--------------------------------------

_⇒_ : ∀ {Γ} (τ : U ⊣ Γ) (σ : U ⊣ (Γ , type τ)) -> U ⊣ Γ
x ⇒ y = Π (type x) (type y)
infixr 4 _⇒_

-- The natural number type

N : ∀ {Γ} -> U ⊣ Γ
N = Π U (type (var₀ ⇒ (var₁ ⇒ var₂) ⇒ var₂))

-- N[] : ∀ {Γ Δ} (f : Γ ~> Δ) -> type N [ f ]T ≡ type N
-- N[] f = cong type p
--   where p : Π U (type (var₀ ⇒ (var₁ ⇒ var₂) ⇒ var₂)) [ f ] ≡  Π U (type (var₀ ⇒ (var₁ ⇒ var₂) ⇒ var₂))
--         p = begin
--           Π U (type (var₀ ⇒ (var₁ ⇒ var₂) ⇒ var₂)) [ f ] ≡⟨ Π[] _ _ f ⟩
--           Π U (type ((var₀ ⇒ (var₁ ⇒ var₂) ⇒ var₂) [ f ↑ U ])) ≡⟨ {!!} ⟩
--           Π U (type (var₀ ⇒ (var₁ ⇒ var₂) ⇒ var₂)) ∎

z : ∀ {Γ} -> type N ⊣ Γ
z = lam (lam (lam var₁))

-- s : ∀ {Γ} (n : type N ⊣ Γ) -> type N ⊣ Γ
-- s {Γ} n = lam (lam (lam {!!})) -- var₀ $ (n' $ var₀ $ var₁ $ var₂)
--   where n' : type N ⊣ (((Γ , U) , type var₀) , type (var₁ ⇒ var₂))
--         n' = coe (cong (Term _) {!!}) (var₊ (var₊ (var₊ n)))

-- The dependent pair type

-- Σ : ∀ {Γ} (τ : Typ Γ) (σ : Typ (Γ , τ)) -> U ⊣ Γ
-- Σ τ σ = Π U (type ((Π (τ [ tail* id* ]T) (type (Π (σ [ tail* id* ↑ τ ]T) (type var₂)))) ⇒ var₀))

-- pair : ∀ {Γ τ σ} (x : τ ⊣ Γ) (y : σ ⊣ (Γ , τ)) -> (type (Σ τ σ)) ⊣ Γ
-- pair x y = lam (lam {!!})


  -- Σ : ∀ {Γ} (τ : Typ Γ) (σ : Typ (Γ , τ)) -> U ⊣ Γ
  -- Σ[] : ∀ {Γ Δ} (τ : Typ Γ) (σ : Typ (Γ , τ)) (f : Γ ~> Δ)
  --       -> (Σ τ σ) [ f ] ≡ Σ (τ [ f ]T) (σ [ f ↑ τ ]T)

  -- pair : ∀ {Γ τ σ} (x : τ ⊣ Γ) (y : σ ⊣ (Γ , τ)) -> (type (Σ τ σ)) ⊣ Γ
  -- pair[] : ∀ {Γ Δ τ σ} (x : τ ⊣ Γ) (y : σ ⊣ (Γ , τ)) (f : Γ ~> Δ)
  --          -> (pair x y) [ f ] ≡[ (λ i → type (Σ[] τ σ f i) ⊣ Δ) ]≡ pair (x [ f ]) (y [ f ↑ τ ])
  
  -- Σ-ind : ∀ {Γ τ σ} (ρ : Typ (Γ , type (Σ τ σ)))
  --                   (xy : (ρ [ {!!} ∘* ({!!} ↑ type (Σ τ σ)) ]T) ⊣ ((Γ , τ) , σ))
  --                   -> ρ ⊣ (Γ , type (Σ τ σ))
