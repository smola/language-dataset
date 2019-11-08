{-# OPTIONS --type-in-type --no-universe-polymorphism #-}
module Reactive where

open import Prelude

infixr 9 _∘ᵗ_ _∘ᵀ_ _∘ᵇ_ _∙_
infixl 9 _∘ʷ_
infix 4 _,ᵀ_ _,ᵗ_
infix 2 _×ᵀ_

-- Coinductive trees will represent protocols of interactions
record Tree : Set where
  coinductive
  field
    Branch : Set
    child : Branch → Tree

open Tree

constᵀ : Set → Tree
constᵀ A .Branch = A
constᵀ A .child _ = constᵀ A

_×ᵀ_ : Tree → Tree → Tree
(p ×ᵀ q) .Branch = p .Branch × q .Branch
(p ×ᵀ q) .child (ph , qh) = p .child ph ×ᵀ q .child qh

alter : Tree → Tree → Tree
alter p q .Branch = p .Branch
alter p q .child hp = alter q (p .child hp)

alter' = λ A B → alter (constᵀ A) (constᵀ B)

-- signals
Sig = λ A B → alter' (Maybe A) (Maybe B)

NoSig  = Sig ⊥ ⊥
OutSig = λ A → Sig A ⊥
InSig  = λ A → Sig ⊥ A
BiSig  = λ A → Sig A A

{-
p:  P₁    P₂ -> P₃    P₄  ...
    |     ^     |     ^
    v     |     v     |
q:  Q₁ -> Q₂    Q₃ -> Q₄  ...
-}
merge : Tree → Tree → Tree
merge p q .Branch = p .Branch
merge p q .child hp .Branch = q .Branch
merge p q .child hp .child hq = merge (q .child hq) (p .child hp)

union2 : Tree → Tree → Tree
union2 p q .Branch = p .Branch ⊎ q .Branch
union2 p q .child (inj₁ ph) with p .child ph
... | r = λ where .Branch → q .Branch
                  .child rh → union2 r (q .child rh)
union2 p q .child (inj₂ qh) with q .child qh
... | r = λ where .Branch → p .Branch
                  .child rh → union2 (p .child rh) r

-- TODO: express with simpler combinators?
union2mb : Tree → Tree → Tree
union2mb p q .Branch = Maybe (p .Branch ⊎ q .Branch)
union2mb p q .child (just (inj₁ ph)) with p .child ph
... | r = λ where .Branch → q .Branch
                  .child rh → union2mb r (q .child rh)
union2mb p q .child (just (inj₂ qh)) with q .child qh
... | r = λ where .Branch → p .Branch
                  .child rh → union2mb (p .child rh) r
union2mb p q .child nothing .Branch = ⊤
union2mb p q .child nothing .child _ = union2mb p q

-----------------------------

data I/O : Set where I O : I/O   -- input and output phases

variable p q r s : Tree
variable i/o : I/O

opposite : I/O → I/O
opposite I = O
opposite O = I

ΠΣ : I/O → (A : Set) → (A → Set) → Set
ΠΣ I A P = (a : A) → P a
ΠΣ O A P = Σ A P

⟨_⟩ : I/O → (A → A → B) → A → A → B
⟨ I ⟩ = id
⟨ O ⟩ = flip

-- an interactive agent which communcates according to the protocol p
-- an agent can be called either in input or in output phase
record Agent (i/o : I/O) (p : Tree) : Set where
  coinductive
  field
    step : ΠΣ i/o (p .Branch) λ a → Agent (opposite i/o) (p .child a)

open Agent

_,ᵀ_ : Agent i/o p → Agent i/o q → Agent i/o (p ×ᵀ q)
_,ᵀ_ {i/o = I} a b .step (k , l) = a .step k ,ᵀ b .step l
_,ᵀ_ {i/o = O} a b .step with a .step | b .step
... | i , j | k , l = (i , k) , (j ,ᵀ l)

_∘ᵗ_ : Agent I (alter p q) → Agent I (alter q r) → Agent I (alter p r)
(b ∘ᵗ a) .step ph .step with b .step ph .step
... | qh , bt with a .step qh .step
... | rh , at = rh , bt ∘ᵗ at

-- stream processors
SP = λ A B → Agent I (alter' A B)

-- same as  Agent I (Sig A B)
SPM = λ A B → SP (Maybe A) (Maybe B)

arr : (A → B) → SP A B
arr f .step x .step = f x , arr f

accum : (A → B → B) → B → SP A B
accum f b .step a .step with f a b
... | b' = b' , accum f b'

maybefy : SP A B → SPM A B
maybefy f .step nothing .step = nothing , (maybefy f)
maybefy f .step (just a) .step with f .step a .step
... | x , y = just x , maybefy y

-- synchronous interaction transformers
IT = λ p q → Agent I (merge p q)

mapAgent : ⟨ i/o ⟩ IT q p → Agent i/o p → Agent i/o q
mapAgent {i/o = I} l i .step hq with l .step hq .step
... | a , l2 = mapAgent l2 (i .step a)
mapAgent {i/o = O} l i .step with i .step
... | a , b with l .step a .step
... | c , l2 = c , (mapAgent l2 b)

_∘ᵀ_ : IT p q → IT q r → IT p r
(b ∘ᵀ a) .step ph .step with b .step ph .step
... | qh , bt with a .step qh .step
... | rh , at = rh , at ∘ᵀ bt

_,ᵗ_ : IT p q → IT r s → IT (p ×ᵀ r) (q ×ᵀ s)
_,ᵗ_ a b .step (k , l) .step with a .step k .step | b .step l .step
... | c , d | e , f = (c , e) , (d ,ᵗ f)

idIT : IT p p
idIT .step a .step = a , idIT

swapIT : IT (p ×ᵀ q) (q ×ᵀ p)
swapIT .step (a , b) .step = (b , a) , swapIT

assocIT : ⟨ i/o ⟩ IT ((p ×ᵀ q) ×ᵀ r) (p ×ᵀ (q ×ᵀ r))
assocIT {i/o = I} .step ((a , b) , c) . step = (a , (b , c)) , assocIT {i/o = O}
assocIT {i/o = O} .step (a , (b , c)) . step = ((a , b) , c) , assocIT {i/o = I}

fstSig : IT (p ×ᵀ Sig A B) p
fstSig .step (x , y) .step = x , λ where .step x .step → (x , nothing) , fstSig

joinSig : A → B → ⟨ i/o ⟩ IT (⟨ i/o ⟩ Sig A C ×ᵀ ⟨ i/o ⟩ Sig B D) (⟨ i/o ⟩ Sig (A × B) (C × D))
joinSig {i/o = I} a b .step (nothing , nothing) .step = nothing      , joinSig {i/o = O} a b
joinSig {i/o = I} a b .step (nothing , just y ) .step = just (a , y) , joinSig {i/o = O} a y
joinSig {i/o = I} a b .step (just x  , nothing) .step = just (x , b) , joinSig {i/o = O} x b
joinSig {i/o = I} a b .step (just x  , just y ) .step = just (x , y) , joinSig {i/o = O} x y
joinSig {i/o = O} a b .step nothing        .step = (nothing , nothing) , joinSig {i/o = I} a b
joinSig {i/o = O} a b .step (just (x , y)) .step = (just x  , just y ) , joinSig {i/o = I} a b

noInput : IT (Sig A B) (OutSig A)
noInput .step x .step = x , λ where .step y .step → nothing , noInput

noOutput : IT (Sig A B) (InSig B)
noOutput .step x .step = nothing , λ where .step y .step → y , noOutput

mkIT : Agent i/o p → ⟨ i/o ⟩ IT p NoSig
mkIT {i/o = I} a .step x .step = nothing , mkIT {i/o = O} (a .step x)
mkIT {i/o = O} a .step _ .step with a .step
... | b , c = b , mkIT {i/o = I} c

constIT = λ S T A B → IT (alter' S T) (alter' A B)

constITM = λ S T A B → IT (Sig S T) (Sig A B)

lensIT : Lens S T A B → constIT S T A B
lensIT k .step s .step with k s
... | a , bt = a , λ where .step b .step → bt b , lensIT k

prismIT : Prism S T A B → constITM S T A B
prismIT f = lensIT (prismToLens f)

mkIT' : SP S A → SP B T → constIT S T A B
mkIT' f g .step s .step with f .step s .step
... | a , cont = a , (mkIT' g cont)

isoIT : (S → A) → (B → T) → constIT S T A B
isoIT f g = lensIT λ x → (f x) , g

-- bidirectional connection
Bi = λ p q → Agent I (union2 p q)

_∘ᵇ_ : Bi p q → Bi q r → Bi p r
(a ∘ᵇ b) .step (inj₁ x) .step with a .step (inj₁ x) .step
... | c , d with b .step (inj₁ c) .step
... | e , f = e , d ∘ᵇ f
(a ∘ᵇ b) .step (inj₂ x) .step with b .step (inj₂ x) .step
... | c , d with a .step (inj₂ c) .step
... | e , f = e , f ∘ᵇ d

isoBi : Iso A A B B → Bi (constᵀ A) (constᵀ B)
isoBi i .step (inj₁ x) .step = (i .proj₁ x) , isoBi i
isoBi i .step (inj₂ x) .step = (i .proj₂ x) , isoBi i

mmb : Bi p q → Agent I (union2mb p q)
mmb x .step nothing .step = _ , mmb x
mmb x .step (just (inj₁ y)) .step with x .step (inj₁ y) .step
... | a , b = a , mmb b
mmb x .step (just (inj₂ y)) .step with x .step (inj₂ y) .step
... | a , b = a , mmb b

entangleBi : IT (Sig A A ×ᵀ Sig B B) (union2mb (constᵀ A) (constᵀ B))
entangleBi .step (nothing , nothing) .step = nothing , λ where .step _ .step → (nothing , nothing) , entangleBi
entangleBi .step (just x , nothing) .step = just (inj₁ x) , λ where .step z .step → (nothing , just z) , entangleBi
entangleBi .step (nothing , just x) .step = just (inj₂ x) , λ where .step z .step → (just z , nothing) , entangleBi
entangleBi .step (just x , just y) .step = nothing , λ where .step _ .step → (nothing , nothing) , entangleBi

entangle : IT (Sig A C ×ᵀ Sig C B) (Sig A B)
entangle .step (a , c) .step = a , λ where .step b .step → (c , b) , entangle

enta : SP A B → IT (Sig A C ×ᵀ Sig C B) NoSig
enta x = entangle ∘ᵀ mkIT (maybefy x)

entaBi : Bi (constᵀ A) (constᵀ B) → IT (Sig A A ×ᵀ Sig B B) NoSig
entaBi x = entangleBi ∘ᵀ mkIT (mmb x)

---------------------------------------------------------------------

data Direction : Set where horizontal vertical : Direction
data Abled     : Set where enabled disabled : Abled
data Validity  : Set where valid invalid : Validity

variable fin      : Fin _
variable vec      : Vec _ _
variable checked  : Bool
variable size     : ℕ
variable name str : String
variable en       : Abled
variable dir      : Direction
variable val      : Validity

oppositeᵉ : Abled → Abled
oppositeᵉ enabled = disabled
oppositeᵉ disabled = enabled

oppositeᵛ : Validity → Validity
oppositeᵛ valid = invalid
oppositeᵛ invalid = valid

isEnabled : I/O → Abled → Set
isEnabled I enabled  = ⊤
isEnabled I disabled = ⊥
isEnabled O _ = ⊤

-- abstract widget
data Widget : Set where
  Button    : Abled → String → Widget
  CheckBox  : Abled → Bool → Widget
  ComboBox  : Abled → Vec String n → Fin n → Widget
  Entry     : Abled → (size : ℕ)(name contents : String) → Validity → Widget
  Label     : String → Widget
  Empty     : Widget
  Container : Direction → Widget → Widget → Widget

variable w w₁ w₂ : Widget

isInput : Widget → Maybe (Abled × Widget)
isInput (Button e x)       = just (e , Button (oppositeᵉ e) x)
isInput (CheckBox e x)     = just (e , CheckBox (oppositeᵉ e) x)
isInput (ComboBox e ss i)  = just (e , ComboBox (oppositeᵉ e) ss i)
isInput (Entry e n s s' v) = just (e , Entry (oppositeᵉ e) n s s' v)
isInput _ = nothing

isJust : Maybe A → Set
isJust = maybe ⊥ (λ _ → ⊤)

-- possible edits of a widget (I: by the user; O: by the program)
data WidgetEdit : I/O → Widget → Set
-- ⟪_⟫ performs the edit
⟪_⟫ : {d : I/O} {a : Widget} (p : WidgetEdit d a) → Widget

data WidgetEdit  where
  toggle         : {_ : isEnabled i/o en} → WidgetEdit i/o (CheckBox en checked)
  click          : WidgetEdit I (Button enabled str)
  setLabel       : String → WidgetEdit O (Label str)
  setEntry       : {_ : isEnabled i/o en} → String → WidgetEdit i/o (Entry en size name str val)
  select         : {vec : Vec String n}{_ : isEnabled i/o en} → Fin n → WidgetEdit i/o (ComboBox en vec fin)
  toggleEnable   : {_ : isJust (isInput w)} → WidgetEdit O w
  toggleValidity : WidgetEdit O (Entry en size name str val)
  replaceBy      : Widget → WidgetEdit O w
  modLeft        : WidgetEdit i/o w₁ → WidgetEdit i/o (Container dir w₁ w₂)
  modRight       : WidgetEdit i/o w₂ → WidgetEdit i/o (Container dir w₁ w₂)
  addToLeft addToRight    : Direction → Widget → WidgetEdit O w
  removeLeft removeRight  : WidgetEdit O (Container dir w₁ w₂)
  _∙_            : (p : WidgetEdit O w) → WidgetEdit O ⟪ p ⟫ → WidgetEdit O w

⟪ replaceBy x ⟫ = x
⟪ modLeft  {dir = dir} {w₂ = w₂} p ⟫ = Container dir ⟪ p ⟫ w₂
⟪ modRight {dir = dir} {w₁ = w₁} p ⟫ = Container dir w₁ ⟪ p ⟫
⟪ toggle {checked = b} {en = en} ⟫   = CheckBox en (not b)
⟪ select {en = en} {vec = vec} fin ⟫ = ComboBox en vec fin
⟪ setEntry {_} {size} {name} {_} {en} {val} {_} str ⟫ = Entry en size name str val
⟪ toggleValidity {size} {name} {str} {en} {val} ⟫     = Entry en size name str (oppositeᵛ val)
⟪ click {s} ⟫  = Button enabled s
⟪ setLabel l ⟫ = Label l
⟪ toggleEnable {w} {e} ⟫ with isInput w | e
... | just (_ , w') | tt = w'     -- note: the JS backend cannot erase 'e', maybe because of the pattern maching on tt here
... | nothing | ()
⟪ addToLeft  {r} dir w ⟫ = Container dir w r
⟪ addToRight {l} dir w ⟫ = Container dir l w
⟪ removeLeft  {w₂ = w₂} ⟫ = w₂
⟪ removeRight {w₁ = w₁} ⟫ = w₁
⟪ p ∙ q ⟫ = ⟪ q ⟫

---------------------------------------

pw : I/O → Widget → Tree
pw i/o w .Branch = Maybe (WidgetEdit i/o w)
pw i/o w .child e = pw (opposite i/o) (maybe w ⟪_⟫ e)

-- GUI component (reactive widget)
WComp' = λ i/o w p → ⟨ i/o ⟩ IT (pw i/o w) p

WComp = λ i/o w A B → WComp' i/o w (⟨ i/o ⟩ Sig A B)

WC = λ p → Σ Widget λ w → WComp' I w p

-- GUI program
GUI = WC NoSig

-- `processMain` is automatically applied on `mainWidget` by the run time system
processMain : WC (Sig A B) → Σ Widget λ w → Agent I (pw I w)
processMain (w , x) = (w , mapAgent {i/o = I} (x ∘ᵀ noInput ∘ᵀ noOutput) (arr id))

-- enforcing  no input ⇒ no output
ease : WComp i/o w A B → WComp i/o w A B
ease {i/o = I} b .step nothing .step = nothing , λ where .step x .step → nothing , ease {i/o = I} b
ease {i/o = I} b .step (just z) .step with b .step (just z) .step
... | x , y = x , ease {i/o = O} y
ease {i/o = O} b .step x .step with b .step x .step
... | c , d = c , ease {i/o = I} d

ease' : WC (Sig A B) → WC (Sig A B)
ease' (_ , x) = _ , ease {i/o = I} x

_∘ʷ_ : WC p → IT p q → WC q
(_ , x) ∘ʷ y = (_ , x ∘ᵀ y)


----------------------------------------------------------

button : ∀ i/o → WComp i/o (Button enabled str) ⊤ ⊥
button I .step nothing      .step = nothing , button O
button I .step (just click) .step = just _  , button O
button O .step _ .step = nothing , button I

button' : String → WC (OutSig ⊤)
button' s = _ , button {s} I

checkbox : ∀ i/o → WComp i/o (CheckBox enabled checked) Bool Bool
checkbox I .step nothing  .step = nothing , checkbox O
checkbox {b} I .step (just toggle) .step = just (not b) , checkbox O
checkbox O .step nothing .step = nothing , checkbox I
checkbox {b} O .step (just b') .step with b == b'
... | true  = nothing     , checkbox I
... | false = just toggle , checkbox I

checkbox' : Bool → WC (BiSig Bool)
checkbox' b = _ , checkbox {b} I

comboBox : ∀ {vec : Vec String n} i/o → WComp i/o (ComboBox enabled vec fin) (Fin n) (Fin n)
comboBox I .step nothing  .step = nothing , comboBox O
comboBox I .step (just (select i)) .step = just i , comboBox O
comboBox O .step nothing .step = nothing , comboBox I
comboBox O .step (just i) .step = just (select i) , comboBox I

comboBox' : (vec : Vec String n)(fin : Fin n) → WC (Sig (Fin n) (Fin n))
comboBox' vec fin = _ , comboBox {fin = fin}{vec = vec} I

label : ∀ i/o → WComp i/o (Label str) ⊥ String
label I .step nothing  .step = nothing , label O
label I .step (just ())
label O .step nothing .step = nothing , label I
label O .step (just m) .step = just (setLabel m) , label I

label' : String → WC (InSig String)
label' n = _ , label {n} I

label'' = λ n → label' n -- ∘ʷ discard

-- todo: refactoring (use less cases)
entry : Bool → ∀ i/o → WComp' i/o (Entry en size name str val) (⟨ i/o ⟩ Sig String (Maybe String) ×ᵀ ⟨ i/o ⟩ Sig ⊥ ⊤)
entry _ I .step nothing .step = (nothing , nothing) , entry false O
entry _ I .step (just (setEntry s)) .step = (just s , nothing) , entry true O
entry {val = v} v' O .step (ii , td) .step with td | ii | v | v'
... | nothing | nothing        | invalid | true = just toggleValidity , entry false I
... | nothing | just nothing   | valid   | _    = just toggleValidity , entry false I
... | nothing | just (just ss) | valid   | _    = just (setEntry ss) , entry false I
... | nothing | just (just ss) | invalid | _    = just (setEntry ss ∙ toggleValidity) , entry false I
... | nothing | _              | _       | _    = nothing , entry false I
... | just _  | nothing        | invalid | true = just (toggleEnable ∙ toggleValidity) , entry false I
... | just _  | just nothing   | valid   | _    = just (toggleEnable ∙ toggleValidity) , entry false I
... | just _  | just (just ss) | valid   | _    = just (toggleEnable ∙ setEntry ss) , entry false I
... | just _  | just (just ss) | invalid | _    = just (toggleEnable ∙ setEntry ss ∙ toggleValidity) , entry false I
... | just _  | _              | _       | _    = just toggleEnable , entry false I

entryF : ℕ → Abled → String → WC (Sig String (Maybe String) ×ᵀ InSig ⊤)
entryF size en name = _ , entry {size} {name} {""} {en} {valid} false I

container' : ∀ i/o → WComp' i/o (Container dir w₁ w₂) (pw i/o w₁ ×ᵀ pw i/o w₂)
container' I .step nothing .step = (nothing , nothing) , container' O
container' I .step (just (modLeft e))  .step = (just e , nothing) , container' O
container' I .step (just (modRight e)) .step = (nothing , just e) , container' O
container' O .step (nothing , nothing) .step = nothing , container' I
container' O .step (just x , nothing) .step = just (modLeft x) , container' I
container' O .step (nothing , just y) .step = just (modRight y) , container' I
container' O .step (just x , just y) .step = just (modLeft x ∙ modRight y) , container' I

container : (dir : Direction) → WComp' I w₁ r → WComp' I w₂ s → WComp' I  (Container dir w₁ w₂) (r ×ᵀ s)
container _ p q = container' I ∘ᵀ (p ,ᵗ q)

fc : Direction → WC p → WC (Sig C D) → WC p
fc dir (_ , b) (_ , d) = _ , container' {dir} I ∘ᵀ ({-ease {I}-} b ,ᵗ ease {i/o = I} d) ∘ᵀ fstSig

infixr 3 _<->_
infixr 4 _<|>_

_<|>_ : WC p → WC (Sig C D) → WC p
_<|>_ = fc horizontal
_<->_ : WC p → WC (Sig C D) → WC p
_<->_ = fc vertical

infix 5 _⟦_⟧_
_⟦_⟧_ : WC p → IT (p ×ᵀ q) r → WC q → WC r
(_ , b) ⟦ t ⟧ (_ , d) = _ , container' {horizontal} I ∘ᵀ (b ,ᵗ d) ∘ᵀ t

entry'' : Abled → ℕ → String → WC (Sig String (Maybe String) ×ᵀ InSig ⊤)
entry'' en len name = entryF len en name ⟦ fstSig ⟧ label' name

entry' = λ len name → entry'' enabled len name ∘ʷ fstSig

addLabel : String → WC p → WC p
addLabel s x = x ⟦ fstSig ⟧ label'' s

---------------------------------------------------------------------------------------------------------------

counter = λ n → label' (showNat n) ⟦ swapIT ∘ᵀ enta (arr (const 1) ∘ᵗ accum _+_ n ∘ᵗ arr showNat) ⟧ button' "Count"

counter' = λ f b n → checkbox' b ∘ʷ noInput ⟦ enta (arr f ∘ᵗ accum _+_ n ∘ᵗ arr showNat) ⟧ label' (showNat n)

floatEntry = λ s → entry' 4 s ∘ʷ prismIT floatPrism
dateEntry = λ en s → entry'' en 4 s ∘ʷ (prismIT floatPrism ,ᵗ idIT)

showDates : (Float × Float) → String
showDates (a , b) = primStringAppend (primShowFloat a) (primStringAppend " -- " (primShowFloat b))

main = processMain (
       label' "count clicks:"
  <|>  counter 0

  <->  label' "count checks:"
  <|>  counter' (if_then 1 else 0) false 0

  <->  label' "count checks and unchecks:"
  <|>  counter' (const 1)          false 0

  <->  label' "count unchecks:"
  <|>  counter' (if_then 0 else 1) false 0

  <->  label' "copy input to label:"
  <|>  entry' 10 "" ∘ʷ noInput ⟦ enta (arr id) ⟧ label' ""

  <->  label' "converter:"
  <|>  floatEntry "Celsius" ⟦ entaBi (isoBi (mulIso 1.8) ∘ᵇ isoBi (plusIso 32.0)) ⟧ floatEntry "Fahrenheit"

  <->  label' "flight booker (unfinished):"
  <|>  ((comboBox' ("one-way" ∷ "return" ∷ []) zero
            ⟦ (mkIT' (arr ( maybe nothing (const (just _))  -- mapMaybe (const _) cannot be used because Agda bug #3380
                          )) (arr id) ,ᵗ (assocIT {i/o = O} ∘ᵀ swapIT))
            ∘ᵀ assocIT {i/o = O}
            ∘ᵀ (swapIT ∘ᵀ entangle ,ᵗ joinSig {i/o = I} 0.0 0.0) ∘ᵀ (swapIT ∘ᵀ fstSig)
            ⟧
       (dateEntry enabled "start date" ∘ʷ fstSig ⟦ idIT ⟧ dateEntry disabled "return date")) ∘ʷ noInput)
            ⟦  {-(isoIT (showDates) (const nothing)  ,ᵗ idIT) ∘ᵀ entangle-} enta (arr showDates)
            ⟧
       (label' "")
 )
