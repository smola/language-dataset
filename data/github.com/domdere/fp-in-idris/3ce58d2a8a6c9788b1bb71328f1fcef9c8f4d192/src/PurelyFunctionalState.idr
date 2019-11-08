module PurelyFunctionalState

import Verified

||| The State Functor, abstracts all the combinators
||| We'll be using in this chapter
|||
data MyState s a =
    StateAction (s -> (a, s))

||| To fold MyState s a into (a, s) you need to give it an initial state
|||
runMyState : MyState s a -> s -> (a, s)
runMyState (StateAction action) = action

||| Like runMyState except that it drops the value and just returns the final state
|||
execMyState : MyState s a -> s -> s
execMyState ma st = snd (runMyState ma st)

-- Need to define the combinators, we're in a strict environment, so you have to start from the most
-- generic one and work your way down...

bindMyState : MyState s a -> (a -> MyState s b) -> MyState s b
bindMyState mx f = StateAction stateAction
    where
        stateAction : s -> (b, s)
        stateAction st = let (x, st1) = runMyState mx st in runMyState (f x) st1

pureMyState : a -> MyState s a
pureMyState x = StateAction (\st => (x, st))

mapMyState : (a -> b) -> MyState s a -> MyState s b
mapMyState f mx = bindMyState mx (pureMyState . f)

lift2MyState : (a -> b -> c) -> MyState s a -> MyState s b -> MyState s c
lift2MyState g mx my = bindMyState mx (\x => bindMyState my (\y => pureMyState (g x y)))

applyMyState : MyState s (a -> b) -> MyState s a -> MyState s b
applyMyState = lift2MyState id

-- instances

instance Functor (MyState s) where
    map = mapMyState

instance VerifiedFunctor (MyState s) where
    mapIdentity fa = ?mapIdentityMyState_rhs
    mapComposition fa k g = ?mapCompositionMyState_rhs

instance Applicative (MyState s) where
    pure = pureMyState
    (<*>) = applyMyState

||| a lot of these are hard to prove without a notion of extensional equality for functions,
||| since these proofs wont be used in programs,
||| I'm happy to cheat with believe_me.
|||
extensionalEquality : (f : a -> b) -> (g : a -> b) -> (x : a) -> (f x = g x )-> f = g
extensionalEquality f g x = believe_me

instance VerifiedApplicative (MyState s) where
    applicativePureId v = ?applicativePureIdMyState_rhs

    applicativeComposition (StateAction u) (StateAction v) (StateAction w) = ?applicativeCompositionMyState_rhs

    applicativeHomomorphism k x = ?applicativeHomomorphismMyState_rhs

    applicativeInterchange u y = ?applicativeInterchangeMyState_rhs

instance Monad (MyState s) where
    (>>=) = bindMyState

instance VerifiedMonad (MyState s) where
    monadPureIdentityL k x = ?monadPureIdentityLMyState_rhs

    monadPureIdentityR ma = ?monadPureIdentityLMyState_rhs

    monadBindAssociative k h ma = ?monadBindAssociativeMyState_rhs

    monadBindApplySame f mx my = ?monadBindApplySameMyState_rhs

-- exercises

-- I skipped the RNG related exercises as Idris' RNG and Int manipulation functions are sort of immature.

||| Exercise 6 : Implement map2 for MyState
|||
total
map2 : (a -> b -> c) -> MyState s a -> MyState s b -> MyState s c
map2 = lift2MyState

||| Exercise 6 : Implement map2 for MyState
|||
||| see map2
|||
total
exercise6 : (a -> b -> c) -> MyState s a -> MyState s b -> MyState s c
exercise6 = map2

total
traverseMyStateList : (a -> MyState s b) -> List a -> MyState s (List b)
traverseMyStateList f xs = foldr (foldFunc f) (pure (the (List b) Nil)) xs
    where
        foldFunc : (a -> MyState s b) -> a -> MyState s (List b) -> MyState s (List b)
        foldFunc g x mys = lift2MyState (::) (g x) mys
||| Exercise 7 : Implement SequenceList for a list of MyState
|||
total
sequenceMyStateList : List (MyState s a) -> MyState s (List a)
sequenceMyStateList = traverseMyStateList id

||| Exercise 7 : Implement SequenceList for a list of MyState.
|||
||| see sequenceMyStateList
|||
total
exercise7 : List (MyState s a) -> MyState s (List a)
exercise7 = sequenceMyStateList

||| Exercise 8 : Implement bindMyState
|||
||| see bindMyState
|||
total
exercise8 : MyState s a -> (a -> MyState s b) -> MyState s b
exercise8 = bindMyState

||| Exercise 9 : Reimplement map and map2 in terms of bindMyState,
||| (I already wrote map2 in terms of bindMyState).
|||
|||
total
mapMyState2 : (a -> b) -> MyState s a -> MyState s b
mapMyState2 f ma = bindMyState ma (pureMyState . f)

||| Exercise 9 : Reimplement map and map2 in terms of bindMyState,
||| (I already wrote map2 in terms of bindMyState).
|||
||| so see mapMyState2.
|||
total
exercise9 : (a -> b) -> MyState s a -> MyState s b
exercise9 = mapMyState2

-- Exercise 10 is all the Functor, Applicative and Monad functions for MyState.
-- which is already done, see the Functor, Applicative and Monad instances for
-- MyState,
--
-- Also see sequenceMyStateList

||| State type for Exercise 11
data CoinMachineState = CoinMachineS Bool Int Int

total
isLocked : CoinMachineState -> Bool
isLocked (CoinMachineS b x y) = b

total
candies : CoinMachineState -> Int
candies (CoinMachineS b x y) = x

total
coins : CoinMachineState -> Int
coins (CoinMachineS b x y) = y

data MachineAction = InsertCoin | TurnKnob

||| Common function for MyState, gets the value of the State
|||
total
getMyState : MyState s s
getMyState = StateAction (\x => (x, x))

||| applies a function to the state and returns the result
|||
total
getsMyState : (s -> a) -> MyState s a
getsMyState f = mapMyState f getMyState

||| This function allows you to write to the state
|||
total
putMyState : s -> MyState s ()
putMyState newState = StateAction (\oldState => ((), newState))

||| Given a conditional, will run the applicative action unless the conditional is False
|||
||| @ b The Conditional
||| @ action the action to run when the conditional is False
|||
total
unlessA : (Applicative f) => (b : Bool) -> (action : f ()) -> f ()
unlessA b action = if b then pure () else action

total
isOutOfCandy : MyState CoinMachineState Bool
isOutOfCandy = do
    candyCount <- getsMyState candies
    pure (candyCount == 0)

total
ifCandy : MyState CoinMachineState () -> MyState CoinMachineState ()
ifCandy action = do
    isOut <- isOutOfCandy
    unlessA isOut action

total
ifUnlocked : MyState CoinMachineState () -> MyState CoinMachineState ()
ifUnlocked action = do
    locked <- getsMyState isLocked
    unlessA locked action

total
ifLocked : MyState CoinMachineState () -> MyState CoinMachineState ()
ifLocked action = do
    locked <- getsMyState isLocked
    when locked action


total
runAction : MachineAction -> MyState CoinMachineState ()
runAction InsertCoin  = ifCandy (
    do
        coins' <- getsMyState coins
        candies' <- getsMyState candies
        putMyState (CoinMachineS False candies' (coins' + 1))
    )
runAction TurnKnob    = ifCandy (ifUnlocked (
    do
        coins' <- getsMyState coins
        candies' <- getsMyState candies
        putMyState (CoinMachineS True (candies' - 1) coins')
    ))

initialState : Int -> Int -> CoinMachineState
initialState = CoinMachineS True

||| Exercise 11 - simulateMachine
|||
total
simulationMachine : (candy : Int) -> (coin : Int) -> (actions : List MachineAction) -> CoinMachineState
simulationMachine candy coin = flip execMyState (initialState candy coin) . traverseMyStateList runAction
