implementation module Yapcol

import Yapcol
import StdMisc
import StdOverloaded
import StdFunc
import StdTuple
import StdList
import Data.Maybe
from Data.Func import $

// ---------- Instances ----------

instance Functor (Parser t) where
	fmap f p = pure f <*> p

instance Applicative (Parser t) where
	pure a = Parser \s -> (Right a, s)
	(<*>) mf ma = mf >>= \f -> ma >>= \a -> pure $ f a

instance Alternative (Parser t) where
	empty = fail "Failed alternative"
	(<|>) (Parser p1) (Parser p2) = Parser \s -> case p1 s of
		(Right x,s) = (Right x,s)
		_ = p2 s

instance Monad (Parser t) where
	bind (Parser ma) f = Parser \s -> case ma s of
		(Right a,s) = run (f a) s
		(Left m,s) = (Left m,s)

// ---------- Top Functions ----------

parse :: (Parser t a) [t] -> Either String a
parse p i = fst $ run p i

run :: (Parser t a) [t] -> (Either String a, [t])
run (Parser p) i = p i

// ---------- Combinators ----------

satisfy :: (t -> Bool) -> Parser t t
satisfy p = Parser \s -> case s of
	[] = (Left "Empty input",[])
	[t:ts]
		| p t = (Right t, ts)
		= (Left "Unable to parse.", [t:ts])

(<?>) infix 1 :: (Parser t a) String -> Parser t a
(<?>) p s = p <|> fail s 

fail :: String -> Parser t a
fail e = Parser \s -> (Left e,s)

many0 :: (Parser t a) -> Parser t [a]
many0 p = many1 p <|> pure []

many1 :: (Parser t a) -> Parser t [a]
many1 p = (\x xs -> [x : xs]) <$> p <*> many0 p

opt :: (Parser t a) -> Parser t ()
opt p = (p >>| pure ()) <|> pure ()

optMaybe :: (Parser t a) -> Parser t (Maybe a)
optMaybe p = pure <$> p

is :: t -> Parser t t | == t
is t = satisfy ((==) t)

choice :: [(Parser t a)] -> Parser t a
choice [] = fail "Unable to parse"
choice [p:ps] = p <|> choice ps

any :: Parser t t
any = satisfy (const True)

lookAhead :: (Parser t a) -> Parser t a
lookAhead (Parser p) = Parser \s -> case p s of
	(Right x, _) = (Right x,s)
	(Left m,_) = (Left m,s)

between :: (Parser t o) (Parser t c) (Parser t a) -> Parser t a
between pO pC p = pO >>| p >>= \a -> pC >>| pure a

skipMany0 :: (Parser t a) -> Parser t ()
skipMany0 p = skipMany1 p <|> pure ()

skipMany1 :: (Parser t a) -> Parser t ()
skipMany1 p = many1 p >>| pure ()

sepBy0 :: (Parser t a) (Parser t s) -> Parser t [a]
sepBy0 p s = sepBy1 p s <|> pure []

sepBy1 :: (Parser t a) (Parser t s) -> Parser t [a]
sepBy1 p s = (\x xs -> [x : xs]) <$> p <*> many0 (s >>| p)

endBy0 :: (Parser t a) (Parser t s) -> Parser t [a]
endBy0 p s = endBy1 p s <|> pure []

endBy1 :: (Parser t a) (Parser t s) -> Parser t [a]
endBy1 p s = sepBy1 p s >>= \xs -> s >>| pure xs







