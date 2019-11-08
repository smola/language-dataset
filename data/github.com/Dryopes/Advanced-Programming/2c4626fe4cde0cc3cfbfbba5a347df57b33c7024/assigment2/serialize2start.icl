module serialize2start

/*
	Definition for assignment 2 in AFP 2016
	Pieter Koopman pieter@cs.ru.nl
	September 2016
*/

import StdEnv, StdMaybe

/* 	1.1 UNIT can only be UNIT, therefore it can only be true. Therefore the alternatives are wrong
*	1.2	No, because if they have different names, they are different types and different types can not be checked against each other
*	1.3 Both are UNIT. No because they have different types.
*/

class serialize a where
	write :: a [String] -> [String]
	read  :: [String] -> Maybe (a,[String])

instance serialize Bool where
	write b c = [toString b:c]
	read ["True":r]  = Just (True,r)
	read ["False":r] = Just (False,r)
	read _ = Nothing

instance serialize Int where
	write i c = [toString i:c]
	read [s:r]
		# i = toInt s
		| s == toString i
			= Just (i,r)
			= Nothing
	read _ = Nothing

:: UNIT       = UNIT
:: EITHER a b = LEFT a | RIGHT b
:: PAIR   a b = PAIR a b
:: CONS   a   = CONS String a

:: ListG a :== EITHER (CONS UNIT) (CONS (PAIR a [a]))

instance serialize [a] | serialize a where  // to be imporved
	write l c = c
	read l = Nothing

:: Bin a = Leaf | Bin (Bin a) a (Bin a)
:: BinG a :== EITHER (CONS UNIT) (CONS (PAIR (Bin a) (PAIR a (Bin a))))

instance serialize (Bin a) | serialize a where // to be imporved
	write a c = c
	read l = Nothing

instance == (Bin a) | == a where // better use the generic approach
	(==) Leaf Leaf = True
	(==) (Bin l a r) (Bin k b s) = l == k && a == b && r == s
	(==) _ _ = False

Start = 
	[test True
	,test False
	,test 0
	,test 123
	,test -36
	,test [42]
	,test [0..4]
	,test [[True],[]]
	,test (Bin Leaf True Leaf)
	,test [Bin (Bin Leaf [1] Leaf) [2] (Bin Leaf [3] (Bin Leaf [4,5] Leaf))]
	,test [Bin (Bin Leaf [1] Leaf) [2] (Bin Leaf [3] (Bin (Bin Leaf [4,5] Leaf) [6,7] (Bin Leaf [8,9] Leaf)))]
	]

test :: a -> ([String],[String]) | serialize, == a
test a = 
	(if (isJust r)
		(if (fst jr == a)
			(if (isEmpty (tl (snd jr)))
				["Oke "]
				["Fail: not all input is consumed! ":snd jr])
			["Fail: Wrong result ":write (fst jr) []])
		["Fail: read result is Nothing "]
	, ["write produces ": s]
	)
	where
		s = write a ["\n"]
		r = read s
		jr = fromJust r
