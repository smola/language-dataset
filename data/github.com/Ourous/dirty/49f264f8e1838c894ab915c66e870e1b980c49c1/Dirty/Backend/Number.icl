implementation module Dirty.Backend.Number

import Math.Geometry, StdEnv, Data.Func, Text, Data.Maybe
import Dirty.Backend.Rational
import Dirty.Backend

:: Number
	= Zero
	| Re !(Magnitude Sign Rational)
	| Im !(Magnitude Sign Rational)
	| Cx !(Magnitude Directed Complex)
	| Invalid

:: Magnitude inf fin
	= Fin !fin
	| Inf !inf
	
:: Sign
	= Positive
	| Negative
	
:: Complex
	= {
		re :: !Rational,
		im :: !Rational
	}
	
:: Directed
	= Directed

one_i :: Number
one_i =: (Im (Fin one))

instance == Sign where
	(==) Positive Positive = True
	(==) Negative Negative = True
	(==) _ _ = False

instance ~ Sign where
	~ Positive = Negative
	~ Negative = Positive
	
instance * Sign where
	(*) Positive Positive = Positive
	(*) Negative Negative = Positive
	(*) _ _ = Negative
	
instance sign Sign where
	sign Positive = 1
	sign Negative = -1
	
INT_MAX =: IF_INT_64_OR_32 9223372036854775807 2147483647
INT_MIN =: IF_INT_64_OR_32 -9223372036854775808 -2147483648
	
VAL_SIGN val
	:== case val of
		(Inf val) = val
		(Fin val) = FIN_SIGN val
	
FIN_SIGN atomic
	:== if(sign atomic < 0) Negative Positive

IS_ZERO atomic
	:== atomic == zero

IS_NOT_NAN atomic // Don't use outside of `handle`
	:== isNaN (toReal atomic)
//		_ = True

IS_FINITE atomic // Don't use outside of `handle`
	:== (atomic-atomic) == zero

// number implementations

handle number //:== number
	:== case number of
		(Re (Fin val))
			| RATIONAL_IS_ZERO val = Zero
			| RATIONAL_IS_FINITE val = number
			| RATIONAL_IS_NAN val = Invalid
			| otherwise = (Re (Inf (FIN_SIGN val)))
		(Im (Fin val))
			| RATIONAL_IS_ZERO val = Zero
			| RATIONAL_IS_FINITE val = number
			| RATIONAL_IS_NAN val = Invalid
			| otherwise = (Im (Inf (FIN_SIGN val)))
		(Cx (Fin {re, im}))
			| RATIONAL_IS_NAN re || RATIONAL_IS_NAN im = Invalid
			| RATIONAL_IS_ZERO re
				| RATIONAL_IS_ZERO im = Zero
				| RATIONAL_IS_FINITE im = (Im (Fin im))
				| otherwise = (Im (Inf (FIN_SIGN im)))
			| RATIONAL_IS_FINITE re
				| RATIONAL_IS_ZERO im = (Re (Fin re))
				| RATIONAL_IS_FINITE im = number
				| otherwise = (Im (Inf (FIN_SIGN im)))
			| otherwise
				| RATIONAL_IS_FINITE im = (Re (Inf (FIN_SIGN re)))
				| otherwise = (Cx (Inf Directed))
		_ = number
		
		
instance + Number where
	(+) Invalid _ = Invalid
	(+) _ Invalid = Invalid
	(+) Zero val = val
	(+) val Zero = val
	(+) (Re (Fin lhs)) (Re (Fin rhs)) // TODO merge Rational into this module because it'll literally double the speed
		= handle (Re (Fin (lhs + rhs)))
	(+) (Re (Fin lhs)) (Im (Fin rhs))
		= (Cx (Fin {re=lhs, im=rhs}))
	(+) (Im (Fin lhs)) (Re (Fin rhs))
		= (Cx (Fin {re=rhs, im=lhs}))
	(+) (Im (Fin lhs)) (Im (Fin rhs))
		= handle (Im (Fin (lhs + rhs)))
	(+) (Re (Fin lhs)) (Cx (Fin rhs))
		= handle (Cx (Fin {rhs& re=lhs+rhs.re}))
	(+) (Im (Fin lhs)) (Cx (Fin rhs))
		= handle (Cx (Fin {rhs& im=lhs+rhs.im}))
	(+) (Cx (Fin lhs)) (Re (Fin rhs))
		= handle (Cx (Fin {lhs& re=lhs.re+rhs}))
	(+) (Cx (Fin lhs)) (Im (Fin rhs))
		= handle (Cx (Fin {lhs& im=lhs.im+rhs}))
	(+) (Cx (Fin lhs)) (Cx (Fin rhs))
		= handle (Cx (Fin {re=lhs.re+rhs.re, im=lhs.im+rhs.im}))
	(+) (Re (Inf lhs)) (Re (Inf rhs))
		| lhs == rhs = (Re (Inf lhs))
		| otherwise = Invalid
	(+) (Im (Inf lhs)) (Im (Inf rhs))
		| lhs == rhs = (Im (Inf lhs))
		| otherwise = Invalid
	(+) (Cx (Inf _)) (Cx (Inf _)) = Invalid
	(+) (Cx (Inf _)) (Re (Inf _)) = Invalid
	(+) (Cx (Inf _)) (Im (Inf _)) = Invalid
	(+) (Cx (Inf _)) _ = (Cx (Inf Directed))
	(+) (Re (Inf _)) (Cx (Inf _)) = Invalid
	(+) (Im (Inf _)) (Cx (Inf _)) = Invalid
	(+) _ (Cx (Inf _)) = (Cx (Inf Directed))
	(+) (Im (Inf _)) (Re (Inf _)) = (Cx (Inf Directed))
	(+) (Re (Inf _)) (Im (Inf _)) = (Cx (Inf Directed))
	(+) (Im (Inf lhs)) _ = (Im (Inf lhs))
	(+) _ (Im (Inf rhs)) = (Im (Inf rhs))
	(+) (Re (Inf lhs)) _ = (Re (Inf lhs))
	(+) _ (Re (Inf rhs)) = (Re (Inf rhs))

instance - Number where
	(-) Invalid _ = Invalid
	(-) _ Invalid = Invalid
	(-) lhs Zero = lhs
	(-) Zero rhs = ~rhs // this is down here for performance
	(-) (Re (Fin lhs)) (Re (Fin rhs))
		= handle (Re (Fin (lhs - rhs)))
	(-) (Re (Fin lhs)) (Im (Fin rhs))
		= (Cx (Fin {re=lhs, im=(~rhs)}))
	(-) (Im (Fin lhs)) (Re (Fin rhs))
		= (Cx (Fin {re=(~rhs), im=lhs}))
	(-) (Im (Fin lhs)) (Im (Fin rhs))
		= handle (Im (Fin (lhs - rhs)))
	(-) (Re (Fin lhs)) (Cx (Fin rhs))
		= handle (Cx (Fin {re=lhs-rhs.re, im=(~rhs.im)}))
	(-) (Im (Fin lhs)) (Cx (Fin rhs))
		= handle (Cx (Fin {re=(~rhs.re), im=lhs-rhs.im}))
	(-) (Cx (Fin lhs)) (Re (Fin rhs))
		= handle (Cx (Fin {lhs& re=lhs.re-rhs}))
	(-) (Cx (Fin lhs)) (Im (Fin rhs))
		= handle (Cx (Fin {lhs& im=lhs.im-rhs}))
	(-) (Cx (Fin lhs)) (Cx (Fin rhs))
		= handle (Cx (Fin {re=lhs.re-rhs.re, im=lhs.im-rhs.im}))
	(-) (Re (Inf lhs)) (Re (Inf rhs))
		| lhs <> rhs = (Re (Inf lhs))
		| otherwise = Invalid
	(-) (Im (Inf lhs)) (Im (Inf rhs))
		| lhs <> rhs = (Im (Inf lhs))
		| otherwise = Invalid
	(-) (Cx (Inf _)) (Cx (Inf _)) = Invalid
	(-) (Cx (Inf _)) (Re (Inf _)) = Invalid
	(-) (Cx (Inf _)) (Im (Inf _)) = Invalid
	(-) (Cx (Inf _)) _ = (Cx (Inf Directed))
	(-) (Re (Inf _)) (Cx (Inf _)) = Invalid
	(-) (Im (Inf _)) (Cx (Inf _)) = Invalid
	(-) _ (Cx (Inf _)) = (Cx (Inf Directed))
	(-) (Re (Inf _)) (Im (Inf _)) = (Cx (Inf Directed))
	(-) (Im (Inf _)) (Re (Inf _)) = (Cx (Inf Directed))
	(-) (Im (Inf lhs)) _ = (Im (Inf lhs))
	(-) _ (Im (Inf rhs)) = (Im (Inf (~rhs)))
	(-) (Re (Inf lhs)) _ = (Re (Inf lhs))
	(-) _ (Re (Inf rhs)) = (Re (Inf (~rhs)))
		
instance zero Number where
	zero = Zero
	
instance * Number where
	(*) Invalid _ = Invalid
	(*) _ Invalid = Invalid
	(*) Zero (Cx (Inf _)) = Invalid
	(*) Zero (Re (Inf _)) = Invalid
	(*) Zero (Im (Inf _)) = Invalid
	(*) Zero _ = Zero
	(*) (Cx (Inf _)) Zero = Invalid
	(*) (Re (Inf _)) Zero = Invalid
	(*) (Im (Inf _)) Zero = Invalid
	(*) _ Zero = Zero
	(*) (Re (Fin lhs)) (Re (Fin rhs))
		= handle (Re (Fin (lhs * rhs)))
	(*) (Re (Fin lhs)) (Im (Fin rhs))
		= handle (Im (Fin (lhs * rhs)))
	(*) (Im (Fin lhs)) (Re (Fin rhs))
		= handle (Im (Fin (lhs * rhs)))
	(*) (Im (Fin lhs)) (Im (Fin rhs))
		= handle (Re (Fin (~(lhs * rhs))))
	(*) (Re (Fin lhs)) (Cx (Fin rhs))
		= handle (Cx (Fin {re=lhs*rhs.re, im=lhs*rhs.im}))
	(*) (Im (Fin lhs)) (Cx (Fin rhs))
		= handle (Cx (Fin {re=(~(lhs*rhs.im)), im=lhs*rhs.re}))
	(*) (Cx (Fin lhs)) (Re (Fin rhs))
		= handle (Cx (Fin {re=lhs.re*rhs, im=lhs.im*rhs}))
	(*) (Cx (Fin lhs)) (Im (Fin rhs))
		= handle (Cx (Fin {re=(~(lhs.im*rhs)), im=lhs.re*rhs}))
	(*) (Cx (Fin lhs)) (Cx (Fin rhs))
		= handle (Cx (Fin {re=lhs.re*rhs.re-lhs.im*rhs.im, im=lhs.im*rhs.re+rhs.im*lhs.re}))
	(*) (Cx _) _ = (Cx (Inf Directed))
	(*) _ (Cx _) = (Cx (Inf Directed))
	(*) (Re lhs) (Re rhs) = (Re (Inf (VAL_SIGN lhs * VAL_SIGN rhs)))
	(*) (Re lhs) (Im rhs) = (Im (Inf (VAL_SIGN lhs * VAL_SIGN rhs)))
	(*) (Im lhs) (Re rhs) = (Im (Inf (VAL_SIGN lhs * VAL_SIGN rhs)))
	(*) (Im lhs) (Im rhs) = (Re (Inf (~(VAL_SIGN lhs * VAL_SIGN rhs))))
	
		
instance / Number where
	(/) Invalid _ = Invalid
	(/) _ Invalid = Invalid
	(/) Zero Zero = Invalid
	(/) Zero _ = Zero
	(/) (Re lhs) Zero = (Re (Inf (VAL_SIGN lhs)))
	(/) (Im lhs) Zero = (Im (Inf (VAL_SIGN lhs)))
	(/) (Cx _) Zero = (Cx (Inf Directed))
	(/) (Re (Fin lhs)) (Re (Fin rhs))
		= handle (Re (Fin (lhs / rhs)))
	(/) (Re (Fin lhs)) (Im (Fin rhs))
		= handle (Im (Fin (~(lhs / rhs))))
	(/) (Im (Fin lhs)) (Re (Fin rhs))
		= handle (Im (Fin (lhs / rhs)))
	(/) (Im (Fin lhs)) (Im (Fin rhs))
		= handle (Re (Fin (lhs / rhs)))
	(/) (Re (Fin lhs)) (Cx (Fin rhs))
		# denominator = rhs.re * rhs.re + rhs.im * rhs.im
		= handle (Cx (Fin {re=((lhs * rhs.re) / denominator), im=(~((lhs * rhs.im) / denominator))}))
	(/) (Im (Fin lhs)) (Cx (Fin rhs))
		# denominator = rhs.re * rhs.re + rhs.im * rhs.im
		= handle (Cx (Fin {re=((lhs * rhs.im) / denominator), im=((lhs * rhs.re) / denominator)}))
	(/) (Cx (Fin lhs)) (Re (Fin rhs))
		= handle (Cx (Fin {re=(lhs.re / rhs), im=(lhs.im / rhs)}))
	(/) (Cx (Fin lhs)) (Im (Fin rhs))
		= handle (Cx (Fin {re=(lhs.im / rhs), im=(~(lhs.re / rhs))}))
	(/) (Cx (Fin lhs)) (Cx (Fin rhs))
		# denominator = rhs.re * rhs.re + rhs.im * rhs.im
		= handle (Cx (Fin {re=((lhs.re * rhs.re + lhs.im * rhs.im) / denominator), im=((lhs.im * rhs.re - lhs.re * rhs.im) / denominator)}))
	(/) (Re (Inf lhs)) (Re (Fin rhs)) = (Re (Inf (lhs * FIN_SIGN rhs)))
	(/) (Re (Inf lhs)) (Im (Fin rhs)) = (Im (Inf (~(lhs * FIN_SIGN rhs))))
	(/) (Im (Inf lhs)) (Re (Fin rhs)) = (Im (Inf (lhs * FIN_SIGN rhs)))
	(/) (Im (Inf lhs)) (Im (Fin rhs)) = (Re (Inf (lhs * FIN_SIGN rhs)))
	(/) (Cx _) (Re (Fin _)) = (Cx (Inf Directed))
	(/) (Cx _) (Im (Fin _)) = (Cx (Inf Directed))
	(/) _ (Cx (Fin _)) = (Cx (Inf Directed))
	(/) (Re (Fin _)) _ = Zero
	(/) (Im (Fin _)) _ = Zero
	(/) (Cx (Fin _)) _ = Zero
	(/) _ _ = Invalid
	
instance one Number where
	one = (Re (Fin one))

instance ^ Number where
	(^) Invalid _ = Invalid
	(^) _ Invalid = Invalid
	(^) _ Zero = Zero
	(^) Zero _ = Zero
	(^) (Re (Fin lhs)) (Re (Fin rhs))
		= handle (Re (Fin (lhs ^ rhs)))
	//(^) (Rational _) (Imaginary _) = abort "Unimplemented Operation: Re^Im"
	//(^) (Imaginary _) (Rational _) = abort "Unimplemented Operation: Im^Re"
	//(^) (Imaginary _) (Imaginary _) = abort "Unimplemented Operation: Im^Im"
	//(^) (Rational _) (Complex _ _) = abort "Unimplemented Operation: Re^Cx"
	//(^) (Imaginary _) (Complex _ _) = abort "Unimplemented Operation: Im^Cx"
	//(^) (Complex _ _) (Rational _) = abort "Unimplemented Operation: Cx^Re"
	//(^) (Complex _ _) (Imaginary _) = abort "Unimplemented Operation: Cx^Im"
	//(^) (Complex _ _) (Complex _ _) = abort "Unimplemented Operation: Cx^Cx"

instance abs Number where
	abs Invalid = Invalid
	abs Zero = Zero
	abs (Re (Fin val)) = (Re (Fin (abs val)))
	abs (Im (Fin val)) = (Re (Fin (abs val)))
	abs (Cx (Fin {re, im})) = (Re (Fin (sqrt(re*re+im*im))))
	abs _ = (Re (Inf Positive))
	
instance ~ Number where
	(~) Invalid = Invalid
	(~) Zero = Zero
	(~) (Re (Fin val)) = (Re (Fin (~val)))
	(~) (Im (Fin val)) = (Im (Fin (~val)))
	(~) (Cx (Fin {re, im})) = (Cx (Fin {re= ~re, im= ~im}))
	(~) (Re (Inf val)) = (Re (Inf (~val)))
	(~) (Im (Inf val)) = (Im (Inf (~val)))
	(~) (Cx (Inf _)) = (Cx (Inf Directed))
	
instance == Number where
	(==) Zero Zero = True
	(==) (Re (Fin lhs)) (Re (Fin rhs)) = lhs == rhs
	(==) (Im (Fin lhs)) (Im (Fin rhs)) = lhs == rhs
	(==) (Cx (Fin lhs)) (Cx (Fin rhs)) = lhs.re == rhs.re && lhs.im == rhs.im
	(==) _ _ = False

instance < Number where
	(<) Invalid _ = False
	(<) _ Invalid = False
	//(<) Infinity _ = True
	//(<) _ Infinity = True
	(<) Zero Zero = False
	(<) Zero (Re rhs) = VAL_SIGN rhs == Positive
	(<) (Re lhs) Zero = VAL_SIGN lhs == Negative
	(<) Zero (Im rhs) = VAL_SIGN rhs == Positive
	(<) (Im lhs) Zero = VAL_SIGN lhs == Negative
	(<) (Re (Fin lhs)) (Re (Fin rhs)) = lhs < rhs
	(<) (Im (Fin lhs)) (Im (Fin rhs)) = lhs < rhs
	(<) lhs=:(Cx (Fin _)) rhs=:(Cx (Fin _)) = toReal lhs < toReal rhs

instance mod Number where
	(mod) Invalid _ = Invalid
	(mod) _ Invalid = Invalid
	(mod) _ Zero = Invalid
	(mod) Zero _ = Zero
	//(mod) Infinity _ = Invalid
	//(mod) val Infinity = val
	(mod) (Re (Fin lhs)) (Re (Fin rhs))
		= handle (Re (Fin (lhs mod rhs)))
	(mod) (Re (Fin lhs)) (Im (Fin rhs))
		= handle (Im (Fin (~(lhs mod rhs))))
	(mod) (Im (Fin lhs)) (Re (Fin rhs))
		= handle (Im (Fin (lhs mod rhs)))
	(mod) (Im (Fin lhs)) (Im (Fin rhs))
		= handle (Re (Fin (lhs mod rhs)))
	(mod) (Re (Fin lhs)) (Cx (Fin rhs))
		# denominator = rhs.re * rhs.re + rhs.im * rhs.im
		= handle (Cx (Fin {re=((lhs * rhs.re) mod denominator), im=(~((lhs * rhs.im) mod denominator))}))
	(mod) (Im (Fin lhs)) (Cx (Fin rhs))
		# denominator = rhs.re * rhs.re + rhs.im * rhs.im
		= handle (Cx (Fin {re=((lhs * rhs.im) mod denominator), im=((lhs * rhs.re) mod denominator)}))
	(mod) (Cx (Fin lhs)) (Re (Fin rhs))
		= handle (Cx (Fin {re=(lhs.re mod rhs), im=(lhs.im mod rhs)}))
	(mod) (Cx (Fin lhs)) (Im (Fin rhs))
		= handle (Cx (Fin {re=(lhs.im mod rhs), im=(~(lhs.re mod rhs))}))
	(mod) (Cx (Fin lhs)) (Cx (Fin rhs))
		# denominator = rhs.re * rhs.re + rhs.im * rhs.im
		= handle (Cx (Fin {re=((lhs.re * rhs.re + lhs.im * rhs.im) mod denominator), im=((lhs.im * rhs.re - lhs.re * rhs.im) mod denominator)}))
	(mod) _ _ = Invalid

instance gcd Number where
	gcd Invalid _ = Invalid
	gcd _ Invalid = Invalid
	gcd Zero _ = Invalid
	gcd _ Zero = Invalid
	gcd (Re (Fin lhs)) (Re (Fin rhs))
		= handle (Re (Fin (gcd lhs rhs)))
	gcd (Im (Fin lhs)) (Im (Fin rhs))
		= handle (Im (Fin (gcd lhs rhs)))
	gcd (Cx (Fin _)) (Cx (Fin _))
		= abort "GCD of Complex not yet implemented!"
	
instance lcm Number where
	lcm Invalid _ = Invalid
	lcm _ Invalid = Invalid
	lcm Zero _ = Zero
	lcm _ Zero = Zero
	lcm (Re (Fin lhs)) (Re (Fin rhs))
		= handle (Re (Fin (lcm lhs rhs)))

instance toInt Number where
	toInt Invalid = 0
	toInt Zero = 0
	toInt (Re (Fin val)) = toInt val
	toInt (Im (Fin val)) = toInt val
	toInt (Cx (Fin {re, im})) = toInt (sqrt (re*re + im*im)) * sign re * sign im
	toInt (Re (Inf Positive)) = INT_MAX
	toInt (Re (Inf Negative)) = INT_MIN
	toInt (Im (Inf Positive)) = INT_MAX
	toInt (Im (Inf Negative)) = INT_MIN
	toInt (Cx _) = 0
	
instance toReal Number where
	toReal Zero = 0.0
	toReal (Re (Fin val)) = toReal val
	toReal (Im (Fin val)) = toReal val
	toReal (Cx (Fin {re, im})) = toReal (sqrt (re*re + im*im)) * (toReal (sign re * sign im))
	toReal (Re (Inf val)) = (toReal (sign val)) / 0.0
	toReal (Im (Inf val)) = (toReal (sign val)) / 0.0
	toReal _ = NaN//0.0/0.0
	
instance toBool Number where
	toBool Zero = False
	toBool Invalid = False
	toBool (Re (Inf _)) = False
	toBool (Im (Inf _)) = False
	toBool (Cx (Inf _)) = False
	toBool _ = True
	
instance toString Number where
	toString Zero = "0"
	toString Invalid = "NaN"
	toString (Re (Fin val)) = toString val
	toString (Im (Fin val)) = toString val +++ "i"
	toString (Cx (Fin {re, im})) = toString im +++ "i" +++ toString re
	toString (Re (Inf val)) = if(val == Negative) "-ReInf" "ReInf"
	toString (Im (Inf val)) = if(val == Negative) "-ImInf" "ImInf"
	toString (Cx _) = "?CxInf"
	
instance fromInt Number where
	fromInt 0 = Zero
	fromInt val = (Re (Fin (fromInt val)))

instance fromReal Number where
	fromReal 0.0 = Zero
	fromReal -0.0 = Zero
	fromReal val = handle (Re (Fin (fromReal val)))

instance fromBool Number where
	fromBool True = (Re (Fin (~one)))
	fromBool False = Zero

instance fromString Number where
	fromString "0" = Zero
	fromString "i" = (Im (Fin one))
	fromString "-" = (Re (Fin (~one)))
	fromString "-i" = (Im (Fin (~one)))
	fromString "NaN" = Invalid
	fromString "?CxInf" = (Cx (Inf Directed))
	fromString "-ReInf" = (Re (Inf Negative))
	fromString "ReInf" = (Re (Inf Positive))
	fromString "-ImInf" = (Im (Inf Negative))
	fromString "ImInf" = (Im (Inf Positive))
	fromString str
		= handle case split "i" str of
			[re] = (Re (Fin (fromString re)))
			[im, ""] = (Im (Fin (fromString im)))
			[im, "-"] = (Cx (Fin {re= ~one, im=fromString im}))
			[im, re] = (Cx (Fin {re=fromString re, im=fromString im}))
			_ = abort ("Bad string format for number: '"+++str+++"'")

instance repr Number where
	repr _ num = [c \\ c <-: toString num]
	
instance eval Number where
	eval ['0'] = Just Zero
	eval ['i'] = Just (Im (Fin one))
	eval ['-'] = Just (Re (Fin (~one)))
	eval ['-i'] = Just (Im (Fin (~one)))
	eval ['NaN'] = Just Invalid
	eval ['?CxInf'] = Just (Cx (Inf Directed))
	eval ['-ReInf'] = Just (Re (Inf Negative))
	eval ['ReInf'] = Just (Re (Inf Positive))
	eval ['-ImInf'] = Just (Im (Inf Negative))
	eval ['ImInf'] = Just (Im (Inf Positive))
	eval str
		= case split ['i'] str of
			[re] = Just (Re (Fin (fromString (toString re))))
			[im, []] = Just (Im (Fin (fromString (toString im))))
			[im, ['-']] = Just (Cx (Fin {re= ~one, im=fromString (toString im)}))
			[im, re] = Just (Cx (Fin {re=fromString (toString re), im=fromString (toString im)}))
			_ = Nothing
	
instance disp Number where
	disp num = [(fromInt o toInt) num]


instance ln Number where
	ln Invalid = Invalid
	//ln Infinity = Infinity
	ln Zero = Invalid
	ln (Re (Fin val)) = handle (Re (Fin (ln val)))
	ln (Re (Inf Positive)) = (Re (Inf Positive))
	//ln (Imaginary _) = abort "Unimplemented Operation: ln Im"
	//ln (Complex _ _) = abort "Unimplemented Operation: ln Cx"
	
instance log10 Number where
	log10 Invalid = Invalid
	//log10 Infinity = Infinity
	log10 Zero = Invalid
	log10 (Re (Fin val)) = handle (Re (Fin (log10 val)))
	log10 (Re (Inf Positive)) = (Re (Inf Positive))
	//log10 (Imaginary _) = abort "Unimplemented Operation: log10 Im"
	//log10 (Complex _ _) = abort "Unimplemented Operation: log10 Cx"
	
instance exp Number where
	exp Invalid = Invalid
	//exp Infinity = Infinity
	exp Zero = one
	exp (Re (Fin val)) = handle (Re (Fin (exp val)))
	exp (Re (Inf Positive)) = (Re (Inf Positive))
	exp (Re (Inf Negative)) = Zero
	//exp (Imaginary _) = abort "Unimplemented Operation: exp Im"
	//exp (Complex _ _) = abort "Unimplemented Operation: exp Cx"
	
instance sqrt Number where
	sqrt Invalid = Invalid
	//sqrt Infinity = Infinity
	sqrt Zero = Zero
	sqrt (Re (Fin val))
		| FIN_SIGN val <> Negative
			= handle (Re (Fin (sqrt val)))
		| otherwise
			= handle (Im (Fin (sqrt (abs val))))
	//sqrt (Imaginary _) = abort "Unimplemented Operation: sqrt Im"
	//sqrt (Complex _ _) = abort "Unimplemented Operation: sqrt Cx"
	
instance sin Number where
	sin Invalid = Invalid
	//sin Infinity = Invalid
	sin Zero = Zero
	sin (Re (Fin val)) = handle (Re (Fin (sin val)))
	sin (Re _) = Invalid
	//sin (Imaginary _) = abort "Unimplemented Operation: sin Im"
	//sin (Complex _ _) = abort "Unimplemented Operation: sin Cx"
	
instance cos Number where
	cos Invalid = Invalid
	//cos Infinity = Invalid
	cos Zero = one
	cos (Re (Fin val)) = handle (Re (Fin (cos val)))
	cos (Re _) = Invalid
	//cos (Imaginary _) = abort "Unimplemented Operation: cos Im"
	//cos (Complex _ _) = abort "Unimplemented Operation: cos Cx"
	
instance tan Number where
	tan Invalid = Invalid
	//tan Infinity = Invalid
	tan Zero = Zero
	tan (Re (Fin val)) = handle (Re (Fin (tan val)))
	tan (Re _) = Invalid
	//tan (Imaginary _) = abort "Unimplemented Operation: tan Im"
	//tan (Complex _ _) = abort "Unimplemented Operation: tan Cx"
	
instance asin Number where
	asin Invalid = Invalid
	//asin Infinity = Infinity
	asin Zero = Zero
	asin (Re (Fin val)) = handle (Re (Fin (asin val)))
	asin (Re (Inf val)) = (Im (Inf (~val)))
	//asin (Imaginary _) = abort "Unimplemented Operation: asin Im"
	//asin (Complex _ _) = abort "Unimplemented Operation: asin Cx"
	
instance acos Number where
	acos Invalid = Invalid
	acos Zero = (Re (Fin (fromReal (pi/2.0))))
	acos (Re (Fin val)) = handle (Re (Fin (acos val)))
	acos (Re val) = (Im val) // inf
	acos (Im (Inf val)) = (Im (Inf (~val)))
	//acos (Imaginary _) = abort "Unimplemented Operation: acos Im"
	//acos (Complex _ _) = abort "Unimplemented Operation: acos Cx"
	
instance atan Number where
	atan Invalid = Invalid
	//atan Infinity = (Rational (Real (pi/2.0)))
	atan Zero = Zero
	atan (Re (Fin val)) = handle (Re (Fin (atan val)))
	atan (Re (Inf val)) = fromReal (pi/2.0 * toReal (sign val))
	//atan (Imaginary _) = abort "Unimplemented Operation: atan Im"
	//atan (Complex _ _) = abort "Unimplemented Operation: atan Cx"
	
INT_OPER op lhs rhs :== (fromInt (op (ENTIER lhs) (ENTIER rhs)))

ENTIER val
	:== entier (toReal val)

bitOR :: !Number !Number -> Number
bitOR Invalid _ = Invalid
bitOR _ Invalid = Invalid
bitOR Zero rhs = rhs
bitOR lhs Zero = lhs
bitOR (Re (Fin lhs)) (Re (Fin rhs))
	= handle (Re (Fin (INT_OPER (bitor) lhs rhs)))
bitOR (Re (Fin lhs)) (Im (Fin rhs))
	= (Cx (Fin {re=lhs, im=rhs}))
bitOR (Im (Fin lhs)) (Re (Fin rhs))
	= (Cx (Fin {re=rhs, im=lhs}))	
bitOR (Im (Fin lhs)) (Im (Fin rhs))
	= handle (Im (Fin (INT_OPER (bitor) lhs rhs)))
bitOR (Re (Fin lhs)) (Cx (Fin rhs))
	= handle (Cx (Fin {rhs&re=INT_OPER (bitor) lhs rhs.re}))
bitOR (Im (Fin lhs)) (Cx (Fin rhs))
	= handle (Cx (Fin {rhs&im=INT_OPER (bitor) lhs rhs.im}))
bitOR (Cx (Fin lhs)) (Re (Fin rhs))
	= handle (Cx (Fin {lhs&re=INT_OPER (bitor) lhs.re rhs}))
bitOR (Cx (Fin lhs)) (Im (Fin rhs))
	= handle (Cx (Fin {lhs&im=INT_OPER (bitor) lhs.im rhs}))
bitOR (Cx (Fin lhs)) (Cx (Fin rhs))
	= handle (Cx (Fin {re=INT_OPER (bitor) lhs.re rhs.re, im=INT_OPER (bitor) lhs.im rhs.im}))
bitOR (Re _) (Im _) = (Cx (Inf Directed))
bitOR (Im _) (Re _) = (Cx (Inf Directed))
bitOR (Re (Fin _)) rhs = rhs
bitOR (Im (Fin _)) rhs = rhs
bitOR (Cx (Fin _)) rhs = rhs
bitOR lhs (Re (Fin _)) = lhs
bitOR lhs (Im (Fin _)) = lhs
bitOR lhs (Cx (Fin _)) = lhs
bitOR _ _ = Invalid

bitAND :: !Number !Number -> Number
bitAND Invalid _ = Invalid
bitAND _ Invalid = Invalid
bitAND Zero _ = Zero
bitAND _ Zero = Zero
bitAND (Re _) (Im _) = Zero
bitAND (Im _) (Re _) = Zero
bitAND (Re (Fin lhs)) (Re (Fin rhs))
	= handle (Re (Fin (INT_OPER (bitand) lhs rhs)))
bitAND (Im (Fin lhs)) (Im (Fin rhs))
	= handle (Im (Fin (INT_OPER (bitand) lhs rhs)))
bitAND (Re (Fin lhs)) (Cx (Fin rhs))
	= handle (Re (Fin (INT_OPER (bitand) lhs rhs.re)))
bitAND (Im (Fin lhs)) (Cx (Fin rhs))
	= handle (Im (Fin (INT_OPER (bitand) lhs rhs.im)))
bitAND (Cx (Fin lhs)) (Re (Fin rhs))
	= handle (Re (Fin (INT_OPER (bitand) lhs.re rhs)))
bitAND (Cx (Fin lhs)) (Im (Fin rhs))
	= handle (Im (Fin (INT_OPER (bitand) lhs.im rhs)))
bitAND (Cx (Fin lhs)) (Cx (Fin rhs))
	= handle (Cx (Fin {re=INT_OPER (bitand) lhs.re rhs.re, im=INT_OPER (bitand) lhs.im rhs.im}))
bitAND (Re (Fin _)) _ = Zero
bitAND (Im (Fin _)) _ = Zero
bitAND (Cx (Fin _)) _ = Zero
bitAND _ (Re (Fin _)) = Zero
bitAND _ (Im (Fin _)) = Zero
bitAND _ (Cx (Fin _)) = Zero
bitAND _ _ = Invalid

bitXOR :: !Number !Number -> Number
bitXOR Invalid _ = Invalid
bitXOR _ Invalid = Invalid
bitXOR Zero rhs = rhs
bitXOR lhs Zero = lhs
bitXOR (Re (Fin lhs)) (Re (Fin rhs))
	= handle (Re (Fin (INT_OPER (bitxor) lhs rhs)))
bitXOR (Re (Fin lhs)) (Im (Fin rhs))
	= (Cx (Fin {re=lhs, im=rhs}))
bitXOR (Im (Fin lhs)) (Re (Fin rhs))
	= (Cx (Fin {re=rhs, im=lhs}))
bitXOR (Im (Fin lhs)) (Im (Fin rhs))
	= handle (Im (Fin (INT_OPER (bitxor) lhs rhs)))
bitXOR (Re (Fin lhs)) (Cx (Fin rhs))
	= handle (Cx (Fin {rhs&re=INT_OPER (bitxor) lhs rhs.re}))
bitXOR (Im (Fin lhs)) (Cx (Fin rhs))
	= handle (Cx (Fin {rhs&im=INT_OPER (bitxor) lhs rhs.im}))
bitXOR (Cx (Fin lhs)) (Re (Fin rhs))
	= handle (Cx (Fin {lhs&re=INT_OPER (bitxor) lhs.re rhs}))
bitXOR (Cx (Fin lhs)) (Im (Fin rhs))
	= handle (Cx (Fin {lhs&im=INT_OPER (bitxor) lhs.im rhs}))
bitXOR (Cx (Fin lhs)) (Cx (Fin rhs))
	= handle (Cx (Fin {re=INT_OPER (bitxor) lhs.re rhs.re, im=INT_OPER (bitxor) lhs.im rhs.im}))
bitXOR (Re _) (Im _) = (Cx (Inf Directed))
bitXOR (Im _) (Re _) = (Cx (Inf Directed))
bitXOR _ _ = Invalid

bitNOT :: !Number -> Number
bitNOT Invalid = Invalid
bitNOT Zero = (Re (Fin (~one)))
bitNOT (Re (Fin val)) = handle (Re (Fin (fromInt (bitnot (ENTIER val)))))
bitNOT (Im (Fin val)) = handle (Im (Fin (fromInt (bitnot (ENTIER val)))))
bitNOT (Cx (Fin {re, im})) = handle (Cx (Fin {re=(fromInt(bitnot(ENTIER re))), im=(fromInt(bitnot(ENTIER im)))}))
bitNOT val = ~val

bitLEFT :: !Number !Number -> Number
bitLEFT Invalid _ = Invalid
bitLEFT _ Invalid = Invalid
bitLEFT Zero _ = Zero
bitLEFT lhs Zero = lhs
bitLEFT (Re (Fin lhs)) (Re (Fin rhs))
	= handle (Re (Fin (INT_OPER (<<) lhs rhs)))
	
bitRIGHT :: !Number !Number -> Number
bitRIGHT Invalid _ = Invalid
bitRIGHT _ Invalid = Invalid
bitRIGHT Zero _ = Zero
bitRIGHT lhs Zero = lhs
bitRIGHT (Re (Fin lhs)) (Re (Fin rhs))
	= handle (Re (Fin (INT_OPER (>>) lhs rhs)))
	
numFloor :: !Number -> Number
numFloor (Re (Fin val)) = handle (Re (Fin (fromInt (ENTIER val))))
numFloor (Im (Fin val)) = handle (Im (Fin (fromInt (ENTIER val))))
numFloor (Cx (Fin {re, im})) = handle (Cx (Fin {re=(fromInt (ENTIER re)), im=(fromInt (ENTIER im))}))
numFloor val = val

numCeiling :: !Number -> Number
numCeiling (Re (Fin val)) = handle (Re (Fin (fromInt (~(ENTIER (~val))))))
numCeiling (Im (Fin val)) = handle (Im (Fin (fromInt (~(ENTIER (~val))))))
numCeiling (Cx (Fin {re, im})) = handle (Cx (Fin {re=(fromInt(~(ENTIER(~re)))), im=(fromInt(~(ENTIER(~im))))}))
numCeiling val = val

numRound :: !Number -> Number
numRound (Re (Fin val)) = handle (Re (Fin (fromInt (toInt val))))
numRound (Im (Fin val)) = handle (Im (Fin (fromInt (toInt val))))
numRound (Cx (Fin {re, im})) = handle (Cx (Fin {re=(fromInt (toInt re)), im=(fromInt (toInt im))}))
numRound val = val

DEG_TO_RAD val :== (fromReal (toRad (deg (toReal val))))

toRadians :: !Number -> Number
toRadians (Re (Fin val)) = handle (Re (Fin (DEG_TO_RAD val)))
toRadians (Im (Fin val)) = handle (Im (Fin (DEG_TO_RAD val)))
toRadians (Cx (Fin {re, im})) = handle (Cx (Fin {re=DEG_TO_RAD re, im=DEG_TO_RAD im}))
toRadians val = val

RAD_TO_DEG val :== (fromReal (toDeg (rad (toReal val))))

toDegrees :: !Number -> Number
toDegrees (Re (Fin val)) = handle (Re (Fin (RAD_TO_DEG val)))
toDegrees (Im (Fin val)) = handle (Im (Fin (RAD_TO_DEG val)))
toDegrees (Cx (Fin {re, im})) = handle (Cx (Fin {re=RAD_TO_DEG re, im=RAD_TO_DEG im}))
toDegrees val = val

instance isInfinite Number
where
	isInfinite (Re (Inf _)) = True
	isInfinite (Im (Inf _)) = True
	isInfinite (Cx (Inf _)) = True
	isInfinite _ = False
instance isComplex Number
where
	isComplex (Cx _) = True
	isComplex _ = False
instance isRational Number
where
	isRational (Re _) = True
	isRational _ = False
instance isImaginary Number
where
	isImaginary (Im _) = True
	isImaginary _ = False
instance isInvalid Number
where
	isInvalid Invalid = True
	isInvalid _ = False
	
	
class toNumber a :: a -> Number
instance toNumber Int
where toNumber i = fromInt i
instance toNumber Real
where toNumber r = fromReal r
instance toNumber Bool
where toNumber b = fromBool b
instance toNumber String
where toNumber s = fromString s