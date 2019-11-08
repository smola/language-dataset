module peg

import StdEnv

import Control.Applicative
import Control.Monad
import Data.Func
import Data.Functor
import Data.Maybe
import Data.Monoid

:: Position :== Char
Inv :== ' '
Emp :== '.'
Peg :== 'o'

:: Direction :== Int
N :== 0
E :== 1
S :== 2
W :== 3

:: Coord     =  {x :: !Int  , y :: !Int}
:: Move      =  {c :: !Coord, d :: !Direction}
:: PegBoard :== {#{#Position}}

english :: PegBoard
english =
	{{Inv, Inv, Peg, Peg, Peg, Inv, Inv}
	,{Inv, Inv, Peg, Peg, Peg, Inv, Inv}
	,{Peg, Peg, Peg, Peg, Peg, Peg, Peg}
	,{Peg, Peg, Peg, Emp, Peg, Peg, Peg}
	,{Peg, Peg, Peg, Peg, Peg, Peg, Peg}
	,{Inv, Inv, Peg, Peg, Peg, Inv, Inv}
	,{Inv, Inv, Peg, Peg, Peg, Inv, Inv}
	}

french :: PegBoard
french =
	{{Inv, Inv, Peg, Peg, Peg, Inv, Inv}
	,{Inv, Peg, Peg, Peg, Peg, Peg, Inv}
	,{Peg, Peg, Peg, Emp, Peg, Peg, Peg}
	,{Peg, Peg, Peg, Peg, Peg, Peg, Peg}
	,{Peg, Peg, Peg, Peg, Peg, Peg, Peg}
	,{Inv, Peg, Peg, Peg, Peg, Peg, Inv}
	,{Inv, Inv, Peg, Peg, Peg, Inv, Inv}
	}

german :: PegBoard
german =
	{{Inv, Inv, Inv, Peg, Peg, Peg, Inv, Inv, Inv}
	,{Inv, Inv, Inv, Peg, Peg, Peg, Inv, Inv, Inv}
	,{Inv, Inv, Inv, Peg, Peg, Peg, Inv, Inv, Inv}
	,{Peg, Peg, Peg, Peg, Peg, Peg, Peg, Peg, Peg}
	,{Peg, Peg, Peg, Peg, Emp, Peg, Peg, Peg, Peg}
	,{Peg, Peg, Peg, Peg, Peg, Peg, Peg, Peg, Peg}
	,{Inv, Inv, Inv, Peg, Peg, Peg, Inv, Inv, Inv}
	,{Inv, Inv, Inv, Peg, Peg, Peg, Inv, Inv, Inv}
	,{Inv, Inv, Inv, Peg, Peg, Peg, Inv, Inv, Inv}
	}

bell :: PegBoard
bell = 
	{{Inv, Inv, Peg, Peg, Peg, Inv, Inv, Inv}
	,{Inv, Inv, Peg, Peg, Peg, Inv, Inv, Inv}
	,{Inv, Inv, Peg, Peg, Peg, Inv, Inv, Inv}
	,{Peg, Peg, Peg, Peg, Peg, Peg, Peg, Peg}
	,{Peg, Peg, Peg, Emp, Peg, Peg, Peg, Peg}
	,{Peg, Peg, Peg, Peg, Peg, Peg, Peg, Peg}
	,{Inv, Inv, Peg, Peg, Peg, Inv, Inv, Inv}
	,{Inv, Inv, Peg, Peg, Peg, Inv, Inv, Inv}
	}

diamond :: PegBoard
diamond = 
	{{Inv, Inv, Inv, Inv, Peg, Inv, Inv, Inv, Inv}
	,{Inv, Inv, Inv, Peg, Peg, Peg, Inv, Inv, Inv}
	,{Inv, Inv, Peg, Peg, Peg, Peg, Peg, Inv, Inv}
	,{Inv, Peg, Peg, Peg, Peg, Peg, Peg, Peg, Inv}
	,{Peg, Peg, Peg, Peg, Emp, Peg, Peg, Peg, Peg}
	,{Inv, Peg, Peg, Peg, Peg, Peg, Peg, Peg, Inv}
	,{Inv, Inv, Peg, Peg, Peg, Peg, Peg, Inv, Inv}
	,{Inv, Inv, Inv, Peg, Peg, Peg, Inv, Inv, Inv}
	,{Inv, Inv, Inv, Inv, Peg, Inv, Inv, Inv, Inv}
	}

solve :: !PegBoard -> Maybe [PegBoard]
solve b
	| 1 == length (getCoords Peg) = pure [b]
	= (\xs->[b:xs]) <$> foldr (<|>) empty [move b m >>= solve\\m<-moves]
where
	moves :: [Move]
	moves = [{c=c,d=d}\\c<-getCoords Emp, d<-[N,E,S,W]]

	getCoords :: Char -> [Coord]
	getCoords f = [{x=x,y=y}\\r<-:b & y<-[0..] , c<-:r & x<-[0..] | f == c]

move :: !PegBoard !Move -> Maybe PegBoard
move b {c=cc=:{x=tx,y=ty}, d}
	# sc=:{x=sx,y=sy} = transform cc d
	# fc=:{x=fx,y=fy} = transform sc d
	= getPos fc >>= \pa->if` (pa<>Peg) empty
	$ getPos sc >>= \pb->if` (pb<>Peg) empty
	$ getPos cc >>= \pc->if` (pc<>Emp) empty
	$ Just {{{c\\c<-:r}\\r<-:b} & [fy,fx]=Emp, [sy,sx]=Emp, [ty,tx]=Peg}
where
	getPos :: !Coord -> Maybe Position
	getPos {x,y}
		| y < 0 || x < 0 || y >= size b || x >= size b.[y] || b.[y,x] == Inv
			= Nothing
		= Just (b.[y,x])

transform :: !Coord !Direction -> Coord
transform c=:{x,y} d = case d of
	N = {c & y=y+1}
	S = {c & y=y-1}
	W = {c & x=x+1}
	E = {c & x=x-1}

Start = [map printPegBoard <$> solve b\\b<-[english,german,french,bell,diamond]]
where
	printPegBoard :: !PegBoard -> String
	printPegBoard b = foldr (+++) "\n" [r+++"\n"\\r<-:b]
