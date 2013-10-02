module Prim
	( Tbl, Atom(T, F, NIL, STR, NUM)
	, mk, ez, toList, fromList, tcons, tcar, tcdr, tmap
	) where
import Data.List

data Atom = T | F | NIL | STR String | NUM Double deriving(Eq,Ord)
data Tbl a = Tbl [a] [(Atom,a)] deriving(Eq,Ord)
nums = map NUM [1..]
mk a b = Tbl a (sort b)
ez (Tbl a b) = (a,b)
toList (Tbl a b) = zip nums a ++ b
fromList l = r 1 [] [] (sort l) where
	r i o u [] = mk (reverse o) u
	r i o u ((k,v):l) = if (NUM i == k)
		then r (i+1) (v:o) u l
		else r i o ((k,v):u) l

tcons a (Tbl o u) = Tbl (a:o) u
tcar (Tbl (a:b) u) = a
tcdr (Tbl (a:b) u) = Tbl b u
tmap f (Tbl o u) = Tbl (map f o) (map (\(k,v)->(k,f v)) u)

instance Show Atom where
	show T = "#t"
	show F = "#f"
	show NIL = "#n"
	show (STR s) = "<" ++ s ++ ">"
	show (NUM n) = show n

instance Show a => Show(Tbl a) where
	show (Tbl o u) = "{" ++ mix (map show o ++ map pair u) ++ "}" where
		pair(k,v) = show k ++ "=" ++ show v
		mix = concat . intersperse ", "
