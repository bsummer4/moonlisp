{-# LANGUAGE UnicodeSyntax #-}

module Prim
	( Tbl, Atom(T, F, STR, NUM)
	, mk, ez, toList, fromList, tcons, tmap
	, module Prelude.Unicode
	) where
import Data.List
import Prelude.Unicode

data Atom = T | F | STR String | NUM Double deriving(Eq,Ord,Read)
data Tbl a = Tbl [a] [(Atom,a)] deriving(Eq,Ord,Read)
nums = map NUM [1..]
mk a b = Tbl a (sort b)
ez (Tbl a b) = (a,b)
toList (Tbl a b) = zip nums a ++ b
fromList l = r 1 [] [] (sort l) where
	r _ o u [] = mk (reverse o) u
	r i o u ((k,v):l) = if (NUM i≡k)
		then r (i+1) (v:o) u l
		else r i o ((k,v):u) l

tcons a (Tbl o u) = Tbl (a:o) u
tmap f (Tbl o u) = Tbl (map f o) (map (\(k,v)→(k,f v)) u)

instance Show Atom where
	show T = "#t"
	show F = "#f"
	show (STR s) = if needsQuotes s then "<" ++ s ++ ">" else s
	show (NUM n) = show n

needsQuotes "" = True
needsQuotes(c:cs) =
	not$and[c `elem` ("_"++['a'..'z']++['A'..'Z']), not$any(≡' ')cs]

instance Show a ⇒ Show(Tbl a) where
	show (Tbl o u) = "{" ++ mix (map show o ++ map pair u) ++ "}" where
		pair(k,v) = show k ++ "→" ++ show v
		mix = unwords
