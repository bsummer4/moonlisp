-- For now, accepted datatypes are just unquoted strings and tables.
-- The lua types are: nil bool number string function userdata thread table
-- Table Syntax: (1 2 3 a=3 b=4)
-- What still needs to be done before I can start working on the compiler?
--  TODO Allow multiple-character symbols.
--  TODO Code clean-up.
--   TODO "Unintersperse" is a shitty name.
--   TODO '=' is not a readable symbol and thus shouldn't be read as one.
--   TODO Use a map for LTable.
--   TODO Ban LNil for keys in LTable.
--  TODO Choose which lua forms we want to implements.
--  TODO Create a data types for our subset of the lua AST.
--  TODO Decide how to represent those syntactic constructs with sexps.
--  TODO Write a function to translate LVals to Lua AST trees.
--  TODO Write a pretty-printer for my sexps.
--  TODO Write a pretty-printer for lua AST.
--  TODO Combine the parser, translator, and lua AST printer.
--   This will work as a proof-of-concept compiler

-- Thoughts
--  Since there is no distinction between symbols and strings:
--   Is “hi” quoted or unquoted?
--    (“+” 3 4)
--    (,“+” 3 4)
--    (append “hi” tommy)
--    (append '“hi” tommy)
--    I think quoted is better. '“hihi” is a PITA.

--------------------------------------------------
-- Data Declations
--------------------------------------------------
import qualified Data.Map as M
data OneOrTwo a = Two a a | One a
data LVal = LEOF | LNil | LSep | LSym String | LTable [(LVal,LVal)]
	deriving (Read, Eq, Ord)

instance Show LVal where
	show e = case e of
		LEOF -> "#<eof>"
		LNil -> "#n"
		LSym s -> s
		LTable [] -> "()"
		LTable es -> "(" ++ concat(map showpair es) ++ ")" where
			showpair(k,v) = " " ++ show k ++ "=" ++ show v ++ " "

--------------------------------------------------
-- Reading
--------------------------------------------------
symChars = "=" ++ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']
wsChars = " \n\t"
data ChkState = OrdEs Int | NamedEs
checkTbl l = (\(m,_)->m) $ foldl f (M.empty,OrdEs 0) l where
	f (m,OrdEs i) (One e) = (insert (LSym(show i)) e m, OrdEs(i+1))
	f (m,OrdEs i) (Two k v) = f (m,NamedEs) (Two k v)
	f (m,NamedEs) (One e) = error "Ordered items cannot come after named ones."
	f (m,NamedEs) (Two k v) = (insert k v m, NamedEs)
	insert k v m = M.insertWithKey collide k v m
	collide k _ _ = error("Table has multiple defintions for key: " ++ show k)

unintersperse sep l = finalize $ foldl r (False,[]) l where
	r (True,One k:acc) v = (False,Two k v:acc)
	r (True,_) _ = error "misplaced ="
	r (False,acc) e = if sep e then (True,acc) else (False,One e:acc)
	finalize (True,_) = error "misplaced ="
	finalize (False,l) = reverse l

parsetbl = M.toList . checkTbl . unintersperse (== LSym "=")
lreadTbl stream = r [] stream where
	r acc s = case s of
		[] -> error "Unterminated List"
		')':cs -> (LTable $ parsetbl $ reverse acc,cs)
		c:cs -> if c `elem` wsChars then r acc cs else case lread (c:cs) of
			(e,remain) -> r (e:acc) remain

lreadSym soFar stream = case (soFar,stream) of
	("=",s) -> (LSym "=",s)
	(sym,[]) -> (LSym(reverse sym),[])
	(sym,c:cs) -> if and[not(c == '='), c `elem` symChars]
		then lreadSym (c:sym) cs
		else (LSym(reverse sym),c:cs)

lread stream = case stream of
	[] -> (LEOF,[])
	' ':cs -> lread cs
	'\n':cs -> lread cs
	'(':cs -> lreadTbl cs
	c:cs -> if c `elem` symChars then lreadSym [c] cs else
		error $ "unexpected '" ++ [c] ++ "'"

--------------------------------------------------
-- Main Loop
--------------------------------------------------
main = interact (\s -> rpl s ++ "\n") where
	rpl' s = rpl s ++ "\n"
	rpl s = case lread s of {(LEOF,[])->""; (e,s')->show e ++ rpl s'}
