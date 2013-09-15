-- For now, accepted datatypes are just unquoted strings and tables.
-- The lua types are: nil bool number string function userdata thread table
-- Table Syntax: (1 2 3 a=3 b=4)
-- What still needs to be done before I can start working on the compiler?
--  TODO Code clean-up.
--   TODO The logic for parsing tables is messy.
--  TODO Decide how to represent those syntactic constructs with sexps.
--  TODO Write a function to translate LVals to Lua AST trees.
--  TODO Write a pretty-printer for my sexps.
--  TODO Write a pretty-printer for lua AST.
--  TODO Combine the parser, translator, and lua AST printer.
--   This will work as a proof-of-concept compiler

import Data.List

--------------------------------------------------
-- Lua AST
--------------------------------------------------
data Lua
	= CALL Lua [Lua]
	| LITERAL LVal
	| IFELSE Lua [Lua] [Lua]
	| IF Lua [Lua]
	| FUNC String [String] [Lua]
	| LAMBDA [String] [Lua]
	| SET String Lua
	| GET String String
	| RETURN Lua
	| FOR Lua [Lua]
	| REPEAT [Lua] Lua
	| WHILE Lua [Lua]

comass = concat . intersperse "," . (\l->"":l)
comas = concat . intersperse "," . map show
stmts = concat . intersperse "\n" . (\l->"":l) . map show
instance Show Lua where
	show e = case e of
		CALL f args -> "(" ++ show f ++ "(" ++ comas args ++ "))"
		LITERAL v -> "(" ++ show v ++ ")"
		IFELSE a is es -> "if" ++ show a ++ stmts is ++ "else" ++ stmts es ++ "end"
		IF a is -> "if" ++ show a ++ stmts is ++ "end"
		FUNC n args body -> "function" ++ n ++ "(" ++ comass args ++ ")" ++ stmts body ++ "end"
		LAMBDA args body -> "function(" ++ comass args ++ ")" ++ stmts body ++ "end"
		SET n e -> n ++ " -> " ++ show e
		GET a b -> a ++ "." ++ b
		RETURN e -> "return" ++ show e
		FOR c ss -> "for" ++ show c ++ stmts ss ++ "end"
		REPEAT ss c -> "repeat" ++ stmts ss ++ "until" ++ show c
		WHILE c ss -> "while" ++ show c ++ "do" ++ stmts ss ++ "end"

--------------------------------------------------
-- Lua Parsing Data Declarations
--------------------------------------------------
data OneOrTwo a = Two a a | One a
data LRead = EOF | SEP | V LVal deriving (Eq, Ord, Show)
data LVal = LSym String | LTable [(Lval,Lval)] deriving (Eq, Ord)

instance Show LVal where
	show (LSym s) = s
	show (LTable es) = "(" ++ concat(map showpair es) ++ ")" where
		showpair(k,v) = " " ++ show k ++ "=" ++ show v ++ " "

--------------------------------------------------
-- Parsing
--------------------------------------------------
symChars = "-+_" ++ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']
wsChars = " \n\t"
lreadSym soFar stream = case (soFar,stream) of
	(sym,[]) -> (V $ LSym $ reverse sym, [])
	(sym,c:cs) -> if c `elem` symChars then lreadSym (c:sym) cs
		else (V $ LSym $ reverse sym, c:cs)

readSeq end stream = r [] stream where
	r acc [] = error "Unterminated Sequence"
	r acc (c:cs) =
		if end == c then (reverse acc,cs) else
		if c `elem` wsChars then r acc cs else
		case lread (c:cs) of (e,remain) -> r (e:acc) remain

data ChkState = OrdEs Int | NamedEs
lreadTbl s = (case readSeq ')' s of (es,s')->(V$LTable$p es,s')) where
	p = map fix . checkTbl . unSep
	fix (V a,V b) = (a,b)
	fix _ = error "wut" -- TODO HACK
	unSep l = finalize $ foldl r (False,[]) l where
		r (True,One k:acc) v = (False,Two k v:acc)
		r (True,_) _ = error "misplaced ="
		r (False,acc) SEP = (True,acc)
		r (False,acc) e = (False,One e:acc)
		finalize (True,_) = error "misplaced ="
		finalize (False,l) = reverse l
	checkTbl l = (\(m,_)->m) $ foldl f ([],OrdEs 0) l where
		f (m,OrdEs i) (One e) = (insert (V$LSym$show i) e m, OrdEs(i+1))
		f (m,OrdEs i) (Two k v) = f (m,NamedEs) (Two k v)
		f (m,NamedEs) (One e) = error "Ordered items cannot come after named ones."
		f (m,NamedEs) (Two k v) = (insert k v m, NamedEs)
		insert k v m = M.insertWithKey collide k v m
		collide k _ _ = error("Table has multiple defintions for key: " ++ show k)

lread stream = case stream of
	[] -> (EOF,[])
	'=':cs -> (SEP,cs)
	'(':cs -> lreadTbl cs
	c:cs ->
		if c `elem` wsChars then lread cs else
		if c `elem` symChars then lreadSym [c] cs else
		error $ "unexpected '" ++ [c] ++ "'"

--------------------------------------------------
-- Main Loop
--------------------------------------------------
main = interact (\s -> rpl s ++ "\n") where
	rpl' s = rpl s ++ "\n"
	rpl s = case lread s of
		(EOF,[]) -> ""
		(SEP,_) -> error "Unexpected ="
		(V e,s') -> show e ++ rpl s'
