module Lua(luaCG) where
import Prim
import IR
import Read
import Util
import Data.List
import Repl

var a = "v" ++ show a
luaCG x = cg x
class ToCG a where { cg::a -> CExp }
instance ToCG Atom where
	cg T = atom("true")
	cg F = atom("false")
	cg NIL = atom("nil")
	cg (STR s) = atom(var s)
	cg (NUM d) = atom(writeNum d)

(brak,paren) = (tuple("[","]"), tuple("(",")"))
instance ToCG LStmt where
	cg (LDO b) = block("do","end") (map cg b)
	cg (LSET o k v) = binop (cg $ LDOT o k) "=" (cg v)
	cg (LLET v e) = stmt "local" $ (CExp Space $ CBINOP (atom(var v)) "=" (cg e))
	cg (LRETURN x) = stmt "return" (cg x)
	cg (LIF c a b) = block ("if","end") [cg c,br "then" a, br "else" b] where
		br s b = block(s,"") $ [cg b]

instance ToCG LExp where
	cg (LATOM p) = cg p
	cg (LVAR v) = atom(var v)
	cg (LCALL f a) = jux (cg f) $ paren [cg a]
	cg (Lλ a s) = (blockexp("function("++var a++")","end") [cg s])
	cg (LDOT a b) = jux (cg a) (brak[cg b])
	cg (LEQ a b) = binop (cg a) "==" (cg b)
	cg (LTABLE forms) = (\x->(CExp Unsafe x)) $ CTUPLE ("{","}") $ map unpair (toList forms) where
		unpair (a,b) = binop (brak[cg a]) "=" (cg b)

keywords =
	[ "and", "break", "do", "else", "elseif", "end", "false", "for", "function"
	, "if","in", "local", "nil", "not", "or", "repeat", "return", "then", "true"
	, "until", "while" ]

tokens =
	[ "+", "-", "*", "/", "%", "^", "#", "==", "~=", "<=", ">=", "<", ">", "="
	, "(", ")", "{", "}", "[", "]", ";", ":",  ",",  ".",  "..", "..." ]

validateID id = if validID id then id else error s where
	s = "'" ++ id ++ "' is not a valid Lua identifier."

validID [] = False
validID s@(first:rest) = and [not tmp, not kw, not leadingDigit, okChars] where
	kw = and [s `elem` keywords, s `elem` tokens]
	leadingDigit = first `elem` digits
	okChars = all (`elem` luaid) s
	letters = ['a'..'z'] ++ ['A'..'Z']
	digits = ['0'..'9']
	luaid = "_" ++ letters ++ digits
	tmp = s == "_"
