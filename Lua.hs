module Lua(luaCG) where
import Prim
import IR
import Read
import Util
import Data.List
import Repl
import Prelude.Unicode

luaCG ∷ LStmt → CExp
luaCG x = cg x

class ToCG a where { cg∷a → CExp }
instance ToCG Atom where
	cg T = atom("true")
	cg F = atom("false")
	cg (STR s) = atom(show s)
	cg (NUM d) = atom(writeNum d)

ret s = block ("do","end") [stmt "return" s]

(brak,paren) = (tuple("[","]"), tuple("(",")"))
instance ToCG LStmt where
	cg (LDO b) = block("do","end") (map cg b)
	cg (LBIND v) = stmt "local" $ atom(var v)
	cg (LASSIGN v e) = CExp Space $ CBINOP (atom(var v)) "=" (cg e)
	cg (LRETURN x) = ret $ cg x
	cg (LSET o k v) = binop (jux (atom$var o) $ brak [atom$var k]) "=" (atom$var v)
	cg (LFOREIGN_DIRECTIVE d) = atom d
	cg (LIF c a b) = block ("if","end") [atom$var c,br "then" a, br "else" b] where
		br s b = block(s,"") $ [cg b]

instance ToCG LExp where
	cg (LATOM p) = cg p
	cg (LVAR v) = atom(var v)
	cg (LNEWTABLE) = atom "{}"
	cg (LGET o k) = jux (atom$var o) $ brak [atom$var k]
	cg (LCALL f a) = jux (atom$var f) $ paren [atom$var a]
	cg (Lλ a s) = (blockexp("function("++var a++")","end") [cg s])
	cg (LGLOBAL s) = atom $ validateID s
	cg (LFOREIGN_CALL f args) = jux (atom$var f) $ paren $ map (atom.var) args
	cg (LFOREIGN_METHOD obj meth args) =
		binop (atom$var obj) ":" $ jux (atom meth) $ paren $ map (atom.var) args
	cg (LTABLE forms) = (\x→(CExp Unsafe x)) $ CTUPLE ("{","}") $ map unpair (toList forms) where
		unpair (a,b) = binop (brak[cg a]) "=" (atom$var b)

keywords =
	[ "and", "break", "do", "else", "elseif", "end", "false", "for", "function"
	, "if","in", "local", "not", "or", "repeat", "return", "then", "true"
	, "until", "while" ]

tokens =
	[ "+", "-", "*", "/", "%", "^", "#", "==", "~=", "<=", ">=", "<", ">", "="
	, "(", ")", "{", "}", "[", "]", ";", ":",  ",",  ".",  "..", "..." ]

validateID id = if validID id then id else error s where
	s = "'" ++ id ++ "' is not a valid Lua identifier."

var a = "v" ++ show a
validID [] = False
validID s@(first:rest) =
	and[not tmp, not kw, not leadingDigit, okChars, not cgvar] where
		cgvar = and['v'≡first, not$null rest, all(`elem` digits) rest]
		kw = or [s `elem` keywords, s `elem` tokens]
		leadingDigit = first `elem` digits
		okChars = all (`elem` luaid) s
		letters = ['a'..'z'] ++ ['A'..'Z']
		digits = ['0'..'9']
		luaid = "_" ++ letters ++ digits
		tmp = s ≡ "_"
