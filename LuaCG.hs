module LuaCG(luaCG) where
import IRs
import StrSexp
import Util
import Data.List
import Repl

luaCG x = cg x
class ToCG a where { cg::a -> CExp }
instance ToCG Atom where
	cg T = atom("true")
	cg F = atom("false")
	cg NIL = atom("nil")
	cg (STR s) = atom(show s)
	cg (NUM d) = atom(writeNum d)

instance ToCG LVar where
	cg (LTVar t k) = jux (cg t) (tuple ("[","]") [cg k])
	cg LTMP = atom "_"
	cg (LVar s) = atom(validateID s)

cgcall(f,es) = jux (cg f) (tuple ("(",")") $ map cg es)
instance ToCG LStmt where
	cg (LDO b) = block("do","end") (map cg b)
	cg (LASSIGN v e) = binop (cg v) "=" (cg e)
	cg (LLOCAL v) = stmt "local" (cg v)
	cg (LCALLSTMT f) = cgcall f
	cg (LBREAK) = atom "break"
	cg (LCONTINUE) = atom "continue"
	cg (LRETURN x) = stmt "return" (cg x)
	cg (LIF c a b) = block ("if","end") [cg c,br "then" a, br "else" b] where
		br s b = block(s,"") $ map cg b

brak a = tuple ("[","]") [a]
instance ToCG LExp where
	cg (LPRIM p) = cg p
	cg (LCALLEXP f) = cgcall f
	cg (LVAR v) = cg v
	cg (LΛ as b) = (blockexp("function"++args,"end") (map cg b)) where
		args = gen $ tuple ("(",")") $ map (atom.validateID) as
	cg (LDOT a b) = jux (cg a) (brak$cg b)
	cg (LTABLE forms) = (\x->(CExp Unsafe x)) $ CTUPLE ("{","}") $ map unpair forms where
		unpair (a,b) = binop (brak$cg a) "=" (cg b)

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
	kw = s `elem` keywords
	leadingDigit = first `elem` digits
	okChars = all (`elem` luaid) s
	letters = ['a'..'z'] ++ ['A'..'Z']
	digits = ['0'..'9']
	luaid = "_" ++ letters ++ digits
	tmp = s == "_"
