module LuaCodeGen where
import Lua
import Sexp
import CodeGen as CG
import Data.List
import Repl

class ToCodeGen a where { cg::a -> Code }
instance ToCodeGen Prim where
	cg T = atom("true")
	cg F = atom("false")
	cg NIL = atom("nil")
	cg (STR s) = atom(show s)
	cg (NUM d) = atom(writeNum d)

instance ToCodeGen Var where
	cg (TVar t k) = jux (cg t) (tuple ("[","]") [cg k])
	cg TMP = atom "_"
	cg (Var s) = atom(validateID s)

instance ToCodeGen FnCall where
	cg (FnCall f es) = jux (cg f) (tuple ("(",")") $ map cg es)

simplifyBlock (Lua.BLOCK code tail) = map cg code ++ fix tail where
	fix Nothing = []
	fix (Just b) = [cg b]

instance ToCodeGen BlockEnd where
	cg (RETURN x) = stmt "return" (cg x)
	cg BREAK = atom "break"
	cg CONTINUE = atom "continue"

instance ToCodeGen Stmt where
	cg (DO b) = block("do","end") (simplifyBlock b)
	cg (ASSIGN v e) = binop (cg v) "=" (cg e)
	cg (LOCAL v) = stmt "local" (cg v)
	cg (CALLSTMT f) = cg f
	cg (IF c a b) = block ("if","end") [cg c,br "then" a, br "else" b] where
		br s b = block(s,"") $ simplifyBlock b

brak a = tuple ("[","]") [a]
instance ToCodeGen Exp where
	cg (LPrim p) = cg p
	cg (CALLEXP f) = cg f
	cg (VAR v) = cg v
	cg (Λ as b) = (blockexp("function"++args,"end") (simplifyBlock b)) where
		args = gen $ tuple ("(",")") $ map (atom.validateID) as
	cg (DOT a b) = jux (cg a) (brak$cg b)
	cg (TABLE forms) = (\x->(Unsafe,x)) $ TUPLE ("{","}") $ map unpair forms where
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
