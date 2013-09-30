module JSCG(jsCG) where
import IRs
import StrSexp
import Util
import Data.List
import Repl

jsCG x = cg x
class ToCodeGen a where { cg::a -> CExp }
(brak,paren) = (tuple("[","]"), tuple("(",")"))
instance ToCodeGen Atom where
	cg T = atom("true")
	cg F = atom("false")
	cg NIL = atom("null")
	cg (STR s) = atom(show s)
	cg (NUM d) = atom(writeNum d)

instance ToCodeGen JVar where
	cg (JTVar t k) = jux (cg t) (brak[cg k])
	cg JTMP = atom "_"
	cg (JVar s) = atom(validateID s)

instance ToCodeGen JStmt where
	cg (JLOCAL v) = semi $ stmt "var" (cg v)
	cg (JEXP x) = semi $ delim(cg x)
	cg (JRETURN x) = semi $ stmt "return" (cg x)
	cg (JBREAK) = semi $ atom "break"
	cg (JCONTINUE) = semi $ atom "continue"

instance ToCodeGen JExp where
	cg (JPrim p) = cg p
	cg (JCALL(f,es)) = jux (cg f) (tuple ("(",")") $ map cg es)
	cg (JVAR v) = cg v
	cg (JΛ as b) = (blockexp("function"++args++"{","}") (map cg b)) where
		args = gen $ paren $ map (atom.validateID) as
	cg (JDOT a b) = jux (cg a) (brak[cg b])
	cg (JTABLE forms) = tbl where
		tbl = (\x->(CExp Unsafe x)) $ CTUPLE ("{","}") $ map unpair forms
		unpair (a,b) = binop (brak[cg a]) ":" (cg b)
	cg (JIF a b c) = triop (cg a) "?" (cg b) ":" (cg c)
	cg (JASSIGN a b) = binop (cg a) "=" (cg b)

keywords =
	[ "break", "case", "catch", "continue", "debugger", "default", "delete"
	, "do", "else", "finally", "for", "function", "if", "in", "instanceof"
	, "new", "return", "switch", "this", "throw", "try", "typeof", "var"
	, "void", "while", "with", "class", "enum", "export", "extends"
	, "import", "super", "implements", "interface", "let", "package"
	, "private", "protected", "public", "static", "yield" ]

operators =
	[ "^", "^=", "~", "<", "<<", "<<=", "<=", "=", "==", ">", ">=", ">>"
	, ">>=", ">>>", ">>>=", "|", "|=", "||", "-", "-=", "--", ",", ":", "!"
	, "!=", "?", "/", "/=", "*", "*=", "&", "&=", "&&", "%", "%=", "+", "+="
	, "++", "delete", "new", "this", "typeof", "void" ]

validateID id = if validID id then id else error s where
	s = "'" ++ id ++ "' is not a valid Javascript identifier."

validID [] = False
validID s@(first:rest) = and [not tmp, not kw, not leadingDigit, okChars] where
	kw = s `elem` keywords
	leadingDigit = first `elem` digits
	okChars = all (`elem` luaid) s
	letters = ['a'..'z'] ++ ['A'..'Z']
	digits = ['0'..'9']
	luaid = "_" ++ letters ++ digits
	tmp = s == "_"
