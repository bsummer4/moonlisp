module IRs where

-- Atomic Data Types
data Atom = T | F | NIL | STR String | NUM Double deriving(Show,Read,Eq,Ord)
type Tbl a = [(Atom,a)]
data Exp
	= ATOM Atom
	| VAR String
	| DO [Exp]
	| Λ String Exp
	| JS String
	| LUA String
	| PY String
	| CALL(Tbl Exp)
	| DATA(Tbl Exp)
	| MACRO(Tbl Exp)
	deriving(Read,Show,Eq,Ord)
