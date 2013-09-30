-- The file defines a representation of a subset of the Lua AST.
--  This is used for compiling to and generating Lua code.

module Lua where
import Sexp
data Var = TMP | Var String | TVar Exp Exp deriving (Show,Read)
data FnCall = FnCall Exp [Exp] deriving (Show,Read)
data Block = BLOCK [Stmt] (Maybe BlockEnd) deriving (Show,Read)
data BlockEnd = RETURN Exp | BREAK | CONTINUE deriving (Show,Read)
data Exp
	= LPrim Prim
	| CALLEXP FnCall
	| VAR Var
	| Λ [String] Block
	| DOT Exp Exp
	| TABLE [(Exp,Exp)]
	deriving (Show,Read)

data Stmt
	= DO Block
	| ASSIGN Var Exp
	| LOCAL Var
	| IF Exp Block Block
	| CALLSTMT FnCall
	deriving (Show,Read)
