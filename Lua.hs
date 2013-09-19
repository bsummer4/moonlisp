-- The file defines a representation of a subset of the Lua AST.
--  This is used for compiling to and generating Lua code.
-- TODO Code generation is ugly and probably not completely correct.

module Lua where
import Data.List
import Ty
import Sexp
import System.IO

data Var = Var String | TVar Exp Exp deriving (Show,Read)
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

comas strs = "(":intersperse "," strs ++ [")"]
instance CodeGen Var where
	cgen (Var s) = s
	cgen (TVar t k) = concat [cgen t, "[", cgen k, "]"]

instance CodeGen FnCall where
	cgen (FnCall f es) = cgen f ++ (concat $ comas $ map cgen es)

instance CodeGen Block where
	cgen (BLOCK b e) = concat $ intersperse ";" $ map cgen b ++ end where
		end = case e of
			Nothing -> []
			Just BREAK -> ["break"]
			Just CONTINUE -> ["continue"]
			Just(RETURN e) -> ["return" ++ "(" ++ cgen e ++ ")"]

instance CodeGen Stmt where
	cgen s = concat $ intersperse " " $ case s of
		DO b -> ["do",cgen b,"end"]
		ASSIGN v e -> [cgen v ++ "=" ++ cgen e]
		LOCAL v -> ["local " ++ cgen v]
		IF e b1 b2 -> ["if", cgen e, "then", cgen b1, "else", cgen b2, "end"]
		CALLSTMT fncall -> [cgen fncall]

instance CodeGen Exp where
	cgen e = concat $ (\l->["("]++l++[")"]) $ intersperse "" $ case e of
		LPrim T -> ["true"]
		LPrim F -> ["false"]
		LPrim NIL -> ["nil"]
		LPrim(STR s) -> ["\"" ++ s ++ "\""]
		LPrim(NUM n) -> [show n]
		VAR(v) -> [cgen v]
		CALLEXP(FnCall f as) -> cgen f:comas(map cgen as)
		Λ as b -> ["function"] ++ comas as ++ [cgen b,"end"]
		DOT t k -> [cgen t, "[", cgen k, "]"]
		TABLE exps -> ["{"] ++ intersperse ", " (map r exps) ++ ["}"] where
			r(k,v) = "[" ++ cgen k ++ "] = " ++ cgen v

maybeRead r = case Prelude.reads r of {[(a,_)]->Just a; _->Nothing}
main = do
	l <- getLine
	case fmap cgen (maybeRead l :: Maybe Stmt) of
		Nothing -> putStrLn ""
		Just code -> putStrLn code
	hFlush stdout >> main
