-- TODO Output is ugly and probably still very invalid.
-- TODO Some things are oversimplified
	-- Variables (Var)
	-- Argument list syntax
	-- Function call syntax.
	-- Function definition syntax.

module Lua where
import Data.List

data Var = Var String | TVar Exp Exp
data FnCall = FnCall Exp [Exp] -- TODO
data Block = BLOCK [Stmt] (Maybe BlockEnd)
data BlockEnd = RETURN Exp | BREAK | CONTINUE
data Exp
	= NIL | FALSE | TRUE
	| STRING String | NUMBER Double
	| CALLEXP FnCall
	| VAR Var
	| LAMBDA [String] Block
	| DOT Exp Exp
	| TABLE [(Exp,Exp)]

data Stmt
	= DO Block
	| ASSIGN [(Var,Exp)]
	| LOCAL [Var]
	| IF Exp Block Block
	| CALLSTMT FnCall

comas strs = "(":intersperse "," strs ++ [")"]

instance Show Var where
	show (Var s) = s
	show (TVar t k) = concat [show t, "[", show k, "]"]

instance Show FnCall where
	show (FnCall f es) = show f ++ (concat $ comas $ map show es)

instance Show Block where
	show (BLOCK b e) = concat $ intersperse ";" $ map show b ++ end where
		end = case e of
			Nothing -> []
			Just BREAK -> ["break"]
			Just CONTINUE -> ["continue"]
			Just(RETURN e) -> ["return" ++ (concat $ comas $ map show e)]

instance Show Stmt where
	show s = concat $ intersperse " " $ case s of
		DO b -> ["do",show b,"end"]
		ASSIGN ments -> [concat $ intersperse ";" $ map assign ments] where
			assign(v,e) = show v ++ "=" ++ show e
		LOCAL vars -> "local" : intersperse "," (map show vars)
		IF e b1 b2 -> ["if", show e, "then", show b1, "else", show b2, "end"]
		CALLSTMT fncall -> [show fncall]

wrap l = ["("] ++ l ++ [")"]

instance Show Exp where
	show e = concat $ wrap $ intersperse "" $ case e of
		NIL -> ["nil"]
		FALSE -> ["false"]
		TRUE -> ["true"]
		STRING s -> ["\"" ++ s ++ "\""]
		NUMBER n -> [show n]
		VAR(v) -> [show v]
		CALLEXP(FnCall f as) -> show f:comas(map show as)
		LAMBDA as b -> ["function"] ++ comas as ++ [show b,"end"]
		DOT t k -> [show t, "[", show k, "]"]
		TABLE exps -> ["{"] ++ intersperse ", " (map r exps) ++ ["}"] where
			r(k,v) = "[" ++ show k ++ "] = " ++ show v
