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
	cg (NUM d) = atom(show d)

instance ToCodeGen Var where
	cg (Var s) = atom s
	cg (TVar t k) = jux (cg t) (tuple ("[","]") [cg k])

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
	cg (DO b) = block ("do","end") (simplifyBlock b)
	cg (ASSIGN v e) = binop (cg v) "=" (cg e)
	cg (LOCAL v) = stmt "local" (cg v)
	cg (CALLSTMT f) = cg f
	-- TODO FIXME HACK HACK HACK
	cg (IF c a b) = hack["if", g c, blk "\nthen" a, blk "\nelse" b,"\nend"] where
		hack ss = (Unsafe,ATOM$concat$intersperse " "$ss)
		g x = gen(cg x)
		blk w c = w ++ " " ++ (concat $ intersperse ";" $ map gen $ simplifyBlock c)

brak a = tuple ("[","]") [a]
instance ToCodeGen Exp where
	cg (LPrim p) = cg p
	cg (CALLEXP f) = cg f
	cg (VAR v) = cg v
	cg (Λ as b) = (block ("function"++args,"end") (simplifyBlock b)) where
		args = gen $ tuple ("(",")") $ map atom as
	cg (DOT a b) = jux (cg a) (brak$cg b)
	cg (TABLE forms) = (\x->(Unsafe,x)) $ TUPLE ("{","}") $ map unpair forms where
		unpair (a,b) = binop (brak$cg a) "=" (cg b)
