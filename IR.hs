-- TODO Use an obfuscated name instead of "t" for the temp variable.
-- TODO Make sure variable names don't clash with our temp variable.
-- TODO Make all of our functions take tables and return tables.
-- TODO support for pattern matching.
-- TODO support for building new lexical scopes.
-- TODO support defining new global variables.
-- TODO Support ‘and’ and ‘or’.
-- TODO Enforce valid Lua variable names for VAR values.
-- TODO Tail-call-optimization is not supported.
--  Lua demands tail calls be of the form "return exp".
--  Our compiler produces code in the form "t=exp; return t";
--  This requires a rewrite with a more complicated approach.
--  Or maybe a simple optimization pass on the Lua.Block
--   data structure after we generate it?

module IR where
import Ty
import Sexp
import qualified Lua as L
import Data.List
import System.IO

data Exp
	= IRPrim Prim
	| VAR String
	| CALL IR.Exp [IR.Exp]
	| Λ [String] IR.Exp
	| DO [IR.Exp]
	| TBL [(Prim,IR.Exp)]
	| ASSIGN String IR.Exp
	| GET IR.Exp IR.Exp
	| SET IR.Exp IR.Exp IR.Exp
	| IF IR.Exp IR.Exp IR.Exp
	deriving (Read,Show)

tmp = L.Var "tmp"
ret stmts = L.BLOCK (L.LOCAL tmp : stmts) $ Just $ L.RETURN $ L.VAR tmp
mkblock e = ret [mkstmt e]
wrap s = L.CALLEXP $ L.FnCall fn [] where fn = L.Λ [] $ ret [s]
wraps ss = L.CALLEXP $ L.FnCall fn [] where fn = L.Λ [] $ ret ss
instance CodeGen IR.Exp where cgen ir = cgen(mkstmt ir)
mkstmt e = case e of
	CALL e args -> L.CALLSTMT $ L.FnCall (mkexp e) (map mkexp args)
	e -> L.ASSIGN tmp $ mkexp e

mkexp e = case e of
	IRPrim x -> L.LPrim x
	IR.VAR s -> L.VAR $ L.Var s
	IR.ASSIGN s e -> wraps [L.ASSIGN(L.Var s)(mkexp e),L.ASSIGN tmp (L.VAR$L.Var s)]
	IR.CALL e args -> L.CALLEXP $ L.FnCall (mkexp e) (map mkexp args)
	IR.Λ args body -> L.Λ args $ mkblock body
	IR.DO exps -> wrap $ L.DO $ ret $ map mkstmt exps
	IR.TBL t -> L.TABLE $ map f t where f(a,b)=(mkexp(IRPrim a),mkexp b)
	IR.GET t k -> L.DOT (mkexp t) (mkexp k)
	IR.SET t k v -> wrap $ L.ASSIGN (L.TVar(mkexp t)(mkexp k)) (mkexp v)
	IR.IF c a b -> wrap $ L.IF (mkexp c) (mkblock a) (mkblock b)

three = IRPrim(NUM 3)
four = IRPrim(NUM 4)
input = "(print (((λ(x) (λ(y) x)) 3) 4))"
dataexp = (CALL (IR.VAR "print") [CALL (CALL (IR.Λ ["x"] (IR.Λ ["y"] (IR.VAR "x"))) [three]) [four]])
output = "(print)((((function(v_x)local t;t=(function(v_y)local t;t=(v_x);return((t))end);return((t))end)((3.0)))((4.0))))"

maybeRead r = case Prelude.reads r of {[(a,_)]->Just a; _->Nothing}
main = getLine >>= putStrLn . cgen . fromSexp . Sexp.read1 >> hFlush stdout >> main

-- Prim = T F NIL STR NUM
-- T = Prim Tbl
-- Exp = IRPrim VAR CALL Λ DO TBL ASSIGN GET SET IF

fromSexp :: T -> Exp
fromSexp (Prim (STR s)) = VAR s
fromSexp (Prim x) = IRPrim x
fromSexp (Tbl t) = case Sexp.arrayNotArray t of
	(_,(_:_)) -> error "Functions can only take ordered arguments."
	(fn:args,[]) -> (CALL (fromSexp fn) (map fromSexp args))
	([],[]) -> error "Can't call empty list."
