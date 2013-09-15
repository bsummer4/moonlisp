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
import qualified Lua as L

data Prim = NIL | FALSE | TRUE | STRING String | NUMBER Double
data Exp
	= PRIM Prim
	| VAR String
	| CALL Exp [Exp]
	| LAMBDA [String] Exp
	| DO [Exp]
	| TBL [(Prim,Exp)]
	| ASSIGN String Exp
	| GET Exp Exp
	| SET Exp Exp Exp
	| IF Exp Exp Exp

t = L.Var "t"
ret stmts = L.BLOCK (L.LOCAL[t] : stmts) $ Just $ L.RETURN [L.VAR t]
mkblock e = ret [mkstmt e]
wrap s = L.CALLEXP $ L.FnCall fn [] where fn = L.LAMBDA [] $ ret [s]
wraps ss = L.CALLEXP $ L.FnCall fn [] where fn = L.LAMBDA [] $ ret ss
mkstmt e = case e of
	CALL e args -> L.CALLSTMT $ L.FnCall (mkexp e) (map mkexp args)
	e -> L.ASSIGN [(t,mkexp e)]

mkexp e = case e of
	PRIM(NIL) -> L.NIL
	PRIM(FALSE) -> L.FALSE
	PRIM(TRUE) -> L.TRUE
	PRIM(STRING s) -> L.STRING s
	PRIM(NUMBER n) -> L.NUMBER n
	VAR s -> L.VAR $ L.Var s
	ASSIGN s e -> wraps [L.ASSIGN [(L.Var s,mkexp e)],L.ASSIGN [(t,L.VAR$L.Var s)]]
	CALL e args -> L.CALLEXP $ L.FnCall (mkexp e) (map mkexp args)
	LAMBDA args body -> L.LAMBDA args $ mkblock body
	DO exps -> wrap $ L.DO $ ret $ map mkstmt exps
	TBL t -> L.TABLE $ map f t where f(a,b)=(mkexp(PRIM a),mkexp b)
	GET t k -> L.DOT (mkexp t) (mkexp k)
	SET t k v -> wrap $ L.ASSIGN [(L.TVar (mkexp t) (mkexp k), mkexp v)]
	IF c a b -> wrap $ L.IF (mkexp c) (mkblock a) (mkblock b)

three = PRIM(NUMBER 3)
four = PRIM(NUMBER 4)
input = "(print (((λ(x) (λ(y) x)) 3) 4))"
dataexp = (CALL (VAR "print") [CALL (CALL (LAMBDA ["x"] (LAMBDA ["y"] (VAR "x"))) [three]) [four]])
output = "(print)((((function(v_x)local t;t=(function(v_y)local t;t=(v_x);return((t))end);return((t))end)((3.0)))((4.0))))"
