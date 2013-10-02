module Trans(toLStmt) where
import Prim
import IR
import Read
import Util
import Data.List
import System.IO

toLStmt :: Exp -> LStmt
toLStmt = mkstmt emptyNS
mkstmt :: Namespace -> Exp -> LStmt
mkstmt ns e = mkdo $ mkstmts ns e
mkdo [s] = s
mkdo ss = LDO ss

builtins = (
	[ ".", "!", "apply", "keys", "%", "append", "read", "write", "+"
	, "-", "*", "/", "%", "^", "=", "~=", "<=", ">=", "<", ">", ".."
	, "nil?", "bool?", "func?", "str?", "num?", "foreign?", "table?"
	, "eq", "neq"
	])

data Namespace = NS Int [String]
nsFromList l = NS (length l) l
emptyNS = nsFromList builtins
wSym (NS i ns) s = (i+1, NS(i+1)(s:ns))
findSym (NS l ns) name = l - idx where
	idx = case elemIndex name ns of {Just a->a; Nothing->error undef}
	undef = "Undefined variable: " ++ name

match ns e ps = [LLET 0 $ LATOM $ NUM 0]
-- mklet (i,syms) e = LLET i $ mkexp (i+1,"":syms) e

-- exp ::= ATOM VAR DO Λ MATCH CALL DATA SYNTAX FFUNC FSTMT
-- lstmt ::= LLET DO IF SET RETURN
-- lexp ::= LATOM LVAR LCALL Lλ LDOT LEQ

wrap :: LStmt -> LExp
wrap e = LCALL (Lλ 0 $ e) (LATOM NIL)
mkstmts :: Namespace -> Exp -> [LStmt]
mkstmts ns e = case e of
	DO es -> concat $ map (mkstmts ns) es
	MATCH e patterns -> match ns e patterns
	FFUNC _ -> error "ffi is not implemented."
	FSTMT _ -> error "ffi is not implemented."
	SYNTAX s -> error "toLExp expects to be run after unsyntax."
	RETURN e -> [LRETURN $ mkexp ns e]
	e -> [LLET 0 $ mkexp ns e]

mkexp ns e = case e of
	ATOM a -> LATOM a
	VAR v -> LVAR (findSym ns v)
	Λ v e -> case wSym ns v of (vi,ns') -> Lλ vi $ mkstmt ns' e
	CALL f a -> LCALL (mkexp ns f) (mkexp ns a)
	DATA d -> LTABLE $ tmap (mkexp ns) d
	s -> wrap $ mkstmt ns s

--irLua = mkstmts
--wrap s = LCALLEXP(fn,[]) where fn = LΛ [] [s]
--wraps ss = LCALLEXP(fn,[]) where fn = LΛ [] ss
--mkstmts e = case e of
--	IDO ss -> concat $ map mkstmts ss
--	ICALL e args -> [LCALLSTMT(mkexp e, map mkexp args)]
--	IASSIGN s e -> [LASSIGN (LVar s) (mkexp e)]
--	IRETURN e -> [LRETURN $ mkexp e]
--	ISET t k v -> [LASSIGN (LTVar (mkexp t) (mkexp k)) (mkexp v)]
--	IIF c a b -> [LIF (mkexp c) (mkstmts a) (mkstmts b)]
--	e -> [LASSIGN LTMP (mkexp e)]
--
---- Read carefully! This will not infinite loop because
---- ‘unstmt’ and ‘retimplicit’ will always return a
---- lambda expression in this case. That is fed back into
---- ‘mkexp’ which will never call unstmt when given a
---- lambda expression.
--unstmt s = mkexp $ retimplicit $ IΛ [] s
--mkexp e = case e of
--	PRIM x -> LPRIM x
--	VAR s -> LVAR $ LVar s
--	CALL e args -> LCALLEXP(mkexp e, map mkexp args)
--	Λ args body -> LΛ args $ mkstmts body
--	TBL t -> LTABLE $ map f t where f(a,b)=(mkexp(IPRIM a),mkexp b)
--	RETURN _ -> error "(return _) can't be used as an expression."
--	GET t k -> LDOT (mkexp t) (mkexp k)
--	ASSIGN s e -> unstmt e
--	DO exps -> unstmt e
--	SET t k v -> unstmt e
--	IF c a b -> unstmt e
