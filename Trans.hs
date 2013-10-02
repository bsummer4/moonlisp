module Trans(toLStmt) where
import Prim
import IR
import Read
import Util
import Data.List
import System.IO

toLStmt :: Exp -> LStmt
toLStmt e = mkstmt emptyNS e
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

eq :: LExp -> LExp -> LExp
eq a b = LCALL (LVAR $ findSym emptyNS "=") (LTABLE $ mk [a,b] [])

match :: Namespace -> Exp -> [(Pattern,Exp)] -> [LStmt]
match ns e [] = [LLET 0 $ mkexp ns e]
match ns e pats = (LLET swvar $ mkexp ns e):ifs pats where
	(swvar,ns') = wSym ns ""
	ifs :: [(Pattern,Exp)] -> [LStmt]
	ifs [] = []
	ifs ((p,s):more) = case p of
		PSYM v -> LLET casevar (LVAR swvar) : mkstmts ns'' s where
			(casevar,ns'') = wSym ns' v
		PATOM a -> [LIF c (mkstmt ns' s) (mkdo $ ifs more)] where
			c = (eq (LVAR swvar) (LATOM a))
		PTBL _ -> error "Pattern matching is not yet supported."

-- Pattern ::= PSYM PATOM PTBL
-- exp ::= ATOM VAR DO Λ MATCH CALL DATA SYNTAX FFUNC FSTMT
-- lstmt ::= LLET DO IF SET RETURN
-- lexp ::= LATOM LVAR LCALL Lλ LDOT LEQ
-- unstmt s = mkexp $ retimplicit $ IΛ [] s
unstmt :: Namespace -> LStmt -> LExp
-- unstmt ns s = mkexp ns $ retimplicit $ Λ "" s
unstmt ns s = LCALL (Lλ 0 $ s) (LATOM NIL)
mkstmts :: Namespace -> Exp -> [LStmt]
mkstmts ns e = case e of
	MATCH e patterns -> match ns e patterns
	DO es -> concat $ map (mkstmts ns) es
	FFUNC _ -> error "ffi is not implemented."
	FSTMT _ -> error "ffi is not implemented."
	SYNTAX s -> error "toLExp expects to be run after unsyntax."
	RETURN e -> [LRETURN $ mkexp ns e]
	e -> [LLET 0 $ mkexp ns e]

mkexp :: Namespace -> Exp -> LExp
mkexp ns e = case e of
	MATCH e patterns -> LCALL (Lλ 0 $ mkdo $ match ns e patterns) (LATOM NIL)
	ATOM a -> LATOM a
	VAR v -> LVAR (findSym ns v)
	Λ v e -> case wSym ns v of (vi,ns') -> Lλ vi $ mkstmt ns' e
	CALL f a -> LCALL (mkexp ns f) (mkexp ns a)
	DATA d -> LTABLE $ tmap (mkexp ns) d
	s -> unstmt ns $ mkstmt ns e

retimplicit :: Exp -> Exp
retimplicit p = r p where
	r (Λ args body) = Λ args (blk body)
	r (CALL f a) = CALL (r f) (r a)
	r (DO es) = DO (map r es)
	r (DATA forms) = DATA(tmap r forms)
	r (MATCH e ps) = MATCH (r e) $ map pat ps where pat(p,e)=(p,r e)
	r (RETURN a) = RETURN $ r a
	r e@(ATOM _) = e
	r e@(VAR _) = e
	blk e@(DO[]) = e
	blk (MATCH e ps) = MATCH (r e) $ map pat ps where pat(p,e)=(p,blk e)
	blk (DO es) = case reverse es of last:before->DO $ reverse(blk last:before)
	blk e@(RETURN _) = e
	blk e = RETURN(retimplicit e)

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
