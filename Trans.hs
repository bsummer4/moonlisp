module Trans(makeImplicitReturnsExplicit, compileToLIR) where
import Prim
import IR
import Read
import Util
import Data.List
import Data.Maybe
import System.IO

compileToLIR :: Exp -> LStmt
compileToLIR e = mkstmt toplevelNS e

-- This makes sure that all function blocks end with a return statement, if they
-- don't, all the tail-call positions are wrapped with return statements.
makeImplicitReturnsExplicit :: Exp -> Exp
makeImplicitReturnsExplicit p = r p where
	r (SYNTAX _) = error "bad AST designer"
	r (Λ args body) = Λ args (fix body)
	r (CALL f a) = CALL (r f) (r a)
	r (DO es) = DO (map r es)
	r (DATA forms) = DATA(tmap r forms)
	r (MATCH e ps) = MATCH (r e) $ map pat ps where pat(p,e)=(p,r e)
	r (RETURN a) = RETURN $ r a
	r (SET a b c) = SET (r a) (r b) (r c)
	r (GET o a) = GET (r o) (r a)
	r (FMETHOD o m as) = FMETHOD (r o) m (map r as)
	r (FCALL f as) = FCALL (r f) (map r as)
	r e@(ATOM _) = e
	r e@(VAR _) = e
	r e@(GLOBAL _) = e
	r e@(FSTMT _) = e
	fix e@(DO es) = case reverse es of
		last:before -> DO $ reverse(fix last:before)
		[] -> e
	fix (MATCH e ps) = MATCH (r e) $ map pat ps where pat(p,e)=(p,fix e)
	fix e@(RETURN _) = e
	fix e@(FSTMT _) = e
	fix e = RETURN(r e)

-- This is a list of all of the identifiers in the standard library. Their order
-- in this list is also used to refer to them by integers in the LIR
-- representation.
builtins :: [String]
builtins = (
	[ "#n", ".", "!", "apply", "keys", "%", "append", "read", "write", "+"
	, "-", "*", "/", "%", "^", "=", "~=", "<=", ">=", "<", ">", ".."
	, "bool?", "func?", "str?", "num?", "foreign?", "table?"
	, "eq", "neq"
	])

mkstmt :: Namespace -> Exp -> LStmt
mkstmt ns e = mkdo code where (_,_,code)=compile ns e
mkdo [s] = s
mkdo ss = LDO ss

-- We use this list of strings to map between identifiers and their ids as we
-- walk the Exp tree.
data Namespace = NS Int [String]
nsFromList l = NS (length l) l
toplevelNS = nsFromList builtins
wSym (NS i ns) s = (i+1, NS(i+1)(s:ns))
findSym (NS l ns) name = l - idx where
	idx = case elemIndex name ns of {Just a->a; Nothing->error undef}
	undef = "Undefined variable: " ++ name

call :: Namespace -> Exp -> Exp -> (Namespace, Int, [LStmt])
call ns f a = (ns3,r,stmts) where
	(ns1,fId,fCode) = compile ns f
	(ns2,aId,aCode) = compile ns1 a
	(r,ns3) = wSym ns2 ""
	stmts = fCode ++ aCode ++ [LBIND r, LASSIGN r$LCALL fId aId]

doThese :: Namespace -> [Exp] -> (Namespace, [Int], [LStmt])
doThese ns exps = (ns,[],[]) -- TODO we need a tfoldl sorta like tmap.

mkSeq :: (Namespace, Int, [LStmt]) -> Exp -> (Namespace, Int, [LStmt])
mkSeq (ns,i,code) e = (ns',j,code++code') where (ns',j,code')=compile ns e

getSeq :: Namespace -> [Exp] -> (Namespace,[Int],[LStmt])
getSeq ns es = foldl r (ns,[],[]) es where
	r :: (Namespace,[Int],[LStmt]) -> Exp -> (Namespace,[Int],[LStmt])
	r (ns,results,code) e = (ns',results++[j],code++code')
		where (ns',j,code')=compile ns e

mktable :: Namespace -> Tbl Exp -> (Namespace, Int, [LStmt])
mktable ns t = (nsF,r,codeF) where
	(r,ns') = wSym ns ""
	setup = [LBIND r, LASSIGN r LNEWTABLE]
	ltbl = toList t
	(nsF,codeF) = foldl f (ns',setup) $ toList t
	f (ns1,code) (k,exp) = (ns3, code++cV++cK++[LSET r iK iV]) where
		(ns2,iV,cV) = compile ns1 exp
		(ns3,iK,cK) = compile ns2 $ ATOM k

-- TODO The resulting code will not be properly tail-recursive.
--  The RETURN clause generates code and then uses a var as it's argument.
--  Instead we need to get the expression that's usually assigned to that var
--  and give that to LRETURN.
--  I haven't worked out yet how to go about that.
compile :: Namespace -> Exp -> (Namespace, Int, [LStmt])
compile ns e = case e of
	VAR s -> (ns,findSym ns s,[])
	ATOM a -> (ns',i,[LBIND i,LASSIGN i$LATOM a]) where (i,ns')=wSym ns ""
	CALL f a -> call ns f a
	DO code -> foldl mkSeq (ns,0,[]) code
	RETURN e -> (ns',v,code ++ [LRETURN$LVAR v]) where (ns',v,code)=compile ns e
	MATCH e pats -> error "TODO"
	DATA t -> mktable ns t
	GLOBAL s -> (ns',i,[LBIND i, LASSIGN i $ LGLOBAL s]) where (i,ns')=wSym ns ""
	FSTMT s -> (ns,0,[LFOREIGN_DIRECTIVE s])
	Λ s e -> (ns',result,[LBIND result, LASSIGN result $ Lλ param$mkdo lexp]) where
		(result,ns') = wSym ns ""
		(param,λns) = wSym ns s
		(_,_,lexp) = compile λns e

	SYNTAX e -> error noMacros where
		noMacros = "Internal error. All macros must be expanded before compilation."

	SET a b c -> case getSeq ns [a,b,c] of
		(ns2,[ai,bi,ci],code) -> (ns2,ci,code++[LSET ai bi ci])
		_ -> error "This will never happen"

	GET a b -> case getSeq ns [a,b] of
		(ns2,[ai,bi],code) -> (ns3,r,code++lget) where
			(r,ns3) = wSym ns2 ""
			lget = [LBIND r, LASSIGN r $ LGET ai bi]
		_ -> error "This will never happen"

	FMETHOD o m as -> case getSeq ns (o:as) of
		(ns2,(oi:ais),code) -> (ns3,r,code++fmethod) where
			(r,ns3) = wSym ns2 ""
			fmethod = [LBIND r, LASSIGN r $ LFOREIGN_METHOD oi m ais]
		_ -> error "This will never happen"

	FCALL f as -> case getSeq ns (f:as) of
		(ns2,(fi:ais),code) -> (ns3,r,code++fcall) where
			(r,ns3) = wSym ns2 ""
			fcall = [LBIND r, LASSIGN r $ LFOREIGN_CALL fi ais]
		_ -> error "This will never happen"
