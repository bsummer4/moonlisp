-- Compile from the internal representation into the Lua representation.

module IRLua where
import IRs
import StrSexp
import Util
import LuaCodeGen
import Data.List
import System.IO
import Repl

wrap s = LCALLEXP(fn,[]) where fn = LΛ [] [s]
wraps ss = LCALLEXP(fn,[]) where fn = LΛ [] ss
mkstmts e = case e of
	IDO ss -> concat $ map mkstmts ss
	ICALL e args -> [LCALLSTMT(mkexp e, map mkexp args)]
	IASSIGN s e -> [LASSIGN (LVar s) (mkexp e)]
	IRETURN e -> [LRETURN $ mkexp e]
	ISET t k v -> [LASSIGN (LTVar (mkexp t) (mkexp k)) (mkexp v)]
	IIF c a b -> [LIF (mkexp c) (mkstmts a) (mkstmts b)]
	e -> [LASSIGN LTMP (mkexp e)]

-- Read carefully! This will not infinite loop because
-- ‘unstmt’ and ‘retimplicit’ will always return a
-- lambda expression in this case. That is fed back into
-- ‘mkexp’ which will never call unstmt when given a
-- lambda expression.
unstmt s = mkexp $ retimplicit $ IΛ [] s
mkexp e = case e of
	IPrim x -> LPrim x
	IVAR s -> LVAR $ LVar s
	ICALL e args -> LCALLEXP(mkexp e, map mkexp args)
	IΛ args body -> LΛ args $ mkstmts body
	ITBL t -> LTABLE $ map f t where f(a,b)=(mkexp(IPrim a),mkexp b)
	IRETURN _ -> error "(return _) can't be used as an expression."
	IGET t k -> LDOT (mkexp t) (mkexp k)
	IASSIGN s e -> unstmt e
	IDO exps -> unstmt e
	ISET t k v -> unstmt e
	IIF c a b -> unstmt e

getArgList (SPRIM _) = error "invalid argument list."
getArgList (STBL t) = case arrayNotArray t of
	(_,(_:_)) -> error "Keyword arguments are not supported."
	(args,[]) -> map getArgName args where
		getArgName (SPRIM (STR s)) = s
		getArgName _ = "invalid argument list."

setExp [] = error "Can't set nothing"
setExp [_] = error "Set it to what?"
setExp [(IVAR s),e] = IASSIGN s e
setExp [_,e] = error "invalid set"
setExp [t,k,v] = ISET t k v
setExp (t:k:remain) = setExp (IGET t k : remain)
getExp [] = error "Can't set nothing"
getExp [e] = e
getExp (e:k:ks) = getExp (IGET e k : ks)
lambdaExp args [e] = IΛ (getArgList args) $ fromSexp e
lambdaExp args body = IΛ (getArgList args) (IDO $ map fromSexp body)
primify [] = []
primify ((SPRIM p,e):more) = (p,fromSexp e) : primify more
primify (_:more) = error "Keys in table-literals may not be tables themselves."
tblExp a p = ITBL $ (zip (map NUM [1..]) (map fromSexp a)) ++ (primify p)
quoteExp (SPRIM p) = IPrim p
quoteExp (STBL t) = ITBL $ primify t

fromSexp (SPRIM (STR s)) = IVAR s
fromSexp (SPRIM x) = IPrim x
fromSexp (STBL t) = case arrayNotArray t of
	((SPRIM(STR "do")):body,[]) -> IDO (map fromSexp body)
	((SPRIM(STR "do")):_,_) -> error "invalid do statement."
	((SPRIM(STR "lambda")):args:body,[]) -> lambdaExp args body
	((SPRIM(STR "lambda")):_,_) -> error "Invalid lambda statement."
	((SPRIM(STR "λ")):args:body,[]) -> lambdaExp args body
	((SPRIM(STR "λ")):_,_) -> error "Invalid λ statement."
	((SPRIM(STR ".")):args,[]) -> getExp (map fromSexp args)
	((SPRIM(STR ".")):_,_) -> error "Invalid . statement."
	((SPRIM(STR "!")):args,[]) -> setExp (map fromSexp args)
	((SPRIM(STR "!")):_,_) -> error "Invalid ! statement."
	([SPRIM(STR "if"),a,b,c],[]) -> IIF (fromSexp a) (fromSexp b) (fromSexp c)
	((SPRIM(STR "if")):_,_) -> error "Invalid if statement."
	((SPRIM(STR "tbl")):array,pairs) -> tblExp array pairs
	([SPRIM(STR "quote"),e],[]) -> quoteExp e
	(_,(_:_)) -> error "Functions may only take ordered arguments."
	(fn:args,[]) -> (ICALL (fromSexp fn) (map fromSexp args))
	([],[]) -> IPrim NIL

retimplicit p = r p where
	r (IΛ args body) = IΛ args (blk body)
	r (ICALL e es) = ICALL (r e) (map r es)
	r (IDO es) = IDO (map r es)
	r (ITBL forms) = ITBL(map (\(p,e)->(p,r e)) forms)
	r (IASSIGN s e) = IASSIGN s (r e)
	r (IGET a b) = IGET (r a) (r b)
	r (ISET a b c) = ISET (r a) (r b) (r c)
	r (IIF a b c) = IIF (r a) (r b) (r c)
	r (IRETURN a) = IRETURN (r a)
	r e@(IPrim _) = e
	r e@(IVAR _) = e
	blk e@(IDO[]) = e
	blk (IDO es) = case reverse es of last:before->IDO $ reverse(blk last:before)
	blk (IIF a b c) = IIF a (blk b) (blk c)
	blk e@(IRETURN _) = e
	blk e = IRETURN(retimplicit e)
