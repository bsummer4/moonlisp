module SexpIR(sexpIR) where
import IRs
import Util
import Data.List

getArgList (SPRIM _) = error "invalid argument list."
getArgList (STBL t) = case arrayNotArray t of
	(_,(_:_)) -> error "Keyword arguments are not supported."
	(args,[]) -> map getArgName args where
		getArgName (SPRIM (STR s)) = s
		getArgName _ = "invalid argument list."

setExp [] = error "Can't set nothing"
setExp [_] = error "Set it to what?"
setExp [(IVAR s),e] = IASSIGN s e
setExp [(IGET a k),e] = ISET a k e
setExp [_,e] = error "invalid set"
setExp [t,k,v] = ISET t k v
setExp (t:k:remain) = setExp (IGET t k : remain)
getExp [] = error "Can't set nothing"
getExp [e] = e
getExp (e:k:ks) = getExp (IGET e k : ks)
lambdaExp args [e] = IΛ (getArgList args) $ sexpIR e
lambdaExp args body = IΛ (getArgList args) (IDO $ map sexpIR body)
primify [] = []
primify ((SPRIM p,e):more) = (p,sexpIR e) : primify more
primify (_:more) = error "Keys in table-literals may not be tables themselves."
tblExp a p = ITBL $ (zip (map NUM [1..]) (map sexpIR a)) ++ (primify p)
quoteExp (SPRIM p) = IPrim p
quoteExp (STBL t) = ITBL $ primify t

split f s = r [] (break f s) where
	r acc (pre,[sep]) = reverse ([]:pre:acc)
	r acc (pre,sep:after) = r (pre:acc) (break f after)
	r acc ([],[]) = reverse acc
	r acc (pre,[]) = reverse(pre:acc)

strOrNum s = case maybeRead s :: Maybe Double of {Just n->NUM n; Nothing->STR s}
ivar s = case split (=='.') s of
	["",""] -> IVAR "."
	[s] -> IVAR s
	[] -> error "wut"
	(v:ks) -> getExp $ IVAR v : map (IPrim . strOrNum) ks

sexpIR (SPRIM (STR s)) = ivar s
sexpIR (SPRIM x) = IPrim x
sexpIR (STBL t) = case arrayNotArray t of
	((SPRIM(STR "do")):body,[]) -> IDO (map sexpIR body)
	((SPRIM(STR "do")):_,_) -> error "invalid do statement."
	((SPRIM(STR "lambda")):args:body,[]) -> lambdaExp args body
	((SPRIM(STR "lambda")):_,_) -> error "Invalid lambda statement."
	((SPRIM(STR "λ")):args:body,[]) -> lambdaExp args body
	((SPRIM(STR "λ")):_,_) -> error "Invalid λ statement."
	((SPRIM(STR ".")):args,[]) -> getExp (map sexpIR args)
	((SPRIM(STR ".")):_,_) -> error "Invalid . statement."
	((SPRIM(STR "←")):args,[]) -> setExp (map sexpIR args)
	((SPRIM(STR "←")):_,_) -> error "Invalid ← statement."
	([SPRIM(STR "if"),a,b,c],[]) -> IIF (sexpIR a) (sexpIR b) (sexpIR c)
	((SPRIM(STR "if")):_,_) -> error "Invalid if statement."
	((SPRIM(STR "tbl")):array,pairs) -> tblExp array pairs
	([SPRIM(STR "quote"),e],[]) -> quoteExp e
	(_,(_:_)) -> error "Functions may only take ordered arguments."
	(fn:args,[]) -> (ICALL (sexpIR fn) (map sexpIR args))
	([],[]) -> IPrim NIL
