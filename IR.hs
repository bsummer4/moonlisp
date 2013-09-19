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

maybeRead r = case Prelude.reads r of {[(a,_)]->Just a; _->Nothing}
main = getLine >>= putStrLn . cgen . fromSexp . Sexp.read1 >> hFlush stdout >> main

getArgList (Prim _) = error "invalid argument list."
getArgList (Tbl t) = case Sexp.arrayNotArray t of
	(_,(_:_)) -> error "Keyword arguments are not supported."
	(args,[]) -> map getArgName args where
		getArgName (Prim (STR s)) = s
		getArgName _ = "invalid argument list."

-- TODO Merge SET and ASSIGN
setExp [] = error "Can't set nothing"
setExp [_] = error "Set it to what?"
setExp [(VAR s),e] = ASSIGN s e
setExp [_,e] = error "invalid set"
setExp [t,k,v] = SET t k v
setExp (t:k:remain) = setExp (GET t k : remain)

getExp [] = error "Can't set nothing"
getExp [e] = e
getExp (e:k:ks) = getExp (GET e k : ks)

primify :: [(T,T)] -> [(Prim,Exp)]
primify [] = []
primify ((Prim p,e):more) = (p,fromSexp e) : primify more
primify (_:more) = error "Keys in table-literals may not be tables themselves."
tblExp a p = TBL $ (zip (map NUM [1..]) (map fromSexp a)) ++ (primify p)

fromSexp :: T -> Exp
fromSexp (Prim (STR s)) = VAR s
fromSexp (Prim x) = IRPrim x
fromSexp (Tbl t) = case Sexp.arrayNotArray t of
	((Prim(STR "do")):body,[]) -> DO (map fromSexp body)
	((Prim(STR "do")):_,_) -> error "invalid do statement."
	((Prim(STR "lambda")):args:body,[]) -> Λ (getArgList args) (DO $ map fromSexp body)
	((Prim(STR "lambda")):_,_) -> error "Invalid lambda statement."
	((Prim(STR "λ")):args:body,[]) -> Λ (getArgList args) (DO $ map fromSexp body)
	((Prim(STR "λ")):_,_) -> error "Invalid λ statement."
	((Prim(STR ".")):args,[]) -> getExp (map fromSexp args)
	((Prim(STR ".")):_,_) -> error "Invalid . statement."
	((Prim(STR "!")):args,[]) -> setExp (map fromSexp args)
	((Prim(STR "!")):_,_) -> error "Invalid ! statement."
	([Prim(STR "if"),a,b,c],[]) -> IF (fromSexp a) (fromSexp b) (fromSexp c)
	((Prim(STR "if")):_,_) -> error "Invalid if statement."
	((Prim(STR "tbl")):array,pairs) -> tblExp array pairs
	(_,(_:_)) -> error "Functions may only take ordered arguments."
	(fn:args,[]) -> (CALL (fromSexp fn) (map fromSexp args))
	([],[]) -> IRPrim NIL
