module IR where
import Sexp
import qualified Lua as L
import qualified LuaCodeGen as LCG
import qualified CodeGen as G
import Data.List
import System.IO
import Repl

data Exp
	= IRPrim Prim
	| VAR String
	| CALL Exp [Exp]
	| Λ [String] Exp
	| DO [Exp]
	| TBL [(Prim,Exp)]
	| ASSIGN String Exp
	| GET Exp Exp
	| SET Exp Exp Exp
	| IF Exp Exp Exp
	deriving (Read,Show)

tmp = L.Var "tmp"
returnBlock stmts = L.BLOCK (L.LOCAL tmp : stmts) $ Just $ L.RETURN $ L.VAR tmp
mkblock e = L.BLOCK (mkstmts e) Nothing
wrap s = L.CALLEXP $ L.FnCall fn [] where fn = L.Λ [] $ returnBlock [s]
wraps ss = L.CALLEXP $ L.FnCall fn [] where fn = L.Λ [] $ returnBlock ss
mkstmts e = case e of
	DO ss -> concat $ map mkstmts ss
	CALL e args -> [L.ASSIGN tmp $ L.CALLEXP $ L.FnCall (mkexp e) (map mkexp args)]
	ASSIGN s e -> [L.ASSIGN tmp (mkexp e), L.ASSIGN (L.Var s) (L.VAR tmp)]
	e -> [L.ASSIGN tmp (mkexp e)]

mkexp e = case e of
	IRPrim x -> L.LPrim x
	VAR s -> L.VAR $ L.Var s
	ASSIGN s e -> wraps [L.ASSIGN(L.Var s)(mkexp e),L.ASSIGN tmp (L.VAR$L.Var s)]
	CALL e args -> L.CALLEXP $ L.FnCall (mkexp e) (map mkexp args)
	Λ args body -> L.Λ args $ returnBlock $ mkstmts body
	DO exps -> wraps $ concat $ map mkstmts exps
	TBL t -> L.TABLE $ map f t where f(a,b)=(mkexp(IRPrim a),mkexp b)
	GET t k -> L.DOT (mkexp t) (mkexp k)
	SET t k v -> wrap $ L.ASSIGN (L.TVar(mkexp t)(mkexp k)) (mkexp v)
	IF c a b -> wrap $ L.IF (mkexp c) (mkblock a) (mkblock b)

maybeRead r = case Prelude.reads r of {[(a,_)]->Just a; _->Nothing}
compile = concat . intersperse "\n" . map (G.gen . LCG.cg) . mkstmts . fromSexp
main = repl noprompt $ fmap compile . Sexp.read1_
noprompt = putStrLn "_PROMPT2=\"\""

getArgList (Prim _) = error "invalid argument list."
getArgList (Tbl t) = case Sexp.arrayNotArray t of
	(_,(_:_)) -> error "Keyword arguments are not supported."
	(args,[]) -> map getArgName args where
		getArgName (Prim (STR s)) = s
		getArgName _ = "invalid argument list."

setExp [] = error "Can't set nothing"
setExp [_] = error "Set it to what?"
setExp [(VAR s),e] = ASSIGN s e
setExp [_,e] = error "invalid set"
setExp [t,k,v] = SET t k v
setExp (t:k:remain) = setExp (GET t k : remain)
getExp [] = error "Can't set nothing"
getExp [e] = e
getExp (e:k:ks) = getExp (GET e k : ks)
lambdaExp args [e] = Λ (getArgList args) $ fromSexp e
lambdaExp args body = Λ (getArgList args) (DO $ map fromSexp body)
primify [] = []
primify ((Prim p,e):more) = (p,fromSexp e) : primify more
primify (_:more) = error "Keys in table-literals may not be tables themselves."
tblExp a p = TBL $ (zip (map NUM [1..]) (map fromSexp a)) ++ (primify p)
quoteExp (Prim p) = IRPrim p
quoteExp (Tbl t) = TBL $ primify t

fromSexp (Prim (STR s)) = VAR s
fromSexp (Prim x) = IRPrim x
fromSexp (Tbl t) = case Sexp.arrayNotArray t of
	((Prim(STR "do")):body,[]) -> DO (map fromSexp body)
	((Prim(STR "do")):_,_) -> error "invalid do statement."
	((Prim(STR "lambda")):args:body,[]) -> lambdaExp args body
	((Prim(STR "lambda")):_,_) -> error "Invalid lambda statement."
	((Prim(STR "λ")):args:body,[]) -> lambdaExp args body
	((Prim(STR "λ")):_,_) -> error "Invalid λ statement."
	((Prim(STR ".")):args,[]) -> getExp (map fromSexp args)
	((Prim(STR ".")):_,_) -> error "Invalid . statement."
	((Prim(STR "!")):args,[]) -> setExp (map fromSexp args)
	((Prim(STR "!")):_,_) -> error "Invalid ! statement."
	([Prim(STR "if"),a,b,c],[]) -> IF (fromSexp a) (fromSexp b) (fromSexp c)
	((Prim(STR "if")):_,_) -> error "Invalid if statement."
	((Prim(STR "tbl")):array,pairs) -> tblExp array pairs
	([Prim(STR "quote"),e],[]) -> quoteExp e
	(_,(_:_)) -> error "Functions may only take ordered arguments."
	(fn:args,[]) -> (CALL (fromSexp fn) (map fromSexp args))
	([],[]) -> IRPrim NIL
