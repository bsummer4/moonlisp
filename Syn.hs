module Syn(fromSexp,unsyntax) where
import Prim
import IR
import Util
import Read

unsyntax ∷ Exp → Exp
unsyntax = t

fromSexp ∷ SExp → Exp
fromSexp (SATOM(STR s)) = VAR s
fromSexp (SATOM a) = ATOM a
fromSexp s@(STABLE es) = r(ez es) where
	r ([],[]) = error "Invalid syntax: ()"
	r (o,n) = SYNTAX $ tmap fromSexp $ mk o n

mkpattern ∷ Exp → Pattern
mkpattern e = case t e of
	VAR v → PSYM v
	ATOM a → PATOM a
	DATA d → PTBL (tmap mkpattern d)
	_ → error "invalid pattern"

fixform ∷ Exp → (Pattern,Exp)
fixform e = case e of {SYNTAX t→f(ez t); _ → error "wut"} where
	f ([p,e],[]) = (mkpattern p,t e)
	f _ = error "wuuuuut"

transforms ∷ [(String,[Exp] → [(Atom,Exp)] → Exp)]
transforms = (
	[ ("str", \[VAR s] [] → ATOM$STR s)
	, ("lookup", \[a,b] [] → GET (t a) (t b))
	, ("call", \(f:args) o → CALL (t f) $ DATA $ tmap t $ mk args o)
	, ("mkdata", \o n → DATA $ tmap t $ mk o n)
	, ("do", \o [] → DO $ map t o)
	, ("λ", \[VAR a,body] [] → Λ a (t body))
	, ("match", \(p:forms) [] → MATCH (t p) (map fixform forms))
	])

t e = case e of {SYNTAX a→r(ez a); _→e} where
	r (VAR v:o,a) = case lookup v transforms of
		Nothing → error $ "Undefined syntax: " ++ show v
		Just tr → tr o a
	r x = error $ "wut is this? " ++ show x
