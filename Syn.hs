{-# LANGUAGE UnicodeSyntax #-}

module Syn(unsyntax) where
import Prim
import IR
import Util
import Read

unsyntax ∷ SExp → Exp
unsyntax = t

mkpattern ∷ SExp → Pattern
mkpattern e = case e of
	SATOM(STR v) → PSYM v
	SATOM a → PATOM a
	STABLE d → PTBL $ tmap mkpattern d

matchPair ∷ SExp → (Pattern,Exp)
matchPair e = case e of {STABLE t→f(ez t); _ → error "wut"} where
	f ([p,e],[]) = (mkpattern p,t e)
	f _ = error "wuuuuut"

mkforeign ∷ SExp → Exp
mkforeign (SATOM(STR v)) = GLOBAL v
mkforeign (STABLE s) = case ez s of
	(SATOM(STR "_call"):f:args,[]) → FCALL (t f) $ map t args
	(SATOM(STR "_meth"):obj:(SATOM(STR m)):args,[]) → FMETHOD (t obj) m $ map t args
	([SATOM(STR "_str"),SATOM(STR s)],[]) → FSTMT s
	([SATOM(STR "_get"),k,(SATOM(STR v))],[]) → GET (mkforeign k) (ATOM$STR v)
	_ → error "Invalid use of $."
mkforeign _ = error "Invalid use of $"

transforms ∷ [(String,[SExp] → [(Atom,SExp)] → Exp)]
transforms = (
	[ ("_str", \[SATOM(STR s)] [] → ATOM$STR s)
	, ("_get", \[a,SATOM(STR b)] [] → GET (t a) (ATOM$STR b))
	, ("_foreign", \[a] [] → mkforeign a)
	, ("_call", \(f:args) o → CALL (t f) $ DATA $ tmap t $ mk args o)
	, ("_tbl", \o n → DATA $ tmap t $ mk o n)
	, ("do", \o [] → DO $ map t o)
	, ("λ", \[SATOM(STR a),body] [] → Λ a (t body))
	, ("fn", \[SATOM(STR a),body] [] → Λ a (t body))
	, ("match", \(p:forms) [] → MATCH (t p) (map matchPair forms))
	, ("def", \[p,exp] [] → DEF (mkpattern p) (t exp))
	])

t ∷ SExp → Exp
t e = case e of
	SATOM(STR v) → VAR v
	SATOM a → ATOM a
	STABLE a → r $ ez a where
		r (SATOM(STR v):o,a) = case lookup v transforms of
			Nothing → error $ "Undefined syntax: " ++ show v
			Just tr → tr o a
		r x = error $ "This doesn't make sense: " ++ show x
