-- TODO Throw an error if we read duplicate keys.

module Sexp where
import Data.List
import Ty
data Prim = T | F | NIL | STR String | NUM Double deriving (Eq,Show,Read,Ord)
data T = Prim Prim | Tbl [(T,T)] deriving (Eq,Show,Read,Ord)
(t,f,n) = (Prim T, Prim F, Prim NIL)
class Sexp a where { sexp::a->T; unsexp::T->a }
instance Sexp T where { sexp=id; unsexp=id }
data OneOrTwo a = Two a a | One a
data Tok = TSEP | TPrim Prim | TBEGIN | TEND deriving (Eq,Ord,Show,Read)
instance CodeGen T where cgen = write
isInt n = n == ((fromIntegral $ truncate n) :: Double)
writeNum n = if isInt n then show(truncate n) else show n
arrayNotArray a = r 0 ([],[]) (sort a) where
	r i (o,u) [] = (reverse o,reverse u)
	r i (o,u) ((k,v):m) = if same k i then r (i+1) (v:o,u) m else r i (o,(k,v):u) m
	same k i = k == (Prim $ NUM i)

-- TODO Make invalid strings impossible to represent.
--  Any string read with ‘read’ should be outputed correctly. However,
--  "“" could be constructed by other code which would case invalid output
--  from write.
showStr s = if all (`elem` symChars) s then s else "“" ++ s ++ "”"
showTbl es = r $ arrayNotArray es where
	r(ordered,named) = "(" ++ mix(order ordered ++ name named) ++ ")"
	name = map pair . sort
	order = map write
	mix = concat . intersperse " "
	pair(k,v) = write k ++ "=" ++ write v

writes = unlines . map write
write (Prim T) = "#t"
write (Prim F) = "#f"
write (Prim NIL) = "#nil"
write (Prim(STR s)) = showStr s
write (Prim(NUM d)) = writeNum d
write (Tbl es) = showTbl es

symChars = "-+_.#" ++ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']
wsChars = " \n\t"
todo() = error "TODO"

mksym "#t" = TPrim T
mksym "#f" = TPrim F
mksym "#nil" = TPrim NIL
mksym ('#':s) = error $ "Invalid hash pattern: " ++ show ('#':s)
mksym s = TPrim $ case (reads s) of { [(d,[])]->NUM d; []->STR s }

lreadSym sym [] = (mksym $ reverse sym, [])
lreadSym sym (c:cs) = if c `elem` symChars then lreadSym (c:sym) cs
	else (mksym $ reverse sym, c:cs)

lreadStr = r "" 1 where
	r str 1 ('”':s) = (TPrim$STR$reverse str,s)
	r str _ [] = error $ "Unterminated string: " ++ (show $ reverse str)
	r str d ('”':s) = r ('”':str) (d-1) s
	r str d ('“':s) = r ('“':str) (d+1) s
	r str d (x:s) = r (x:str) d s

parseSeq toks = ordered 0 [] toks where
	ordered n acc [] = error "unterminated sequence"
	ordered n acc (TEND:ts) = (mktable acc,ts)
	ordered n acc (TSEP:ts) = error "unexpected ="
	ordered n acc (t:TSEP:ts) = named acc (t:TSEP:ts)
	ordered n acc ts = case parse1 ts of
		Nothing -> error "Unterminated sequence"
		Just(lv,more) -> ordered (n+1) ((Prim$NUM n,lv):acc) more
	named acc [] = error "unterminated sequence"
	named acc (TEND:ts) = (mktable acc,ts)
	named acc (TSEP:ts) = error "unexpected ="
	named acc (TBEGIN:_) = error "Tables are not valid keys"
	named acc (a:TSEP:TSEP:ts) = error "unexpected ="
	named acc (a:TSEP:ts) = case (parse1 [a], parse1 ts) of
		(Just(k,[]),Just(v,remain)) -> named ((k,v):acc) remain
		_ -> error "wat"
	named acc _ = error "ordered elements may not follow named ones"
	mktable acc = Tbl acc

parse1 toks = case toks of
	[] -> Nothing
	(TEND:_) -> error "Unexpected sequence terminator"
	(TSEP:_) -> error "Unexpected separator"
	(TBEGIN:ts) -> Just(parseSeq ts)
	(TPrim T:ts) -> Just(Prim T,ts)
	(TPrim F:ts) -> Just(Prim F,ts)
	(TPrim NIL:ts) -> Just(Prim NIL,ts)
	(TPrim(STR s):ts) -> Just(Prim$STR s,ts)
	(TPrim(NUM s):ts) -> Just(Prim$NUM s,ts)

tokenize1 s = case s of
	[] -> Nothing
	'=':cs -> Just(TSEP,cs)
	'(':cs -> Just(TBEGIN,cs)
	'“':cs -> Just(lreadStr cs)
	')':cs -> Just(TEND,cs)
	c:cs ->
		if c `elem` wsChars then tokenize1 cs else
		if c `elem` symChars then Just(lreadSym [c] cs) else
		error("unexpected '" ++ [c] ++ "'")

streamProcess proc1 s = case proc1 s of
	Nothing -> []
	Just(t,s') -> t:streamProcess proc1 s'

tokenize = streamProcess tokenize1
parse = streamProcess parse1
read = parse . tokenize
read1 x = case Sexp.read x of {[]->Prim NIL; (t:ts)->t}
rpl = interact $ concat . map show . Sexp.read
