module StrSexp(sread,sread1,sread1_,swrite) where
import IRs
import Util
import Data.List

(t,f,n) = (SPRIM T, SPRIM F, SPRIM NIL)
data OneOrTwo a = Two a a | One a
data Tok = TSEP | SYM String | TPRIM Atom | TBEGIN | TEND
	deriving (Eq,Ord,Show,Read)

-- TODO Make invalid strings impossible to represent.
--  Any string read with ‘read’ should be outputed correctly. However,
--  "“" could be constructed by other code which would case invalid output
--  from write.
showStr s = if all (not.(`elem` unsymChars)) s then s else "“" ++ s ++ "”"
showTbl es = r $ arrayNotArray es where
	r(ordered,named) = "(" ++ mix(order ordered ++ name named) ++ ")"
	name = map pair . sort
	order = map swrite
	mix = concat . intersperse " "
	pair(k,v) = swrite k ++ "=" ++ swrite v

writes = unlines . map swrite
swrite (SPRIM T) = "#t"
swrite (SPRIM F) = "#f"
swrite (SPRIM NIL) = "#nil"
swrite (SPRIM(STR s)) = showStr s
swrite (SPRIM(NUM d)) = writeNum d
swrite (STBL es) = showTbl es

unsymChars = "()[]{}“” \t\n\r"
wsChars = " \n\t"
todo() = error "TODO"

mksym "#t" = TPRIM T
mksym "#f" = TPRIM F
mksym "#nil" = TPRIM NIL
mksym "#" = SYM "#"
mksym ('#':s) = error $ "Invalid hash pattern: " ++ show ('#':s)
mksym s = case (reads s) of
	[(d,[])] -> TPRIM$NUM d
	[(_,_:_)] -> error $ "Invalid number: " ++ show s
	[] -> SYM s

lreadSym sym [] = (mksym $ reverse sym, [])
lreadSym sym (c:cs) = if not (c `elem` unsymChars) then lreadSym (c:sym) cs
	else (mksym $ reverse sym, c:cs)

lreadDelimSym = r "" where
	r acc (']':more) = (SYM$reverse acc, more)
	r acc (c:more) = r (c:acc) more
	r acc [] = error "Unexpected EOF"

lreadDumbStr = r "" where
	r acc ('"':more) = (TPRIM$STR$reverse acc, more)
	r acc (c:more) = r (c:acc) more
	r acc [] = error "Unexpected EOF"

lreadStr = r "" 1 where
	r str 1 ('”':s) = (TPRIM$STR$reverse str,s)
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
		Just(lv,more) -> ordered (n+1) ((SPRIM$NUM n,lv):acc) more
	named acc [] = error "unterminated sequence"
	named acc (TEND:ts) = (mktable acc,ts)
	named acc (TSEP:ts) = error "unexpected ="
	named acc (TBEGIN:_) = error "Tables are not valid keys"
	named acc (a:TSEP:TSEP:ts) = error "unexpected ="
	named acc (a:TSEP:ts) = case (parse1 [a], parse1 ts) of
		(Just(k,[]),Just(v,remain)) -> named ((k,v):acc) remain
		_ -> error "wat"
	named acc _ = error "ordered elements may not follow named ones"
	mktable acc = STBL acc

qtbl = STBL . zip (map (SPRIM . NUM) [0..])
quote e = qtbl [SPRIM$STR$"quote",e]

parse1 toks = case toks of
	[] -> Nothing
	(TEND:_) -> error "Unexpected sequence terminator"
	(TSEP:_) -> error "Unexpected separator"
	(TBEGIN:ts) -> Just(parseSeq ts)
	(TPRIM T:ts) -> Just(SPRIM T,ts)
	(TPRIM F:ts) -> Just(SPRIM F,ts)
	(TPRIM NIL:ts) -> Just(SPRIM NIL,ts)
	(TPRIM(STR s):ts) -> Just(quote$SPRIM$STR s,ts)
	(SYM s:ts) -> Just(SPRIM$STR s,ts)
	(TPRIM(NUM s):ts) -> Just(SPRIM$NUM s,ts)

tokenize1 s = case s of
	[] -> Nothing
	'=':cs -> Just(TSEP,cs)
	'(':cs -> Just(TBEGIN,cs)
	'“':cs -> Just(lreadStr cs)
	'"':cs -> Just(lreadDumbStr cs)
	'[':cs -> Just(lreadDelimSym cs)
	')':cs -> Just(TEND,cs)
	c:cs ->
		if c `elem` wsChars then tokenize1 cs else
		if not(c `elem` unsymChars) then Just(lreadSym [c] cs) else
		error("unexpected '" ++ [c] ++ "'")

stream p s = case p s of {Nothing->[]; Just(t,s')->t:stream p s'}
tokenize = stream tokenize1
parse = stream parse1
sread = parse . tokenize
sread1 x = case sread x of {[]->SPRIM NIL; (t:ts)->t}
sread1_ x = case sread x of {[t]->Just t; _->Nothing}
rpl = interact $ concat . map show . sread
