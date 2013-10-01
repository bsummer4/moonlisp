module StrExp(sread,sread1,sread1_,swrite) where
import IRs
import Util
import Data.List

(t,f,n) = (ATOM T, ATOM F, ATOM NIL)
data OneOrTwo a = Two a a | One a
data Ty = TPAREN | TBRAK | TCURLY deriving (Eq,Ord,Show,Read)
data Tok = TSEP | SYM String | TATOM Atom | TBEGIN Ty | TEND Ty
	deriving (Eq,Ord,Show,Read)

-- TODO Make invalid strings impossible to represent.
--  Any string read with ‘read’ should be outputed correctly. However,
--  "“" could be constructed by other code which would case invalid output
--  from write.
showStr s = "“" ++ s ++ "”"
showSym s = if all (not.(`elem` unsymChars)) s then s else "<" ++ s ++ ">"
showTbl ty es = r $ arrayNotArray es where
	boo TPAREN b = "(" ++ b ++ ")"
	boo TBRAK b = "[" ++ b ++ "]"
	boo TCURLY b = "{" ++ b ++ "}"
	r(ordered,named) = boo ty $ mix(order ordered ++ name named)
	name = map pair . sort
	order = map swrite
	mix = concat . intersperse " "
	pair(STR k,v) = swrite(VAR k) ++ "=" ++ swrite v
	pair(k,v) = swrite(ATOM k) ++ "=" ++ swrite v

writes = unlines . map swrite
swrite (ATOM T) = "#t"
swrite (ATOM F) = "#f"
swrite (ATOM NIL) = "#nil"
swrite (ATOM(STR s)) = showStr s
swrite (ATOM(NUM d)) = writeNum d
swrite (VAR s) = showSym s
swrite (DATA es) = showTbl TCURLY es
swrite (CALL es) = showTbl TBRAK es
swrite (MACRO es) = showTbl TPAREN es

unsymChars = "#=()[]{}“” \t\n\r"
wsChars = " \n\t"
todo() = error "TODO"

mksym "#t" = TATOM T
mksym "#f" = TATOM F
mksym "#nil" = TATOM NIL
mksym "#" = SYM "#"
mksym ('#':s) = error $ "Invalid hash pattern: " ++ show ('#':s)
mksym s = case (reads s) of
	[(d,[])] -> TATOM$NUM d
	[(_,_:_)] -> error $ "Invalid number: " ++ show s
	[] -> SYM s

lreadSym sym [] = (mksym $ reverse sym, [])
lreadSym sym (c:cs) = if not (c `elem` unsymChars) then lreadSym (c:sym) cs
	else (mksym $ reverse sym, c:cs)

lreadDelimSym = r "" where
	r acc ('>':more) = (SYM$reverse acc, more)
	r acc (c:more) = r (c:acc) more
	r acc [] = error "Unexpected EOF"

lreadDumbStr = r "" where
	r acc ('"':more) = (TATOM$STR$reverse acc, more)
	r acc (c:more) = r (c:acc) more
	r acc [] = error "Unexpected EOF"

lreadStr = r "" 1 where
	r str 1 ('”':s) = (TATOM$STR$reverse str,s)
	r str _ [] = error $ "Unterminated string: " ++ (show $ reverse str)
	r str d ('”':s) = r ('”':str) (d-1) s
	r str d ('“':s) = r ('“':str) (d+1) s
	r str d (x:s) = r (x:str) d s

tty TPAREN = MACRO
tty TBRAK = CALL
tty TCURLY = DATA
parseSeq ty toks = ordered 0 [] toks where
	ordered :: Double -> [(Atom,Exp)] -> [Tok] -> (Exp,[Tok])
	ordered n acc [] = error "unterminated sequence"
	ordered n acc (TEND TPAREN:ts) = (mktable acc,ts)
	ordered n acc (TEND TBRAK:ts) = (mktable acc,ts)
	ordered n acc (TEND TCURLY:ts) = (mktable acc,ts)
	ordered n acc (TSEP:ts) = error "unexpected ="
	ordered n acc (t:TSEP:ts) = named acc (t:TSEP:ts)
	ordered n acc ts = case parse1 ts of
		Nothing -> error "Unterminated sequence"
		Just(lv,more) -> ordered (n+1) ((NUM n,lv):acc) more
	named :: [(Atom,Exp)] -> [Tok] -> (Exp,[Tok])
	named acc [] = error "unterminated sequence"
	named acc (TEND TPAREN:ts) = (mktable acc,ts)
	named acc (TEND TBRAK:ts) = (mktable acc,ts)
	named acc (TEND TCURLY:ts) = (mktable acc,ts)
	named acc (TSEP:ts) = error "unexpected ="
	named acc (TBEGIN _:_) = error "Tables are not valid keys"
	named acc (a:TSEP:TSEP:ts) = error "unexpected ="
	named acc (a:TSEP:ts) = case (parse1 [a], parse1 ts) of
		(Just(ATOM k,[]),Just(v,remain)) -> named ((k,v):acc) remain
		(Just(VAR k,[]),Just(v,remain)) -> named ((STR k,v):acc) remain
		_ -> error "wat"
	named acc _ = error "ordered elements may not follow named ones"
	mktable acc = (tty ty) acc

parse1 :: [Tok] -> Maybe(Exp,[Tok])
parse1 toks = case toks of
	[] -> Nothing
	(TEND TPAREN:_) -> error "Unexpected sequence terminator"
	(TEND TBRAK:_) -> error "Unexpected sequence terminator"
	(TEND TCURLY:_) -> error "Unexpected sequence terminator"
	(TSEP:_) -> error "Unexpected separator"
	(TBEGIN ty:ts) -> Just(parseSeq ty ts)
	(TATOM T:ts) -> Just(ATOM T,ts)
	(TATOM F:ts) -> Just(ATOM F,ts)
	(TATOM NIL:ts) -> Just(ATOM NIL,ts)
	(TATOM(STR s):ts) -> Just(ATOM(STR s),ts)
	(SYM s:ts) -> Just(VAR s,ts)
	(TATOM(NUM s):ts) -> Just(ATOM$NUM s,ts)

tokenize1 s = case s of
	[] -> Nothing
	'=':cs -> Just(TSEP,cs)
	'(':cs -> Just(TBEGIN TPAREN,cs)
	'[':cs -> Just(TBEGIN TBRAK,cs)
	'{':cs -> Just(TBEGIN TCURLY,cs)
	'“':cs -> Just(lreadStr cs)
	'"':cs -> Just(lreadDumbStr cs)
	'<':cs -> Just(lreadDelimSym cs)
	')':cs -> Just(TEND TPAREN,cs)
	']':cs -> Just(TEND TBRAK,cs)
	'}':cs -> Just(TEND TCURLY,cs)
	c:cs ->
		if c `elem` wsChars then tokenize1 cs else
		if not(c `elem` unsymChars) then Just(lreadSym [c] cs) else
		error("unexpected '" ++ [c] ++ "'")

stream p s = case p s of {Nothing->[]; Just(t,s')->t:stream p s'}
tokenize = stream tokenize1
parse = stream parse1
sread = parse . tokenize
sread1 x = case sread x of {[]->ATOM NIL; (t:ts)->t}
sread1_ x = case sread x of {[t]->Just t; _->Nothing}
rpl = interact $ concat . map show . sread
