module Read(sread,sread1,sread1_,swrite) where
import Prim
import IR
import Util
import Data.List

sread :: String -> [SExp]
sread1 :: String -> Maybe SExp
sread1_ :: String -> SExp
swrite :: SExp -> String

-- Lexing
data OneOrTwo a = Two a a | One a
data Ty = TPAREN | TBRAK | TCURLY deriving (Eq,Ord,Show,Read)
data Tok = TSEP | TBEGIN Ty | TEND Ty | TSYM String | TSTR String
	deriving (Eq,Ord,Show,Read)

wsChars = " \t\n\r"
illegalChars = "\0\v"
syntaxChars = "←()[]{}<>“”\""
niceChar c = not $ any (c `elem`) [wsChars,illegalChars,syntaxChars]
unexpected c = error $ "unexpected character: " ++ show c
illegal c = error $ "illegal character: " ++ show c

lexSym sym [] = (TSYM $ reverse sym, [])
lexSym sym (c:cs) = if not $ any (c `elem`) [illegalChars,syntaxChars,wsChars]
	then lexSym (c:sym) cs
	else (TSYM $ reverse sym, c:cs)

lexDelimSym = r "" where
	r acc ('>':more) = (TSYM$reverse acc, more)
	r acc (c:more) = r (c:acc) more
	r acc [] = unexpected "EOF"

lexDumbStr = r "" where
	r acc ('"':more) = (TSTR$reverse acc, more)
	r acc (c:more) = r (c:acc) more
	r acc [] = unexpected "EOF"

lexStr = r "" 1 where
	r str 1 ('”':s) = (TSTR$reverse str,s)
	r str _ [] = error $ "Unterminated string: " ++ (show $ reverse str)
	r str d ('”':s) = r ('”':str) (d-1) s
	r str d ('“':s) = r ('“':str) (d+1) s
	r str d (x:s) = r (x:str) d s

slex [] = Nothing
slex (c:cs) = case c of
	'←' -> Just $ (TSEP,cs)
	'(' -> Just $ (TBEGIN TPAREN,cs)
	')' -> Just $ (TEND TPAREN,cs)
	'[' -> Just $ (TBEGIN TBRAK,cs)
	']' -> Just $ (TEND TBRAK,cs)
	'{' -> Just $ (TBEGIN TCURLY,cs)
	'}' -> Just $ (TEND TCURLY,cs)
	'<' -> Just $ lexDelimSym cs
	'>' -> unexpected c
	'“' -> Just $ lexStr cs
	'”' -> unexpected c
	'"' -> Just $ lexDumbStr cs
	_ ->
		if c `elem` wsChars then slex cs else
		if c `elem` illegalChars then illegal c else
		Just $ lexSym [c] cs

-- Reading
tbltbl es = STABLE $ mk es []
tag t e = tbltbl [SATOM$STR$t, e]
mkstr s = tag "str" (SATOM$STR$s)
mktbl TPAREN es = STABLE es
mktbl TBRAK es = STABLE $ tcons (SATOM$STR$"call") es
mktbl TCURLY es = STABLE $ tcons (SATOM$STR$"mkdata") es
undot sym@('.':more) = SATOM(STR sym)
undot sym = f (split (=='.') sym) where
	f :: [String] -> SExp
	f [] = error "wat... undot"
	f (first:more) = foldl dot (SATOM$STR first) more
	dot :: SExp -> String -> SExp
	dot exp "" = error $ "Invalid dotted form: " ++ show sym
	dot exp str = tbltbl[SATOM$STR$"call", SATOM$STR$".", exp, mkstr str]

parseSym s = case s of
	"#t" -> SATOM $ T
	"#true" -> SATOM $ T
	"#f" -> SATOM $ F
	"#false" -> SATOM $ F
	('#':s) -> error $ "Invalid hash pattern: " ++ show ('#':s)
	s -> case reads s of
		[] -> undot s
		[(d,[])] -> SATOM $ NUM d
		[(_,_)] -> error $ "Invalid number: " ++ show s
		_ -> error "Unexpected behavior from Prelude.reads."

parse1 :: [Tok] -> Maybe(SExp,[Tok])
parse1 toks = case toks of
	[] -> Nothing
	TEND _ : _ -> error "Unexpected sequence terminator"
	TSEP : _ -> error "Unexpected separator"
	TBEGIN ty : ts -> Just $ parseSeq ty ts
	TSYM s : ts -> Just (parseSym s, ts)
	TSTR s : ts -> Just (mkstr s, ts)

parseSeq :: Ty -> [Tok] -> (SExp,[Tok])
parseSeq ty toks = ordered 1 [] toks where
	mktable end acc = if (ty == end) then mktbl ty (fromList acc) else
		error$concat["non-matching table delimiters: ", show ty, " and ", show end]
	ordered :: Double -> [(Atom,SExp)] -> [Tok] -> (SExp,[Tok])
	ordered n acc [] = error "unterminated sequence"
	ordered n acc (TEND endty:ts) = (mktable endty acc, ts)
	ordered n acc (TSEP:ts) = error "unexpected :"
	ordered n acc (t:TSEP:ts) = named acc (t:TSEP:ts)
	ordered n acc ts = case parse1 ts of
		Nothing -> error "Unterminated sequence"
		Just(lv,more) -> ordered (n+1) ((NUM n,lv):acc) more
	named :: [(Atom,SExp)] -> [Tok] -> (SExp,[Tok])
	named acc [] = error "unterminated sequence"
	named acc (TEND endty:ts) = (mktable endty acc, ts)
	named acc (TSEP:ts) = error "unexpected :"
	named acc (TBEGIN _:_) = error "Tables are not valid keys"
	named acc (a:TSEP:TSEP:ts) = error "unexpected :"
	named acc (a:TSEP:ts) = case (parse1 [a], parse1 ts) of
		(Just(SATOM k,[]),Just(v,remain)) -> named ((k,v):acc) remain
		_ -> error "wat"
	named acc _ = error "ordered elements may not follow named ones"

-- Writting
showSym s = if all niceChar s then s else "<" ++ s ++ ">"
showTbl :: Tbl SExp -> String
showTbl es = r $ ez es where
	r(ordered,named) = "(" ++ (mix $ order ordered ++ name named) ++ ")"
	name = map pair . sort
	order = map swrite
	mix = concat . intersperse " "
	pair(k,v) = swrite(SATOM k) ++ "←" ++ swrite v

writes = unlines . map swrite
swrite (SATOM T) = "#t"
swrite (SATOM F) = "#f"
swrite (SATOM(STR s)) = showSym s
swrite (SATOM(NUM d)) = writeNum d
swrite (STABLE es) = showTbl es

stream p s = case p s of {Nothing->[]; Just(t,s')->t:stream p s'}
tokenize = stream slex
parse = stream parse1
sread = parse . tokenize
sread1 x = case sread x of {[t]->Just t; _->Nothing}
sread1_ x = case sread1 x of {Nothing->error "parse error"; Just x->x}
rpl = interact $ concat . map swrite . sread
