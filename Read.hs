-- TODO Throw an error if we read duplicate keys.

module Read where
import Data.List

data OneOrTwo a = Two a a | One a
data Tok = SEP | NUM Double | SYM String | BEGIN | END deriving (Eq,Ord,Show)
data LVal = LSym String | LNum Double | LTable [(LVal,LVal)]
	deriving (Eq,Ord)
instance Show LVal where show = writeLVal
symChars = "-+_." ++ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']
wsChars = " \n\t"
todo() = error "TODO"

writeLVal = f where
	f (LSym s) = s
	f (LNum d) = show d
	f (LTable es) = "(" ++ concat(map pair es) ++ ")"
	pair(k,v) = " " ++ f k ++ "=" ++ f v ++ " "

mksym s = case (reads s) of { [(d,"")]->NUM d; []->SYM s }
lreadSym soFar stream = case (soFar,stream) of
	(sym,[]) -> (mksym $ reverse sym, [])
	(sym,c:cs) -> if c `elem` symChars then lreadSym (c:sym) cs
		else (mksym $ reverse sym, c:cs)

parseSeq toks = ordered 1 [] toks where
	ordered n acc [] = error "unterminated sequence"
	ordered n acc (END:ts) = (mktable acc,ts)
	ordered n acc (SEP:ts) = error "unexpected ="
	ordered n acc (t:SEP:ts) = named acc (t:SEP:ts)
	ordered n acc ts = case parse1 ts of
		Nothing -> error "Unterminated sequence"
		Just(lv,more) -> ordered (n+1) ((LNum n,lv):acc) more
	named acc [] = error "unterminated sequence"
	named acc (END:ts) = (mktable acc,ts)
	named acc (SEP:ts) = error "unexpected ="
	named acc (BEGIN:_) = error "Tables are not valid keys"
	named acc (a:SEP:SEP:ts) = error "unexpected ="
	named acc (a:SEP:ts) = case (parse1 [a], parse1 ts) of
		(Just(k,[]),Just(v,remain)) -> named ((k,v):acc) remain
		_ -> error "wat"
	named acc _ = error "ordered elements may not follow named ones"
	mktable acc = LTable acc

parse1 toks = case toks of
	[] -> Nothing
	(END:_) -> error "Unexpected sequence terminator"
	(SEP:_) -> error "Unexpected separator"
	(BEGIN:ts) -> Just(parseSeq ts)
	(SYM s:ts) -> Just(LSym s,ts)
	(NUM s:ts) -> Just(LNum s,ts)

tokenize1 s = case s of
	[] -> Nothing
	'=':cs -> Just(SEP,cs)
	'(':cs -> Just(BEGIN,cs)
	')':cs -> Just(END,cs)
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
rpl = interact $ concat . map show . Read.read
