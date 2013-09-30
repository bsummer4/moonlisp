module Util where
import IRs
import Data.List

-- Atom Operations
isInt n = n == ((fromIntegral $ truncate n) :: Double)
writeNum n = if isInt n then show(truncate n) else show n
arrayNotArray a = r 0 ([],[]) (sort a) where
	r i (o,u) [] = (reverse o,reverse u)
	r i (o,u) ((k,v):m) = if same k i then r (i+1) (v:o,u) m else r i (o,(k,v):u) m
	same k i = k == (SPRIM $ NUM i)

-- Code Generation Utilities
tabs 0 = ""
tabs n = ' ':tabs(n-1)
dent i "" = ""
dent i s = tabs i ++ s
gen c = r 0 c where
	block i codes = concat $ map (\s->tabs i++r i s++"\n") codes
	r i (CExp _ (CATOM s)) = s
	r i (CExp _ (CSTMT s (CExp Space c))) = s ++ " " ++ r i (CExp Space c)
	r i (CExp _ (CSTMT s c)) = s ++ r i (delim c)
	r i (CExp _ (CTUPLE(pre,post) mid)) = pre++(concat$intersperse ","$map (r i) mid)++post
	r i (CExp _ (CBINOP a sep b)) = r i a ++ sep ++ r i b
	r i (CExp _ (CBLOCK(pre,post) [s])) = pre ++ r i (space s) ++ post
	r i (CExp _ (CBLOCK(pre,post) mid)) = pre ++ "\n" ++ block (1+i) mid ++ dent (i+1) post

atom x = CExp Space $ CATOM x
block d cs = CExp Space $ CBLOCK d cs
blockexp d cs = CExp Unsafe $ CBLOCK d cs
tuple d cs = CExp Safe $ CTUPLE d cs
binop a o (CExp Unsafe b) = binop a o (delim(CExp Unsafe b))
binop (CExp Unsafe a) o b = binop (delim(CExp Unsafe a)) o b
binop a o b = CExp Unsafe $ CBINOP a o b
stmt s c = CExp Space $ CSTMT s c
delim (CExp Safe e) = CExp Safe e
delim (CExp Unsafe e) = CExp Safe $ CTUPLE ("(",")") [CExp Unsafe e]
delim (CExp Space e) = CExp Safe $ CTUPLE ("(",")") [CExp Space e]
space (CExp Safe e) = CExp Safe $ e
space (CExp Space e) = CExp Safe $ CTUPLE (" "," ") [CExp Unsafe e]
space (CExp Unsafe e) = CExp Safe $ CTUPLE ("(",")") [CExp Space e]
jux (CExp Unsafe a) b = jux (delim $ CExp Unsafe a) b
jux a (CExp Unsafe b) = jux a (delim $ CExp Unsafe b)
jux (CExp Space a) (CExp Space b) =
	CExp Space $ CBINOP (CExp Space a) " " (CExp Space b)
jux a b = CExp Space $ CBINOP a "" b
