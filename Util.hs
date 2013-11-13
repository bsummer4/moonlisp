module Util where
import Prim
import IR
import Data.List

split isSep l = r [] [] l where
	r strs chars [] = reverse(reverse chars:strs)
	r strs chars (e:es) = if isSep e then r (reverse chars:strs) [] es else
		r strs (e:chars) es

maybeRead r = case Prelude.reads r of {[(a,_)]->Just a; _->Nothing}
isInt n = n == ((fromIntegral $ truncate n) :: Double)
writeNum n = if isInt n then show(truncate n) else show n

-- Code Generation Utilities
tabs 0 = ""
tabs n = ' ':tabs(n-1)
dent i "" = ""
dent i s = tabs i ++ s
gen c = r 0 c where
	genBlk i pre post mid =  pre ++ "\n" ++ block (1+i) mid ++ dent (i+1) post
	block i codes = concat $ map (\s->tabs i++r i s++"\n") codes
	genTuple i pre post mid = pre++(concat$intersperse ","$map (r i) mid)++post
	r i (CExp _ (CATOM s)) = s
	r i (CExp _ (CSTMT s (CExp Space c))) = s ++ " " ++ r i (CExp Space c)
	r i (CExp _ (CSTMT s c)) = s ++ r i (delim c)
	r i (CExp _ (CTUPLE(pre,post) mid)) = genTuple i pre post mid
	r i (CExp _ (CBINOP a sep b)) = r i a ++ sep ++ r i b
	r i (CExp _ (CTRIOP a sep1 b sep2 c)) = r i a ++ sep1 ++ r i b ++ sep2 ++ r i c
	--r i (CExp _ (CBLOCK(pre,post) mid@[CExp _(CBLOCK _ _)])) = genBlk i pre post mid
	--r i (CExp _ (CBLOCK(pre,post) [s])) = pre ++ r i (space s) ++ post
	r i (CExp _ (CBLOCK(pre,post) mid)) = genBlk i pre post mid
	r i (CExp _ (CSEMI c)) = r i c ++ ";"

atom x = CExp Space $ CATOM x
block d cs = CExp Space $ CBLOCK d cs
blockexp d cs = CExp Unsafe $ CBLOCK d cs
tuple d cs = CExp Safe $ CTUPLE d cs
binop a o (CExp Unsafe b) = binop a o (delim(CExp Unsafe b))
binop (CExp Unsafe a) o b = binop (delim(CExp Unsafe a)) o b
binop a o b = CExp Unsafe $ CBINOP a o b
triop a x b y c = CExp Unsafe $ CTRIOP (delim a) x (delim b) y (delim c)
stmt s c = CExp Space $ CSTMT s c
semi e@(CExp Unsafe a) = semi(delim e)
semi e = CExp Safe (CSEMI e)
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
