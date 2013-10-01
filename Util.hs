module Util where
import IRs
import Data.List

-- Atom Operations
maybeRead r = case Prelude.reads r of {[(a,_)]->Just a; _->Nothing}
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
	genBlk i pre post mid =  pre ++ "\n" ++ block (1+i) mid ++ dent (i+1) post
	block i codes = concat $ map (\s->tabs i++r i s++"\n") codes
	genTuple i pre post mid = pre++(concat$intersperse ","$map (r i) mid)++post
	r i (CExp _ (CATOM s)) = s
	r i (CExp _ (CSTMT s (CExp Space c))) = s ++ " " ++ r i (CExp Space c)
	r i (CExp _ (CSTMT s c)) = s ++ r i (delim c)
	r i (CExp _ (CTUPLE(pre,post) mid)) = genTuple i pre post mid
	r i (CExp _ (CBINOP a sep b)) = r i a ++ sep ++ r i b
	r i (CExp _ (CTRIOP a sep1 b sep2 c)) = r i a ++ sep1 ++ r i b ++ sep2 ++ r i c
	r i (CExp _ (CBLOCK(pre,post) mid@[CExp _(CBLOCK _ _)])) = genBlk i pre post mid
	r i (CExp _ (CBLOCK(pre,post) [s])) = pre ++ r i (space s) ++ post
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

retimplicit :: IExp -> IExp
retimplicit p = r p where
	r (IΛ args body) = IΛ args (blk body)
	r (ICALL e es) = ICALL (r e) (map r es)
	r (IDO es) = IDO (map r es)
	r (ITBL forms) = ITBL(map (\(p,e)->(p,r e)) forms)
	r (IASSIGN s e) = IASSIGN s (r e)
	r (IGET a b) = IGET (r a) (r b)
	r (ISET a b c) = ISET (r a) (r b) (r c)
	r (IIF a b c) = IIF (r a) (r b) (r c)
	r (IRETURN a) = IRETURN (r a)
	r e@(IPrim _) = e
	r e@(IVAR _) = e
	blk e@(IDO[]) = e
	blk (IDO es) = case reverse es of last:before->IDO $ reverse(blk last:before)
	blk (IIF a b c) = IIF a (blk b) (blk c)
	blk e@(IRETURN _) = e
	blk e = IRETURN(retimplicit e)
