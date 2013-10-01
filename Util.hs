module Util where
import IRs
import Data.List

maybeRead r = case Prelude.reads r of {[(a,_)]->Just a; _->Nothing}
isInt n = n == ((fromIntegral $ truncate n) :: Double)
writeNum n = if isInt n then show(truncate n) else show n
arrayNotArray :: (Tbl Exp) -> ([Exp],[(Atom,Exp)])
arrayNotArray a = r 0 ([],[]) (sort a) where
	r i (o,u) [] = (reverse o,reverse u)
	r i (o,u) ((k,v):m) = if same k i then r (i+1) (v:o,u) m else r i (o,(k,v):u) m
	same k i = k == NUM i
