-- This defines the basic types and typeclasses.
module Ty where
data Prim = T | F | NIL | STR String | NUM Double deriving (Eq,Show,Ord)
data T = Prim Prim | Tbl [(T,T)] deriving (Eq,Show,Ord)
(t,f,n) = (Prim T, Prim F, Prim NIL)
class CodeGen a where { cgen::a->String }
class Sexp a where { sexp::a->T; unsexp::T->a }
instance Sexp T where { sexp=id; unsexp=id }
