module Main where
import Data.List(filter,lookup)
import qualified Read as S
import qualified IR
import IR(Prim,Exp(PRIM),Prim(NIL),Prim(FALSE),Prim(TRUE))
import IR(Prim(STRING),Prim(NUMBER))

-- TODO Change IR so that functions take table arguments.
-- TODO Implement pattern matching / destructuring.
-- TODO Get function calls to work.
-- TODO Compile basic forms: quote, begin, define, and if.

tester = concat . map show . map IR.mkexp . map compile
proc = concat . map show . map IR.mkexp . map compile . S.read
main = interact proc
compile r = case r of
	S.LSym s -> IR.VAR s
	S.LNum d -> IR.PRIM(IR.NUMBER d)
	S.LTable m -> IR.CALL (compile f) (map compile rest) where
		f = getFirst m
		rest = getRest m
		getRest = map (\(_,v)->v) . filter (\(k,v) -> S.LNum 1/=k)
		getFirst = forSure . lookup (S.LNum 1) where
			forSure Nothing = error "Invalid function call"
			forSure (Just a) = a
