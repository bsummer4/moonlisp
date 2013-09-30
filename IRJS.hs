-- Compile from the internal representation into the Lua representation.

module IRJS(irJS) where
import IRs
import StrSexp
import Util
import JSCG
import Data.List
import System.IO
import Repl

irJS = mkstmts
wrap s = JCALL(fn,[]) where fn = JΛ [] [s]
wraps ss = JCALL(fn,[]) where fn = JΛ [] ss
mkstmts :: IExp -> [JStmt]
mkstmts e = case e of
	IDO ss -> concat $ map mkstmts ss
	IRETURN e -> [JRETURN $ mkexp e]
	e -> [JEXP $ mkexp e]

-- Read carefully! This will not infinite loop because
-- ‘unstmt’ and ‘retimplicit’ will always return a
-- lambda expression in this case. That is fed back into
-- ‘mkexp’ which will never call unstmt when given a
-- lambda expression.
unstmt s = mkexp $ retimplicit $ IΛ [] s
mkexp :: IExp -> JExp
mkexp e = case e of
	IPrim x -> JPrim x
	IVAR s -> JVAR $ JVar s
	ICALL e args -> JCALL(mkexp e, map mkexp args)
	IΛ args body -> JΛ args $ mkstmts body
	ITBL t -> JTABLE $ map f t where f(a,b)=(mkexp(IPrim a),mkexp b)
	IRETURN _ -> error "(return _) can't be used as an expression."
	IASSIGN s e -> JASSIGN (JVar s) (mkexp e)
	IDO exps -> unstmt e
	IGET t k -> JDOT (mkexp t) (mkexp k)
	ISET t k v -> JASSIGN (JTVar (mkexp t) (mkexp k)) (mkexp v)
	IIF c a b -> (JIF (mkexp c) (mkexp a) (mkexp b))
