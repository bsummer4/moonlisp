-- Compile from the internal representation into the Lua representation.

module IRLua(irLua) where
import IRs
import StrSexp
import Util
import LuaCG
import Data.List
import System.IO
import Repl

irLua = mkstmts
wrap s = LCALLEXP(fn,[]) where fn = LΛ [] [s]
wraps ss = LCALLEXP(fn,[]) where fn = LΛ [] ss
mkstmts e = case e of
	IDO ss -> concat $ map mkstmts ss
	ICALL e args -> [LCALLSTMT(mkexp e, map mkexp args)]
	IASSIGN s e -> [LASSIGN (LVar s) (mkexp e)]
	IRETURN e -> [LRETURN $ mkexp e]
	ISET t k v -> [LASSIGN (LTVar (mkexp t) (mkexp k)) (mkexp v)]
	IIF c a b -> [LIF (mkexp c) (mkstmts a) (mkstmts b)]
	e -> [LASSIGN LTMP (mkexp e)]

-- Read carefully! This will not infinite loop because
-- ‘unstmt’ and ‘retimplicit’ will always return a
-- lambda expression in this case. That is fed back into
-- ‘mkexp’ which will never call unstmt when given a
-- lambda expression.
unstmt s = mkexp $ retimplicit $ IΛ [] s
mkexp e = case e of
	IPRIM x -> LPRIM x
	IVAR s -> LVAR $ LVar s
	ICALL e args -> LCALLEXP(mkexp e, map mkexp args)
	IΛ args body -> LΛ args $ mkstmts body
	ITBL t -> LTABLE $ map f t where f(a,b)=(mkexp(IPRIM a),mkexp b)
	IRETURN _ -> error "(return _) can't be used as an expression."
	IGET t k -> LDOT (mkexp t) (mkexp k)
	IASSIGN s e -> unstmt e
	IDO exps -> unstmt e
	ISET t k v -> unstmt e
	IIF c a b -> unstmt e
