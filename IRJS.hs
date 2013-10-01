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
wrap s = JCALL fn [] where fn = JΛ [] [s]
wraps ss = JCALL fn [] where fn = JΛ [] ss
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
jsop 0 s [] = JOP0 s
jsop 1 s [a] = JOP1 s (mkexp a)
jsop 2 s [a,b] = JOP2 (mkexp a) s (mkexp b)
jsop _ s _ = error $ "Invalid use of operator " ++ s ++ "."
jscall e args = case e of
	IVAR s -> case jsymClass s of
		JOpN n -> jsop n s args
		JSId -> JCALL (mkexp e) (map mkexp args)
		JSReserved -> error $ s ++ " is a reserved word in Javascript."
		JSKeyword -> error $ s ++ " is a keyword in Javascript."
	e -> JCALL (mkexp e) (map mkexp args)

jsvar s = case jsymClass s of
	JSId -> JVAR $ JVar s
	JOpN _ -> error "Javascript operator used as id."
	JSReserved -> error $ s ++ " is a reserved work in Javascript"
	JSKeyword -> error $ s ++ " is a keyword in Javascript."

mkexp :: IExp -> JExp
mkexp e = case e of
	IPRIM x -> JPRIM x
	IVAR s -> jsvar s
	ICALL (IVAR ":") [a,b] -> JBIND (mkexp a) (mkexp b)
	ICALL e args -> jscall e args
	IΛ args body -> JΛ args $ mkstmts body
	ITBL t -> JTABLE $ map f t where f(a,b)=(mkexp(IPRIM a),mkexp b)
	IRETURN _ -> error "(return _) can't be used as an expression."
	IASSIGN s e -> JASSIGN (JVar s) (mkexp e)
	IDO exps -> unstmt e
	IGET t k -> JDOT (mkexp t) (mkexp k)
	ISET t k v -> JASSIGN (JTVar (mkexp t) (mkexp k)) (mkexp v)
	IIF c a b -> (JIF (mkexp c) (mkexp a) (mkexp b))
