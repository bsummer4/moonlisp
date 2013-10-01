module Moonlisp(main) where
import Util
import IRs
import StrSexp
import SexpIR
import IRLua
import IRJS
import LuaCG
import JSCG
import Data.List
import Repl
import System.IO
import System.Environment

lua = map (gen.luaCG) . irLua
js = map (gen.jsCG) . irJS
compile lang = concat . intersperse "\n" . lang . retimplicit . sexpIR
noprompt = putStrLn "_PROMPT2=\"\""
chooseLang [] = lua
chooseLang ["lua"] = lua
chooseLang ["js"] = js
chooseLang _ = error "Invalid arguments"
main = getArgs >>= go.chooseLang where
	go lang = repl (return()) $ fmap (compile lang) . sread1_
