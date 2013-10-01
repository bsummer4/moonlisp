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
	go lang = repl jslib $ fmap (compile lang) . sread1_

jslib = putStrLn $ unlines [
	  "function $(obj,k) {"
	, " return function() {"
	, "  return obj[k].apply(obj,Array.apply(null,arguments)); }};"
	]
