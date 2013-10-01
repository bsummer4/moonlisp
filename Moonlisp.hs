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
chooseLang [] = (lua,noprompt)
chooseLang ["lua"] = (lua,noprompt)
chooseLang ["js"] = (js,jslib)
chooseLang _ = error "Invalid arguments"
main = getArgs >>= go.chooseLang where
	go (lang,start) = repl start $ fmap (compile lang) . sread1_

jslib = putStrLn(unlines
	[ "function cons(a,b) {"
	, " var r=[a];"
	, " for(var i=0; i<b.length; i++) r.push(b[i]);"
	, " return r; }"
	])
