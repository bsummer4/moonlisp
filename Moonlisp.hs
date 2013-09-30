module Moonlisp where
import IRLua
import IRs
import StrSexp
import Util
import LuaCodeGen
import Data.List
import System.IO
import Repl

maybeRead r = case Prelude.reads r of {[(a,_)]->Just a; _->Nothing}
compile = concat . intersperse "\n" . map (gen . luaCodeGen) . mkstmts . retimplicit .  fromSexp
main = repl noprompt $ fmap compile . sread1_
noprompt = putStrLn "_PROMPT2=\"\""
