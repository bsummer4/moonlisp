-- The file defines a representation of a subset of the Lua AST.
--  This is used for compiling to and generating Lua code.

module CodeGen where
import Data.List
import Ty

data DelimTy = Unsafe | Space | Safe deriving (Show,Read)
type Code = (DelimTy,Code1)
data Code1
	= ATOM String
	| STMT String Code
	| BLOCK (String,String) [Code]
	| TUPLE (String,String) [Code]
	| BINOP Code String Code
	deriving (Show,Read)

tabs 0 = ""
tabs n = ' ':tabs(n-1)
dent i "" = ""
dent i s = tabs i ++ s
gen c = r 0 c where
	block i codes = concat $ map (\s->tabs i++r i s++"\n") codes
	r i (_,ATOM s) = s
	r i (_,STMT s (Space,c)) = s ++ " " ++ r i (Space,c)
	r i (_,STMT s c) = s ++ r i (delim c)
	r i (_,TUPLE(pre,post) mid) = pre++(concat$intersperse ","$map (r i) mid)++post
	r i (_,BINOP a sep b) = r i a ++ sep ++ r i b
	r i (_,BLOCK(pre,post) mid) = pre ++ "\n" ++ block (1+i) mid ++ dent (i+1) post

atom x = (Space,ATOM x)
block d cs = (Unsafe,BLOCK d cs)
tuple d cs = (Safe,TUPLE d cs)
binop a o (Unsafe,b) = binop a o (delim(Unsafe,b))
binop (Unsafe,a) o b = binop (delim(Unsafe,a)) o b
binop a o b = (Unsafe,BINOP a o b)
stmt s c = (Unsafe,STMT s c)
delim (Safe,e) = (Safe,e)
delim (Unsafe,e) = (Safe,TUPLE ("(",")") [(Unsafe,e)])
delim (Space,e) = (Safe,TUPLE ("(",")") [(Space,e)])
jux (Unsafe,a) b = jux (delim(Unsafe,a)) b
jux a (Unsafe,b) = jux a (delim(Unsafe,b))
jux (Space,a) (Space,b) = (Space,BINOP (Space,a) " " (Space,b))
jux a b = (Space,BINOP a "" b)
