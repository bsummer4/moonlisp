{-# LANGUAGE UnicodeSyntax #-}

module Test where
import Prim
import IR
import Util
import Lua
import Trans
import Syn
import Read
import System.Environment

-- data Atom = T | F | STR String | NUM Double deriving(Eq,Ord)
-- data Tbl a = Tbl [a] [(Atom,a)] deriving(Eq,Ord)
-- data LExp
	-- = LATOM Atom
	-- | LVAR Int
	-- | LCALL Int Int
	-- | Lλ Int LStmt
	-- | LTABLE(Tbl Int)
	-- deriving (Show,Read,Eq,Ord)

-- data LStmt
	-- = LDO [LStmt]
	-- | LBIND Int
	-- | LASSIGN Int LExp
	-- | LIF Int LStmt LStmt
	-- | LRETURN Int
	-- deriving (Show,Read,Eq,Ord)

lbinds ∷ [LStmt]
lbinds =
	[ LFOREIGN_DIRECTIVE "require 'io'"
	, LFOREIGN_DIRECTIVE "require 'os'"
	, LBIND 0, LASSIGN 0 $ Lλ 1 $ LDO
		[ LBIND 2, LASSIGN 2 $ LGLOBAL "io"
		, LBIND 3, LASSIGN 3 $ LATOM $ STR "write"
		, LBIND 4, LASSIGN 4 $ LGET 2 3
		, LRETURN $ LFOREIGN_CALL 4 [1]
		]
	, LBIND 1, LASSIGN 1 $Lλ 2 $ LDO
		[ LBIND 3, LASSIGN 3 $ LGLOBAL "os"
		, LBIND 4, LASSIGN 4 $ LATOM $ STR "exit"
		, LBIND 5, LASSIGN 5 $ LGET 3 4
		, LRETURN $ LFOREIGN_CALL 5 [2] ]]

lEarlyReturn ∷ LStmt
lEarlyReturn = LDO
	[ LBIND 2, LASSIGN 2 $ LATOM $ STR "Hi"
	, LRETURN $ LCALL 0 2
	, LBIND 3, LASSIGN 3 $ LCALL 0 2
	, LRETURN $ LVAR 3
	]

lHelloWorld ∷ [LStmt]
lHelloWorld =
	[ LBIND 2, LASSIGN 2 $ LATOM $ STR $ "Hello World!\n"
	, LBIND 3, LASSIGN 3 $ LCALL 0 2
	, LBIND 4, LASSIGN 4 $ LATOM $ NUM $ 0
	, LRETURN $ LCALL 1 4 ]

simpleExp ∷ Exp
simpleExp = RETURN $ DO [CALL (VAR "write") $ ATOM $ STR "Hello World!\n"]

eHelloWorld ∷ Exp
eHelloWorld = DO
	[ FSTMT "require 'io'"
	, FSTMT "require 'os'"
	, FCALL (GET (GLOBAL "io") (ATOM (STR "write"))) [ATOM$STR "Hello World!\n"]
	, FCALL (GET (GLOBAL "os") (ATOM (STR "exit"))) [ATOM$NUM 0]
	]

mHelloWorld = unlines
	[ "$“require 'io'”"
	, "$“require 'os'”"
	, "$[$io.write “Hello World!\n”]"
	, "$[$os.exit 0]"
	]

parse ∷ String → Exp
parse = DO ∘ map unsyntax ∘ sread

toLIR ∷ Exp → LStmt
toLIR = Trans.compileToLIR ∘ Trans.makeImplicitReturnsExplicit

compiler ∷ Exp → IO()
compiler = putStrLn∘gen∘luaCG∘toLIR

yo = putStrLn∘gen∘luaCG∘LDO
luaHW = yo $ lbinds ++ lHelloWorld

trace inputS = unlines
	[ inputS
	, sep, show ir
	, sep, show lir
	, sep, lua
	] where
		sexp = sread inputS
		ir = DO $ map unsyntax sexp
		lir = toLIR ir
		cg = luaCG lir
		lua = gen cg
		sep = take 78 $ repeat '='

main = getArgs >>= run where
	run [] = run ["trace"]
	run ["trace"] = getContents >>= putStrLn∘trace
	run ["run"] = error "TODO"
	run ["compile"] = getContents >>= (compiler∘parse)
	run _ = error "usage: Test [ trace | run | compile ]"
