module IRs where

-- Atomic Data Types
data Atom = T | F | NIL | STR String | NUM Double deriving (Eq,Show,Read,Ord)

-- S-expression Representation
data Sexp = SPRIM Atom | STBL [(Sexp,Sexp)] deriving (Eq,Show,Read,Ord)

-- Code Generation
data CExp = CExp DelimTy CExp1 deriving (Show,Read)
data DelimTy = Unsafe | Space | Safe deriving (Show,Read)
data CExp1
	= CATOM String
	| CSTMT String CExp
	| CBLOCK (String,String) [CExp]
	| CTUPLE (String,String) [CExp]
	| CBINOP CExp String CExp
	| CTRIOP CExp String CExp String CExp
	| CSEMI CExp
	deriving (Show,Read)

-- Internal Representation
-- TODO HACK Only ‘return’ statements generated by ‘retimplicit’ are safe.
--  Since we can't support arbitrary ‘return’ statments, this should not
--  be allowed in this ADT.
data IExp
	= IPrim Atom
	| IVAR String
	| ICALL IExp [IExp]
	| IΛ [String] IExp
	| IDO [IExp]
	| ITBL [(Atom,IExp)]
	| IASSIGN String IExp
	| IGET IExp IExp
	| ISET IExp IExp IExp
	| IIF IExp IExp IExp
	| IRETURN IExp
	deriving (Read,Show)

-- Lua Representation
data LVar = LTMP | LVar String | LTVar LExp LExp deriving (Show,Read)
data LExp
	= LPrim Atom
	| LCALLEXP (LExp,[LExp])
	| LVAR LVar
	| LΛ [String] [LStmt]
	| LDOT LExp LExp
	| LTABLE [(LExp,LExp)]
	deriving (Show,Read)

data LStmt
	= LDO [LStmt]
	| LASSIGN LVar LExp
	| LLOCAL LVar
	| LIF LExp [LStmt] [LStmt]
	| LCALLSTMT (LExp,[LExp])
	| LRETURN LExp
	| LBREAK
	| LCONTINUE
	deriving (Show,Read)

-- Javascript Representation
data JVar = JTMP | JVar String | JTVar JExp JExp deriving (Show,Read)
data JExp
	= JPrim Atom
	| JCALL (JExp,[JExp])
	| JVAR JVar
	| JΛ [String] [JStmt]
	| JDOT JExp JExp
	| JTABLE [(JExp,JExp)]
	| JOP0 String
	| JOP1 String JExp
	| JOP2 JExp String JExp
	| JIF JExp JExp JExp
	| JASSIGN JVar JExp
	deriving (Show,Read)

data JStmt
	= JLOCAL JVar
	| JEXP JExp
	| JRETURN JExp
	| JBREAK
	| JCONTINUE
	deriving (Show,Read)
