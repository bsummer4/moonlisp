module IR where

-- Atomic Data Types
data Atom = T | F | NIL | STR String | NUM Double deriving(Show,Read,Eq,Ord)
type Tbl a = [(Atom,a)]
data Pattern = PSYM String | PATOM Atom | PTBL(Tbl Pattern)
	deriving(Show,Read,Eq,Ord)

data Exp
	= ATOM Atom
	| VAR String
	| DO [Exp]
	| Λ String Exp
	| MATCH Exp [(Pattern,Exp)]
	| JS String
	| LUA String
	| PY String
	| CALL(Tbl Exp)
	| DATA(Tbl Exp)
	| MACRO(Tbl Exp)
	deriving(Read,Show,Eq,Ord)

-- Lower-level representation.
data LExp
	= LATOM Atom
	| LVAR Int
	| LCALL LExp LExp
	| Lλ Int LStmt
	| LDOT LExp LExp
	| LEQ LExp LExp
	| LTABLE(Tbl LExp)
	deriving (Show,Read)

data LStmt
	= LDO [LStmt]
	| LLET Int LExp
	| LIF LExp LStmt LStmt
	| LSET LExp LExp LExp
	| LRETURN LExp
	deriving (Show,Read)

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
