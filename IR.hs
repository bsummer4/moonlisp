module IR where
import Prim

-- Atomic Data Types
data SExp = SATOM Atom | STABLE(Tbl SExp) deriving(Show,Eq,Ord)
data Pattern = PSYM String | PATOM Atom | PTBL(Tbl Pattern)
	deriving(Show,Eq,Ord)

-- [[WARNING]] ‘RETURN’ is for internal use only! [[WARNING]]
data Exp
	= ATOM Atom
	| VAR String
	| DO [Exp]
	| Λ String Exp
	| MATCH Exp [(Pattern,Exp)]
	| CALL Exp Exp
	| DATA(Tbl Exp)
	| SYNTAX (Tbl Exp)
	| FFUNC String
	| FSTMT String
	| RETURN Exp
	deriving(Show,Eq,Ord)

-- Lower-level representation.
data LExp
	= LATOM Atom
	| LVAR Int
	| LCALL LExp LExp
	| Lλ Int LStmt
	| LTABLE(Tbl LExp)
	deriving (Show)

data LStmt
	= LDO [LStmt]
	| LLET Int LExp
	| LIF LExp LStmt LStmt
	| LRETURN LExp
	deriving (Show)

-- Code Generation
data CExp = CExp DelimTy CExp1 deriving (Show)
data DelimTy = Unsafe | Space | Safe deriving (Show)
data CExp1
	= CATOM String
	| CSTMT String CExp
	| CBLOCK (String,String) [CExp]
	| CTUPLE (String,String) [CExp]
	| CBINOP CExp String CExp
	| CTRIOP CExp String CExp String CExp
	| CSEMI CExp
	deriving (Show)
