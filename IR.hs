module IR where
import Prim

-- Atomic Data Types
data SExp = SATOM Atom | STABLE(Tbl SExp) deriving(Show,Read,Eq,Ord)
data Pattern = PSYM String | PATOM Atom | PTBL(Tbl Pattern)
	deriving(Show,Read,Eq,Ord)

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
	deriving(Show,Read,Eq,Ord)

-- Lower-level representation.
data LExp
	= LATOM Atom
	| LVAR Int
	| LCALL Int Int
	| LGET Int Int
	| LGLOBAL String
	| LFOREIGN_METHOD Int String [Int]
	| LFOREIGN_CALL Int [Int]
	| Lλ Int LStmt
	| LTABLE(Tbl Int)
	| LFFI String
	deriving (Show,Read,Eq,Ord)

data LStmt
	= LDO [LStmt]
	| LBIND Int
	| LASSIGN Int LExp
	| LIF Int LStmt LStmt
	| LFOREIGN_DIRECTIVE String
	| LRETURN LExp
	deriving (Show,Read,Eq,Ord)

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
