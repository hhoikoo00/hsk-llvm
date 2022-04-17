module AST.Expr (Expr(..), Ident(..), Args(..), UOp(..), BOp(..)) where

data Expr = LInt      Integer
          | LBool     Bool
          | LChar     Char
          | LStr      String
          | Var       Ident
          | FuncCall  Ident   Args
          | UnaryOp   UOp     Expr
          | BinaryOp  BOp     Expr    Expr
          | Parens    Expr
  deriving (Eq, Show)

newtype Ident = I     String    deriving (Eq, Show)
newtype Args  = Args  [Expr]    deriving (Eq, Show)

data UOp = Negate | Not | Len | Ord | Chr
  deriving (Eq, Show)

data BOp  = Mult | Div | Add | Sub | Mod
          | Gt | Gte | Lt | Lte
          | Eq | Neq
          | And | Or
  deriving (Eq, Show)
