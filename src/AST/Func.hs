module AST.Func (Func(..), ParamList(..)) where

import           AST.Expr (Ident)
import           AST.Stat (Stat)
import           AST.Type (Type)

data Func = Func Ident ParamList Type Stat

newtype ParamList = ParamList [(Ident, Type)]
