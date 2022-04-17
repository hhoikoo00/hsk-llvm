module AST.Stat (Stat(..), StatOne(..)) where

import           AST.Expr           (Expr, Ident)
import           AST.Type           (Type)
import qualified Data.List.NonEmpty as NE

newtype Stat = Stat (NE.NonEmpty StatOne)
  deriving (Eq, Show)

data StatOne = Skip
             | InitVar    Ident Type Expr
             | AssignVar  Ident Expr
             | Scan       Ident
             | Print      Expr
             | Exit       Expr
             | Return     Expr
             | If         Expr Stat Stat
             | While      Expr Stat
             | Do         Stat
  deriving (Eq, Show)
