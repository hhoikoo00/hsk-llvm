module AST.Program (Program(..)) where

import           AST.Func (Func)

newtype Program = Program [Func]
