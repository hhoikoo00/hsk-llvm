module AST.Type (Type(..)) where

data Type = TInt
          | TBool
          | TChar
          | TString
  deriving (Eq, Show)
