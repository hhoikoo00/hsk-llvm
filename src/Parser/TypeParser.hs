{-# LANGUAGE OverloadedStrings #-}

module Parser.TypeParser (pType) where

import           AST.Type        (Type (..))
import           Parser.Lexer    (pKeyword)
import           Parser.Parser   (Parser)
import           Text.Megaparsec (choice, MonadParsec (label))

pType :: Parser Type
pType = label "Type" $ choice
  [ TInt    <$ pKeyword "int"
  , TBool   <$ pKeyword "bool"
  , TChar   <$ pKeyword "char"
  , TString <$ pKeyword "string"
  ]
