{-# LANGUAGE OverloadedStrings #-}

module Parser.Lexer (pKeyword, pOp, pIdent, pIntLiteral, pBoolLiteral, pCharLiteral, pStrLiteral) where

import           Data.Char                  (isAlphaNum)
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Parser.ParserType          (Parser)
import           Text.Megaparsec            (MonadParsec (try), between, choice,
                                             empty, label, manyTill,
                                             notFollowedBy, satisfy, takeWhileP,
                                             (<?>), (<|>))
import           Text.Megaparsec.Char       (alphaNumChar, char, letterChar,
                                             punctuationChar, space1, string)
import qualified Text.Megaparsec.Char.Lexer as L

sc :: Parser ()
sc = L.space              -- Space Consumer Parser
  space1                  -- Non-empty whitespace parser
  (L.skipLineComment "#") -- Line comment parser
  empty                   -- Block comment parser - empty for ToyLang

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

pKeyword :: Text -> Parser Text
pKeyword kwd = lexeme $ try $ string kwd <* notFollowedBy alphaNumChar

pOp :: Text -> Parser Text
pOp op = symbol op <* notFollowedBy punctuationChar

pIdent :: Parser String
pIdent = label "Identifier" $ lexeme $ do
  firstChar <- letterChar <|> char '_'
  -- `takeWhileP` used over `many` for performance.
  restChars <- T.unpack <$> takeWhileP Nothing (\c -> isAlphaNum c || c == '_')
  return (firstChar : restChars)

pIntLiteral :: Parser Integer
pIntLiteral = label "Integer Literal" $ lexeme $
  L.signed sc (lexeme L.decimal)

pBoolLiteral :: Parser Bool
pBoolLiteral = label "Boolean literal" $ choice
  [ True <$ pKeyword "true"
  , False <$ pKeyword "false"
  ]

pChar :: Parser Char
pChar = unescapedChar <|> escapedChar
  where
    -- `satisfy` used over `noneOf` for performance.
    unescapedChar :: Parser Char
    unescapedChar = satisfy (\c -> c /= '\\' && c /= '\'' && c /= '\"')

    escapedChar :: Parser Char
    escapedChar = char '\\' *> choice
      [ '\NUL'  <$  char '0'
      , '\t'    <$  char 't'
      , '\n'    <$  char 'n'
      , '\r'    <$  char 'r'
      ,             char '\"'
      ,             char '\''
      ,             char '\\'
      ]

pCharLiteral :: Parser Char
pCharLiteral = label "Character literal" $ lexeme $
  between (char '\'') (char '\'') pChar

pStrLiteral :: Parser String
pStrLiteral = label "String literal" $ lexeme $
  char '\"' *> manyTill pChar (char '\"')
