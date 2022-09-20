{-# LANGUAGE OverloadedStrings #-}

module MegaparsecDemo.LexerDemo where

import           Data.Text                  (Text)
import           Data.Void                  (Void)
import           Text.Megaparsec            (Parsec, between, empty, manyTill,
                                             notFollowedBy)
import           Text.Megaparsec.Char       (alphaNumChar, char, space1, string)
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

sc :: Parser ()
sc =
  L.space
    space1
    (L.skipLineComment "//")
    empty -- (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

charLiteral :: Parser Char
charLiteral = between (char '\'') (char '\'') L.charLiteral

stringLiteral :: Parser String
stringLiteral = char '\"' *> manyTill L.charLiteral (char '\"')

-- manyTill :: Alternative m => m a -> m end -> m [a]
-- manyTill p end = go
--   where
--     go = ([] <$ end) <|> ((:) <$> p <*> go)

-- L.decimal, L.octal, L.hexadecimal
--   :: (MonadParsec e s m, Token s ~ Char, Num a) => m a
integer :: Parser Integer
integer = lexeme L.decimal

-- L.scientific :: (MonadParsec e s m, Token s ~ Char)
--                  => m Scientific
-- L.float      :: (MonadParsec e s m, Token s ~ Char, RealFloat a)
--                  => m a

signedInteger :: Parser Integer
signedInteger = L.signed sc integer

-- notFollowedBy :: MonadParsec e s m => m a -> m ()

pKeyword :: Text -> Parser Text
pKeyword keyword = lexeme (string keyword <* notFollowedBy alphaNumChar)

-- what if the keyword we are matching against is just a prefix of an
-- identifier? => must eliminate this case.

-- lookAhead :: MonadParsec e s m => m a -> m a
-- If the argument p of lookAhead succeeds, the whole construct lookAhead p
-- also succeeds but the input stream (and the entire parser state) stays
-- untouched, i.e. nothing is consumed.

-- withPredicate1
--   :: (a -> Bool)       -- ^ The check to perform on parsed input
--   -> String            -- ^ Message to print when the check fails
--   -> Parser a          -- ^ Parser to run
--   -> Parser a          -- ^ Resulting parser that performs the check
-- withPredicate1 f msg p = do
--   r <- lookAhead p
--   if f r
--     then p
--     else fail msg

run :: IO ()
run = undefined
