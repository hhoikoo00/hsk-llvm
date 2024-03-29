module Parser.ParserType (Parser) where

import           Data.Text       (Text)
import           Data.Void       (Void)
import           Text.Megaparsec (Parsec)

type Parser = Parsec Void Text
