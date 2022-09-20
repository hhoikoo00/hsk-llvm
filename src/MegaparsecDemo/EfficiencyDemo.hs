{-# LANGUAGE OverloadedStrings #-}

module MegaparsecDemo.EfficiencyDemo (run) where

import           Data.Char            (isAlphaNum)
import           Data.Text            (Text)
import qualified Data.Text            as T
import           Data.Void            (Void)
import           Text.Megaparsec      (Parsec, some, takeWhile1P)
import           Text.Megaparsec.Char (alphaNumChar)

type Parser = Parsec Void Text

-- Inline generously, especially for short functions. This is especially true
-- for parsers that are defined in one module and used in another one
--   -> INLINE and INLINEABLE pragmas make GHC dump function definitions into
--   interface files and this facilitates specializing.


-- Use the fast primitives such as takeWhileP, takeWhile1P, and takeP
-- whenever you can.
-- Replaces many and some.
pDemoTakeWhileP :: Parser ()
pDemoTakeWhileP = do
  user <- T.pack <$> some alphaNumChar
  user <- takeWhile1P (Just "alpha num character") isAlphaNum
  --                  ^                            ^
  --                  |                            |
  -- label for tokens we match against         predicate
  return ()

-- takeWhileP  (Just "foo") f = many (satisfy f <?> "foo")
-- takeWhileP  Nothing      f = many (satisfy f)
-- takeWhile1P (Just "foo") f = some (satisfy f <?> "foo")
-- takeWhile1P Nothing      f = some (satisfy f)

-- Avoid oneOf and noneOf preferring satisfy and notChar whenever possible.


run :: IO ()
run = undefined
