{-# LANGUAGE OverloadedStrings #-}
module MegaparsecDemo.Demo (run) where

import           Control.Monad        (forM_)
import           Data.Text            (Text)
import           Data.Void            (Void)
import           Text.Megaparsec      (MonadParsec (eof), Parsec, many,
                                       parseTest, satisfy)
import           Text.Megaparsec.Char (char, string, string')

type Parser = Parsec Void Text
--                   ^    ^
--                   |    |
-- Custom error component Type of input stream

basicParsers :: IO ()
basicParsers = do
  forM_ ["", "a", "b"] $
    parseTest (satisfy (== 'a') :: Parser Char)
  forM_ ["a", "d"] $
    parseTest (satisfy (> 'c') :: Parser Char)

  forM_ ["a", "d"] $
    parseTest (char 'a' :: Parser Char)
  -- newline = single '\n'

  forM_ ["foo", "bar"] $
    parseTest (string "foo" :: Parser Text)
  forM_ ["fOO", "FOO", "foz"] $
    parseTest (string' "foo" :: Parser Text)

mySequenceMonad :: Parser (Char, Char, Char)
mySequenceMonad = do
  a <- char 'a'
  b <- char 'b'
  c <- char 'c'
  return (a, b, c)

mySequenceApplicative :: Parser (Char, Char, Char)
mySequenceApplicative =
  (,,) <$> char 'a'
       <*> char 'b'
       <*> char 'c'

monadAppParsers :: IO ()
monadAppParsers = do
  forM_ ["abc", "bcd", "adc"] $
    parseTest mySequenceMonad
  forM_ ["abc", "bcd", "adc"] $
    parseTest mySequenceApplicative

manyEofParsers :: IO ()
manyEofParsers = do
  forM_ ["aaa", "aabb"] $
    parseTest (many (char 'a') :: Parser [Char])

  forM_ ["aaa", "aabb"] $
    parseTest (many (char 'a') <* eof :: Parser [Char])

run :: IO ()
run = do
  manyEofParsers
