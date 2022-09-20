{-# LANGUAGE OverloadedStrings #-}

module MegaparsecDemo.TestDemo where

import           Control.Applicative           hiding (some)
import           Data.Text                     (Text)
import           Data.Void                     (Void)
import           Test.Hspec                    (describe, hspec, it)
import           Test.Hspec.Megaparsec         (failsLeaving, initialState,
                                                parseSatisfies, shouldFailOn,
                                                shouldFailWith, shouldParse,
                                                shouldSucceedOn,
                                                succeedsLeaving)
import           Text.Megaparsec               (Parsec, parse, runParser', some)
import           Text.Megaparsec.Char          (char)
import           Text.Megaparsec.Error.Builder (err, etok, utok)

type Parser = Parsec Void Text

myParser :: Parser String
myParser = some (char 'a')

main :: IO ()
main = hspec $
  describe "myParser" $ do
    it "returns correct result" $
      parse myParser "" "aaa" `shouldParse` "aaa"
    it "result of parsing satisfies what it should" $
      parse myParser "" "aaaa" `parseSatisfies` ((== 4) . length)

    it "should parse 'a's all right" $
      parse myParser "" `shouldSucceedOn` "aaaa"
    it "should fail on 'b's" $
      parse myParser "" `shouldFailOn` "bbb"

    it "fails on 'b's producing correct error message" $
      parse myParser "" "bbb" `shouldFailWith` err 0 (utok 'b' <> etok 'a')
      -- The first argument of err is offset of the parse error (the number of
      -- tokens that had been consumed before we got the error). In this
      -- example it is simply 0.
      -- utok stands for “unexpected token”, similarly etok means “expected
      -- token”.

    it "consumes all 'a's but does not touch 'b's" $
      runParser' myParser (initialState "aaabbb") `succeedsLeaving` "bbb"
    it "fails without consuming anything" $
      runParser' myParser (initialState "bbbccc") `failsLeaving` "bbbccc"


