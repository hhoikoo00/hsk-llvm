{-# LANGUAGE OverloadedStrings #-}

module Parser.LexerSpec (spec) where

import           Parser.Lexer          (pKeyword, pOp)
import           Test.Hspec            (Spec, describe, it)
import           Test.Hspec.Megaparsec (shouldParse)
import           Text.Megaparsec       (parse)


testFileName :: String
testFileName = "LexerTest"


pKeywordSpec :: Spec
pKeywordSpec = describe "pKeyword" $ do
    let pIf = pKeyword "if"

    it "parses valid keyword without whitespace correctly" $ do
        parse pIf testFileName "if" `shouldParse` "if"


pOpSpec :: Spec
pOpSpec = describe "pOp" $ do
    let pPlus = pOp "+"

    it "parses valid operator without whitespace correctly" $ do
        parse pPlus testFileName "+" `shouldParse` "+"


spec :: Spec
spec = describe "Parser.Lexer" $ do
    pKeywordSpec
    pOpSpec
