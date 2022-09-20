{-# LANGUAGE OverloadedStrings #-}

module MegaparsecDemo.ExprDemo (run) where

import           Control.Monad                  (forM_)
import           Control.Monad.Combinators.Expr (Operator (InfixL, Postfix, Prefix),
                                                 makeExprParser)
import           Data.Text                      (Text)
import           Data.Void                      (Void)
import           MegaparsecDemo.LexerDemo       (lexeme, symbol)
import           Text.Megaparsec                (MonadParsec (eof), Parsec,
                                                 between, choice,
                                                 errorBundlePretty, many,
                                                 parseTest, (<?>))
import           Text.Megaparsec.Char           (alphaNumChar, letterChar)
import qualified Text.Megaparsec.Char.Lexer     as L

data Expr
  = Var String
  | Int Int
  | Negation Expr
  | Sum      Expr Expr
  | Subtr    Expr Expr
  | Product  Expr Expr
  | Division Expr Expr
  deriving (Eq, Ord, Show)

type Parser = Parsec Void Text

-- makeExprParser :: MonadParsec e s m
--   => m a               -- ^ Term parser
--   -> [[Operator m a]]  -- ^ Operator table, see 'Operator'
--   -> m a               -- ^ Resulting expression parser

-- term => a box that that is to be considered as an indivisible whole by the
-- expression parsing algorithm when it works with things like associativity
-- and precedence

pVariable :: Parser Expr
pVariable = Var <$> lexeme
  ((:) <$> letterChar <*> many alphaNumChar <?> "variable")

pInteger :: Parser Expr
pInteger = Int <$> lexeme L.decimal

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

pTerm :: Parser Expr
pTerm = choice
  [ parens pExpr
  , pVariable
  , pInteger
  ]

pExpr :: Parser Expr
pExpr = makeExprParser pTerm operatorTable

-- data Operator m a -- N.B.
--   = InfixN  (m (a -> a -> a)) -- ^ Non-associative infix
--   | InfixL  (m (a -> a -> a)) -- ^ Left-associative infix
--   | InfixR  (m (a -> a -> a)) -- ^ Right-associative infix
--   | Prefix  (m (a -> a))      -- ^ Prefix
--   | Postfix (m (a -> a))      -- ^ Postfix

operatorTable :: [[Operator Parser Expr]]
operatorTable =
  [ [ prefix "-" Negation
    , prefix "+" id
    ]
  , [ binary "*" Product
    , binary "/" Division
    ]
  , [ binary "+" Sum
    , binary "-" Subtr
    ]
  ]

binary :: Text -> (Expr -> Expr -> Expr) -> Operator Parser Expr
binary  name f = InfixL  (f <$ symbol name <?> "operator")

prefix, postfix :: Text -> (Expr -> Expr) -> Operator Parser Expr
prefix  name f = Prefix  (f <$ symbol name <?> "operator")
postfix name f = Postfix (f <$ symbol name <?> "operator")


run :: IO ()
run = do
  let { testCases =
    [ "a * (b + 2)"
    , "a * b + 2"
    , "a * b / 2"
    , "a * (b $ 2)"
    ]};

  forM_ testCases $ \test -> do
    parseTest (pExpr <* eof) test
