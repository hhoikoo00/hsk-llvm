{-# LANGUAGE OverloadedStrings #-}

module Parser.ExprParser (pExpr) where

import           AST.Expr                       (Args (..), BOp (..), Expr (..),
                                                 Ident (..), UOp (..))
import           Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import           Parser.Lexer                   (pBoolLiteral, pCharLiteral,
                                                 pIdent, pIntLiteral, pOp,
                                                 pStrLiteral)
import           Parser.ParserType              (Parser)
import           Text.Megaparsec                (MonadParsec (label), between,
                                                 choice, optional, sepBy)

pExpr :: Parser Expr
pExpr = label "Expression" $ makeExprParser pTerm operatorTable

pTerm :: Parser Expr
pTerm = choice
  [ LInt          <$> pIntLiteral
  , LBool         <$> pBoolLiteral
  , LChar         <$> pCharLiteral
  , LStr          <$> pStrLiteral
  , varOrFuncCall <$> (I <$> pIdent)  <*> optional pArgs
  , Parens        <$> pBracket pExpr
  ]
  where varOrFuncCall :: Ident -> Maybe Args -> Expr
        varOrFuncCall id (Just args) = FuncCall id args
        varOrFuncCall id Nothing     = Var id

        pArgs :: Parser Args
        pArgs = pBracket $ Args <$> sepBy pExpr (pOp ",")

operatorTable :: [[Operator Parser Expr]]
operatorTable =
  [ [ prefix "-"    (UnaryOp Negate)
    , prefix "not"  (UnaryOp Not)
    , prefix "len"  (UnaryOp Len)
    , prefix "ord"  (UnaryOp Ord)
    , prefix "chr"  (UnaryOp Chr)
    ]
  , [ binary "*"    (BinaryOp Mult)
    , binary "/"    (BinaryOp Div)
    , binary "mod"  (BinaryOp Mod)
    ]
  , [ binary "+"    (BinaryOp Add)
    , binary "-"    (BinaryOp Sub)
    ]
  , [ binary ">"    (BinaryOp Gt)
    , binary ">="   (BinaryOp Gte)
    , binary "<"    (BinaryOp Lt)
    , binary "<="   (BinaryOp Lte)
    ]
  , [ binary "=="   (BinaryOp Eq)
    , binary "!="   (BinaryOp Neq)
    ]
  , [ binary "and"  (BinaryOp And)
    ]
  , [ binary "or"   (BinaryOp Or)
    ]
  ]
  where binary name f = InfixL  (f <$ pOp name)
        prefix name f = Prefix  (f <$ pOp name)

pBracket :: Parser a -> Parser a
pBracket = between (pOp "(") (pOp ")")
