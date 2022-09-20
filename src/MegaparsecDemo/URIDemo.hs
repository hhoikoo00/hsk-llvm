{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module MegaparsecDemo.URIDemo (run) where

import           Control.Applicative        (Alternative ((<|>)), optional)
import           Control.Monad              (forM_, void)
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Data.Void                  (Void)
import           Text.Megaparsec            (MonadParsec (label, try), Parsec,
                                             choice, errorBundlePretty,
                                             optional, runParser, some, (<?>),
                                             (<|>))
import           Text.Megaparsec.Char       (alphaNumChar, char, string)
import qualified Text.Megaparsec.Char.Lexer as L

-- Handle parsing of the following format:
--    scheme:[//[user:password@]host[:port]][/]path[?query][#fragment]
--                                          ^
--                                          | unhandled from this point

type Parser = Parsec Void Text
--                   ^    ^
--                   |    |
-- Custom error component Type of input stream

--
-- Data Types
--

data Scheme
  = SchemeData
  | SchemeFile
  | SchemeFtp
  | SchemeHttp
  | SchemeHttps
  | SchemeIrc
  | SchemeMailto
  deriving (Eq, Show)

data Uri = Uri
  { uriScheme    :: Scheme
  , uriAuthority :: Maybe Authority
  } deriving (Eq, Show)

data Authority = Authority
  { authUser :: Maybe (Text, Text) -- (user, password)
  , authHost :: Text
  , authPort :: Maybe Int
  } deriving (Eq, Show)

--
-- Parsers
--

pScheme :: Parser Scheme
-- pScheme = string "data" <|> string "file" <|> string "ftp" <|> string "http"
--       <|> string "https" <|> string "irc" <|> string "mailto
pScheme = choice
  [ SchemeData   <$ string "data"
  , SchemeFile   <$ string "file"
  , SchemeFtp    <$ string "ftp"
  , SchemeHttps  <$ string "https"
  , SchemeHttp   <$ string "http"
  , SchemeIrc    <$ string "irc"
  , SchemeMailto <$ string "mailto" ]

pUri_1 :: Parser Uri
pUri_1 = do
  uriScheme <- pScheme
  void (char ':')
  uriAuthority <- optional $ do                  -- (1)
    void (string "//")
    authUser <- optional . try $ do              -- (2)
      user <- T.pack <$> some alphaNumChar       -- (3)
      void (char ':')
      password <- T.pack <$> some alphaNumChar
      void (char '@')
      return (user, password)
    authHost <- T.pack <$> some (alphaNumChar <|> char '.')
    authPort <- optional (char ':' *> L.decimal) -- (4)
    return Authority {..}                        -- (5)
  return Uri {..}                                -- (6)
-- In (1) and (2) we need to wrap the argument of optional with try because it
--   is a composite parser, not a primitive.
-- For (1), however, as once "//" is parsed we know we are in the Authority part
--   of the text, we don't need to backtrack if that parsing fails.
-- (3) some is just like many, but demands that its argument parser matches at
--   least once: some p = (:) <$> p <*> many p.
-- (4) Do not use try unless necessary! Here if char ':' succeeds (which is by
--   itself built on top of token, so it does not need a try), we know for sure
--   that port must follow after it, so we just demand a decimal number with L.
--   decimal. After matching :, we are committed and do not need a way to go
--   back.
-- In (5) and (6) we assemble Authority and Uri values using the
--   RecordWildCards language extension.
-- void :: Functor f => f a -> f () is used to explicitly discard the result to
--   parsing, without it we would get warnings about unused values from GHC.

pUriWithError :: Parser Uri
pUriWithError = do
  uriScheme <- pScheme <?> "valid scheme"
  void (char ':')
  uriAuthority <- optional $ do
    void (string "//")
    authUser <- optional . try $ do
      user <- T.pack <$> some alphaNumChar <?> "username"
      void (char ':')
      password <- T.pack <$> some alphaNumChar <?> "password"
      void (char '@')
      return (user, password)
    authHost <- T.pack <$> some (alphaNumChar <|> char '.') <?> "hostname"
    authPort <- optional (char ':' *> label "port number" L.decimal)
    return Authority {..}
  return Uri {..}

run :: IO ()
run = do
  let p = runParser pUriWithError "Demo.file"
      validTestCases =    [ "https://mark:secret@example.com"
                          , "https://mark:secret@example.com:123"
                          , "https://example.com:123"
                          ]
      invalidTestCases =  [ "https://mark@example.com:123"
                          , "foo://example.com"
                          , "https://mark:@example.com"
                          ]

  forM_ (validTestCases ++ invalidTestCases) $ \input -> do
    case p input of
      Left error -> putStrLn (errorBundlePretty error)
      Right uri  -> print uri
