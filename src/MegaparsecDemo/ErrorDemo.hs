{-# LANGUAGE OverloadedStrings #-}

module MegaparsecDemo.ErrorDemo () where

import           Data.Text (Text)
import           Data.Void (Void)


{-
data ParseError s e
  = TrivialError Int (Maybe (ErrorItem (Token s))) (Set (ErrorItem (Token s)))
    -- ^ Trivial errors, generated by Megaparsec's machinery. The data
    -- constructor includes the offset of error, unexpected token (if any),
    -- and expected tokens.
  | FancyError Int (Set (ErrorFancy e))
    -- ^ Fancy, custom errors.

* s is the type of input stream.
* e is the type of custom component of parse error.

data ErrorItem t
  = Tokens (NonEmpty t)      -- ^ Non-empty stream of tokens
  | Label (NonEmpty Char)    -- ^ Label (cannot be empty)
  | EndOfInput               -- ^ End of input

* NonEmpty is a type for non-empty lists, it comes from Data.List.NonEmpty.
-}

{-
fail "I'm failing, help me!" :: Parser ()

failure :: MonadParsec e s m
  => Maybe (ErrorItem (Token s)) -- ^ Unexpected item (if any)
  -> Set (ErrorItem (Token s)) -- ^ Expected items
  -> m a
unfortunateParser :: Parser ()
unfortunateParser = failure (Just EndOfInput) (Set.fromList es)
  where
    es = [Tokens (NE.fromList "a"), Tokens (NE.fromList "b")]
-}




{-
data ErrorFancy e
  = ErrorFail String
    -- ^ 'fail' has been used in parser monad
  | ErrorIndentation Ordering Pos Pos
    -- ^ Incorrect indentation error: desired ordering between reference
    -- level and actual level, reference indentation level, actual
    -- indentation level
  | ErrorCustom e
    -- ^ Custom error data, can be conveniently disabled by indexing
    -- 'ErrorFancy' by 'Void'

* When we do not need any custom data in our parse errors, we parametrize
  ErrorFancy by Void.
-}

{-
fancyFailure :: MonadParsec e s m
  => Set (ErrorFancy e) -- ^ Fancy error components
  -> m a

It is often desirable to define a helper like the one we have in the lexer
modules instead of calling fancyFailure directly:
incorrectIndent :: MonadParsec e s m
  => Ordering  -- ^ Desired ordering between reference level and actual level
  -> Pos               -- ^ Reference indentation level
  -> Pos               -- ^ Actual indentation level
  -> m a
incorrectIndent ord ref actual = fancyFailure . Set.singleton $
  ErrorIndentation ord ref actual
-}

{-
data Custom = NotKeyword Text
  deriving (Eq, Show, Ord)

instance ShowErrorComponent Custom where
  showErrorComponent (NotKeyword txt) = T.unpack txt ++ " is not a keyword"

type Parser = Parsec Custom Text

notKeyword :: Text -> Parser a
notKeyword = customFailure . NotKeyword

From the Text.Megaparsec module:
customFailure :: MonadParsec e s m => e -> m a
customFailure = fancyFailure . E.singleton . ErrorCustom
-}




-- Catching parse errors in a running parser
-- It is possible to “catch” a parse error, alter it in some way, and then
-- re-throw, just like with exceptions. This is enabled by the observing
-- primitive:

{-
-- | @'observing' p@ allows to “observe” failure of the @p@ parser, should
-- it happen, without actually ending parsing, but instead getting the
-- 'ParseError' in 'Left'. On success parsed value is returned in 'Right'
-- as usual. Note that this primitive just allows you to observe parse
-- errors as they happen, it does not backtrack or change how the @p@
-- parser works in any way.

observing :: MonadParsec e s m
  => m a             -- ^ The parser to run
  -> m (Either (ParseError (Token s) e) a)
-}

{- Complete program example
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Main (main) where

import Control.Applicative hiding (some)
import Data.List (intercalate)
import Data.Set (Set)
import Data.Text (Text)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Data.Set as Set

data Custom
  = TrivialWithLocation
    [String] -- position stack
    (Maybe (ErrorItem Char))
    (Set (ErrorItem Char))
  | FancyWithLocation
    [String] -- position stack
    (ErrorFancy Void) -- Void, because we do not want to allow to nest Customs
  deriving (Eq, Ord, Show)

instance ShowErrorComponent Custom where
  showErrorComponent (TrivialWithLocation stack us es) =
    parseErrorTextPretty (TrivialError @Text @Void undefined us es)
      ++ showPosStack stack
  showErrorComponent (FancyWithLocation stack cs) =
    parseErrorTextPretty (FancyError @Text @Void undefined (Set.singleton cs))
      ++ showPosStack stack

showPosStack :: [String] -> String
showPosStack = intercalate ", " . fmap ("in " ++)

type Parser = Parsec Custom Text

inside :: String -> Parser a -> Parser a
inside location p = do
  r <- observing p
  case r of
    Left (TrivialError _ us es) ->
      fancyFailure . Set.singleton . ErrorCustom $
        TrivialWithLocation [location] us es
    Left (FancyError _ xs) -> do
      let f (ErrorFail msg) = ErrorCustom $
            FancyWithLocation [location] (ErrorFail msg)
          f (ErrorIndentation ord rlvl alvl) = ErrorCustom $
            FancyWithLocation [location] (ErrorIndentation ord rlvl alvl)
          f (ErrorCustom (TrivialWithLocation ps us es)) = ErrorCustom $
            TrivialWithLocation (location:ps) us es
          f (ErrorCustom (FancyWithLocation ps cs)) = ErrorCustom $
            FancyWithLocation (location:ps) cs
      fancyFailure (Set.map f xs)
    Right x -> return x

myParser :: Parser String
myParser = some (char 'a') *> some (char 'b')

main :: IO ()
main = do
  parseTest (inside "foo" myParser) "aaacc"
  parseTest (inside "foo" $ inside "bar" myParser) "aaacc"
-}

{-Output
1:4:
  |
1 | aaacc
  |    ^
unexpected 'c'
expecting 'a' or 'b'
in foo
1:4:
  |
1 | aaacc
  |    ^
unexpected 'c'
expecting 'a' or 'b'
in foo, in bar
-}

{-
Thus, the feature can be used to attach location labels to parse errors, or
indeed define regions in which parse errors are processed in some way. The
idiom is quite useful, so there is even a non-primitive helper called region
defined in terms of the observing primitive:

-- | Specify how to process 'ParseError's that happen inside of this
-- wrapper. This applies to both normal and delayed 'ParseError's.
--
-- As a side-effect of the implementation the inner computation will start
-- with empty collection of delayed errors and they will be updated and
-- “restored” on the way out of 'region'.

region :: MonadParsec e s m
  => (ParseError s e -> ParseError s e)
     -- ^ How to process 'ParseError's
  -> m a
     -- ^ The “region” that the processing applies to
  -> m a
region f m = do
  r <- observing m
  case r of
    Left err -> parseError (f err) -- see the next section
    Right x -> return x
-}




{-
Fundamental primitive for error reporting:
parseError :: MonadParsec e s m => ParseError s e -> m a

All other functions we have seen so far are defined in terms of parseError:
failure
  :: MonadParsec e s m
  => Maybe (ErrorItem (Token s)) -- ^ Unexpected item (if any)
  -> Set (ErrorItem (Token s)) -- ^ Expected items
  -> m a
failure us ps = do
  o <- getOffset
  parseError (TrivialError o us ps)

fancyFailure
  :: MonadParsec e s m
  => Set (ErrorFancy e) -- ^ Fancy error components
  -> m a
fancyFailure xs = do
  o <- getOffset
  parseError (FancyError o xs)

Set error offset (that is, position) to something else:

withPredicate2
  :: (a -> Bool)       -- ^ The check to perform on parsed input
  -> String            -- ^ Message to print when the check fails
  -> Parser a          -- ^ Parser to run
  -> Parser a          -- ^ Resulting parser that performs the check
withPredicate2 f msg p = do
  o <- getOffset
  r <- p
  if f r
    then return r
    else do
      setOffset o
      fail msg

* setOffset o will make the error to be located correctly, but it will also
  invalidate the parser state as a side effect—the offset will not reflect
  reality anymore.

Solution: either use region to reset the parse error location, or use
parseError in the first place:
withPredicate3
  :: (a -> Bool)       -- ^ The check to perform on parsed input
  -> String            -- ^ Message to print when the check fails
  -> Parser a          -- ^ Parser to run
  -> Parser a          -- ^ Resulting parser that performs the check
withPredicate3 f msg p = do
  o <- getOffset
  r <- p
  if f r
    then return r
    else region (setErrorOffset o) (fail msg)
withPredicate4
  :: (a -> Bool)       -- ^ The check to perform on parsed input
  -> String            -- ^ Message to print when the check fails
  -> Parser a          -- ^ Parser to run
  -> Parser a          -- ^ Resulting parser that performs the check
withPredicate4 f msg p = do
  o <- getOffset
  r <- p
  if f r
    then return r
    else parseError (FancyError o (Set.singleton (ErrorFail msg)))
-}




-- Multiple Parse Errors.
{-
Skip over a problematic part of input and resume parsing from a position that
is known to be good.

-- | @'withRecovery' r p@ allows continue parsing even if parser @p@
-- fails. In this case @r@ is called with the actual 'ParseError' as its
-- argument. Typical usage is to return a value signifying failure to
-- parse this particular object and to consume some part of the input up
-- to the point where the next object starts.
--
-- Note that if @r@ fails, original error message is reported as if
-- without 'withRecovery'. In no way recovering parser @r@ can influence
-- error messages.

withRecovery
  :: (ParseError s e -> m a) -- ^ How to recover from failure
  -> m a             -- ^ Original parser
  -> m a             -- ^ Parser that can recover from failures

* Before Megaparsec 8 users had to pick the type a to be a sum type including
  the possibilities for success and failure. For example, it could be Either
  (ParseError s e) Result. The parse errors had to be collected and later
  manually added to the ParseErrorBundle before displaying.
-}

{-
Megaparsec 8 supports delayed parse errors:

-- | Register a 'ParseError' for later reporting. This action does not end
-- parsing and has no effect except for adding the given 'ParseError' to the
-- collection of “delayed” 'ParseError's which will be taken into
-- consideration at the end of parsing. Only if this collection is empty
-- parser will succeed. This is the main way to report several parse errors
-- at once.

registerParseError :: MonadParsec e s m => ParseError s e -> m ()

-- | Like 'failure', but for delayed 'ParseError's.

registerFailure
  :: MonadParsec e s m
  => Maybe (ErrorItem (Token s)) -- ^ Unexpected item (if any)
  -> Set (ErrorItem (Token s)) -- ^ Expected items
  -> m ()

-- | Like 'fancyFailure', but for delayed 'ParseError's.

registerFancyFailure
  :: MonadParsec e s m
  => Set (ErrorFancy e) -- ^ Fancy error components
  -> m ()

* These errors can be registered in the error-processing callback of
  withRecovery making the resulting type Maybe Result. This takes care of
  including the delayed errors in the final ParseErrorBundle as well as making
  the parser fail in the end if the collection of delayed errors in not empty.
-}
