{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- | This package provides the ability to run a Checklist of several
-- "checks" during a single test.  A "bad" check does not immediately
-- result in a test failure; at the end of the test (passed or failed
-- due to primary testing), all failed checks are reported (and any
-- failed checks will result in an overall test failure at the end.
--
-- This type of checking can be very useful when needing to test
-- various aspects of an operation that is complex to setup, has
-- multiple effects, or where the checks are related such that knowing
-- about the multiple failures makes debugging easier.
--
-- An alternative approach is to have some sort of common preparation
-- code and use a separate test for each item.  This module simply
-- provides a convenient method to collate related items under the
-- aegis of a single test.
--
-- This package also provides the 'checkValues' function which can be
-- used to check a number of derived values from a single input value
-- via a checklist.  This can be used to independently verify a number
-- of record fields of a data structure or to validate related
-- operations performed from a single input.
--
-- See the documentation for 'check' and 'checkValues' for examples of
-- using this library.  The tests in the source package also provide
-- additional examples of usage.

module Test.Tasty.Checklist
  (
    withChecklist
  , CanCheck
  , check
  , discardCheck
  , checkValues
  , DerivedVal(Val)
  -- * Error reporting
  , ChecklistFailures
  -- * Displaying tested values
  , TestShow(testShow)
  , testShowList
  )
where

import           Control.Exception ( evaluate )
import           Control.Monad ( unless )
import           Control.Monad.Catch
import           Control.Monad.IO.Class ( MonadIO, liftIO )
import           Data.IORef
import qualified Data.List as List
import qualified Data.Parameterized.Context as Ctx
import           Data.Text ( Text )
import qualified Data.Text as T
import           System.IO ( hFlush, hPutStrLn, stdout, stderr )


-- | The ChecklistFailures exception is thrown if any checks have
-- failed during testing.

data ChecklistFailures = ChecklistFailures Text [CheckResult]

-- | The 'CheckResult' captures the failure information for a check

data CheckResult = CheckFailed Text Text

instance Exception ChecklistFailures

instance Show CheckResult where
  show (CheckFailed what val) =
    "Failed check of " <> T.unpack what <> " against " <> show val

instance Show ChecklistFailures where
  show (ChecklistFailures topMsg fails) =
    "ERROR: " <> T.unpack topMsg <> "\n  " <>
    show (length fails) <> " checks failed in this checklist:\n  ↪" <>
    List.intercalate "\n  ↪" (show <$> fails)

-- | A convenient Constraint to apply to functions that will perform
-- checks (i.e. call 'check' one or more times)

type CanCheck = (?checker :: IORef [CheckResult])


-- | This should be used to wrap the test that contains checks.  This
-- initializes the environment needed for the checks to run, and on
-- exit from the test, reports any (and all) failed checks as a test
-- failure.

withChecklist :: (MonadIO m, MonadMask m)
              => Text -> (CanCheck => m a) -> m a
withChecklist topMsg t = do
  checks <- liftIO $ newIORef mempty
  r <- (let ?checker = checks in t)
       `onException` (liftIO $
                       do cs <- List.reverse <$> readIORef checks
                          unless (null cs) $ do
                            hFlush stdout
                            hPutStrLn stderr ""
                            let pfx = "        ⚠ "
                            mapM_ (hPutStrLn stderr . (pfx <>) . show) cs
                            hFlush stderr
                     )

  -- If t failed, never get here:
  liftIO $ do
    collected <- List.reverse <$> readIORef checks
    unless (null collected) $
      throwM (ChecklistFailures topMsg collected)
  return r

-- | This is used to run a check within the code.  The first argument
-- is the "name" of this check, the second is a function that takes a
-- value and returns 'True' if the value is OK, or 'False' if the
-- value fails the check.  The last argument is the value to check.
--
-- >>> :set -XOverloadedStrings
-- >>> :{
-- >>> defaultMain $ testCase "odd numbers" $ withChecklist "odds" $ do
-- >>>  let three = 3 :: Int
-- >>>  check "three is odd" odd three
-- >>>  check "two is odd" odd (2 :: Int)
-- >>>  check "7 + 3 is odd" odd $ 7 + three
-- >>>  check "7 is odd" odd (7 :: Int)
-- >>> :}
-- tst1: FAIL
--   Exception: ERROR: numbers
--     2 checks failed in this checklist:
--     ↪Failed check of "two is odd" with "2"
--     ↪Failed check of "7 + 3 is odd" with "10"
--
-- Any check failures are also printed to stdout (and omitted from the
-- above for clarity).  This is so that those failures are reported
-- even if a more standard test assertion is used that prevents
-- completion of the checklist.  Thus, if an `assertEqual "values"
-- three 7` had been added to the above, that would have been the only
-- actual (and immediate) fail for the test, but any failing 'check's
-- appearing before that 'assertEqual' would still have printed.

check :: (CanCheck, TestShow a, MonadIO m)
      => Text -> (a -> Bool) -> a -> m ()
check what eval val = do
  r <- liftIO $ evaluate (eval val)
  unless r $ do
    let chk = CheckFailed what $ T.pack $ testShow val
    liftIO $ modifyIORef ?checker (chk:)


-- | Sometimes checks are provided in common testing code, often in
-- setup/preparation for the main tests.  In some cases, the check is
-- not applicable for that particular test.  This function can be used
-- to discard any pending failures for the associated named check.
--
-- This is especially useful when a common code block is used to
-- perform a set of checks: if a few of the common checks are not
-- appropriate for the current situation, 'discardCheck' can be used
-- to throw away the results of those checks by matching on the check
-- name.

discardCheck :: (CanCheck, MonadIO m) => Text -> m ()
discardCheck what = do
  let isCheck n (CheckFailed n' _) = n == n'
  liftIO $ modifyIORef ?checker (filter (not . isCheck what))

----------------------------------------------------------------------

-- | The 'checkValues' is a checklist that tests various values that
-- can be derived from the input value.  The input value is provided,
-- along with an 'Data.Parameterized.Context.Assignment' list of
-- extraction functions and the expected result value (and name) of
-- that extraction.  Each extraction is performed as a check within
-- the checklist.
--
-- This is convenient to gather together a number of validations on a
-- single datatype and represent them economically.
--
-- One example is testing the fields of a record structure:
--
-- > {-# LANGUAGE PatternSynonyms, OverloadedStrings #-}
-- >
-- > import Data.Parameterized.Context ( pattern Empty, pattern (:>) )
-- > import Test.Tasty.Checklist
-- >
-- > data Struct = MyStruct { foo :: Int, bar :: Char, baz :: String }
-- >
-- > instance Show Struct where
-- >    show s = baz s <> " is " <> foo s <> bar s
-- >
-- > someFun :: Int -> Struct
-- > someFun n = MyStruct (n * 6)
-- >               (if n * 6 == 42 then '!' else '?')
-- >               "The answer to the universe"
-- >
-- > oddAnswer :: Struct -> Bool
-- > oddAnswer = odd . foo
-- >
-- > test = testCase "someFun result" $
-- >    someFun 3 `checkValues`
-- >         (Empty
-- >         :> Val "foo" foo 42
-- >         :> Val "baz field" baz "The answer to the universe"
-- >         :> Val "shown" show "The answer to the universe is 42!"
-- >         :> Val "odd answer" oddAnswer False
-- >         :> Val "double-checking foo" foo 42
-- >         )
--
-- Running this test:
--
-- >>> defaultMain test
-- ERROR: on input "The answer to the universe is 18?"
--   2 checks failed:
--   ↪Failed check of "foo" with "42"
--   ↪Failed check of "shown" with "The answer to the universe is 42!"
--
-- In this case, several of the values checked were correct, but more
-- than one was wrong.  Helpfully, this test output lists /all/ the
-- wrong answers for the single input provided.

checkValues :: CanCheck
            => TestShow dType
            => dType -> Ctx.Assignment (DerivedVal dType) idx ->  IO ()
checkValues got expF = Ctx.traverseWithIndex_ (chkValue got) expF


chkValue :: CanCheck
         => TestShow dType
         => dType -> Ctx.Index idx valType -> DerivedVal dType valType -> IO ()
chkValue got _idx (Val txt fld v) =
  check (txt <> " on input «" <> T.pack (testShow got) <> "»") (fld got ==) v


-- | Each entry in the 'Data.Parameterized.Context.Assignment' list
-- for 'checkValues' should be one of these 'DerivedVal' values.

data DerivedVal i d where
  Val :: (TestShow d, Eq d) => Text -> (i -> d) -> d -> DerivedVal i d

----------------------------------------------------------------------

-- | The 'TestShow' class is defined to provide a way for the various
-- data objects tested by this module to be displayed when tests fail.
-- The default 'testShow' will use a 'Show' instance, but this can be
-- overridden if there are alternate ways t o display a particular
-- object (e.g. pretty-printing, etc.)

class TestShow v where
  testShow :: v -> String
  default testShow :: Show v => v -> String
  testShow = show

-- Some TestShow instances using Show for regular datatypes
instance TestShow ()
instance TestShow Bool
instance TestShow Int
instance TestShow Integer
instance TestShow Float
instance TestShow Char
instance TestShow String

instance (TestShow a, TestShow b) => TestShow (a,b) where
  testShow (a,b) = "(" <> testShow a <> ", " <> testShow b <> ")"
instance (TestShow a, TestShow b, TestShow c) => TestShow (a,b,c) where
  testShow (a,b,c) = "(" <> testShow a <> ", " <> testShow b <> ", " <> testShow c <> ")"

-- | A helper function for defining a testShow for lists of items.
--
-- > instance TestShow [Int] where testShow = testShowList

testShowList :: TestShow v => [v] -> String
testShowList  l = "[ " <> (List.intercalate ", " (testShow <$> l)) <> " ]"
