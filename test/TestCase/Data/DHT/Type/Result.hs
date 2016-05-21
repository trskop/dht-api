{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:       $HEADER$
-- Description:  Test cases for Data.DHT.Type.Result module.
-- Copyright:    (c) 2015 Jan Šipr, Matej Kollár; 2015-2016 Peter Trško
-- License:      BSD3
--
-- Stability:    stable
-- Portability:  GHC specific language extensions.
--
-- Test cases for "Data.DHT.Type.Result" module.
module TestCase.Data.DHT.Type.Result (tests)
  where

import Control.Exception (Exception(fromException), SomeException)
import Control.Monad (Monad((>>=), return))
import Data.Bool (Bool(False, True))
import Data.Eq (Eq)
import Data.Function (($))
import Data.Maybe (Maybe(Just, Nothing))
import Data.Monoid ((<>))
import Data.Typeable (Typeable)
import System.IO (IO)
import Text.Show (Show(show))

import Test.HUnit (Assertion, assertBool, assertFailure)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)

import Data.DHT.Type.Result
    ( exception
    , result
    , wait'
    )


tests :: [Test]
tests =
    [ testGroup "wait'"
        [ testCase "Handle received exception"
            handleExceptionTest
        , testCase "Receive result without handler being invoked"
            handleResultTest
        ]
    ]

handleExceptionTest :: Assertion
handleExceptionTest =
    exception DummyException >>= wait' handler >>= checkResult
  where
    handler :: SomeException -> IO Bool
    handler ex = case fromException ex of
        Just DummyException -> return True
        Nothing             -> do
            assertFailure $ "Received unexpected exception: " <> show ex
            return False

    checkResult = assertBool
        "Exception handler wasn't invoked, which it should have been."

handleResultTest :: Assertion
handleResultTest = result True >>= wait' handler >>= checkResult
  where
    handler :: SomeException -> IO Bool
    handler _exception = return False

    checkResult = assertBool
        "Exception handler was invoked, which it wasn't supposed to."

data DummyException = DummyException
  deriving (Eq, Show, Typeable)

instance Exception DummyException
