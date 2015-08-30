{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:       $HEADER$
-- Description:  Test cases for Data.DHT.Core module.
-- Copyright:    (c) 2015, Jan Šipr, Matej Kollár, Peter Trško
-- License:      BSD3
--
-- Stability:    stable
-- Portability:  DeriveDataTypeable, LambdaCase, NoImplicitPrelude
--
-- Test cases for "Data.DHT.Core" module.
module TestCase.Data.DHT.Core (tests)
  where

import Prelude (error)

import Control.Concurrent.MVar (newMVar, tryReadMVar)
import Control.Exception (Exception(fromException, toException))
import qualified Data.Char as Char (toLower)
import Data.Data (Data(toConstr), Typeable, showConstr)
import Data.Either (Either(Left, Right))
import Data.Eq (Eq)
import Data.Function ((.), ($))
import Data.Functor (Functor(fmap))
import qualified Data.List as List (map)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Monoid ((<>))
import Data.String (String)
import System.IO (IO)
import Text.Show (Show(show, showsPrec))

import Test.HUnit (Assertion, assertEqual, assertFailure)
import Test.Framework (Test)
import Test.Framework.Providers.HUnit (testCase)

import qualified Data.DHT.Core as Core
import Data.DHT.Type.Handle
    ( DhtHandle(DhtHandle)
    , DhtHandle'(DhtHandle', insert, join, leave, lookup, state)
    )
import Data.DHT.Type.Result (DhtResult, DhtResultVar(DhtResultVar))


tests :: [Test]
tests =
    -- Check if, for example, calling Core.join would call join stored in
    -- DhtHandle'.
    [ testCase "Calling join"   . testCall Join   $ \h -> Core.join h
    , testCase "Calling leave"  . testCall Leave  $ \h -> Core.leave h
    , testCase "Calling lookup" . testCall Lookup $ \h -> Core.lookup h keyExpected
    , testCase "Calling insert" . testCall Insert $ \h ->
        Core.insert h keyExpected valueExpected
    ]
  where
    keyExpected = error "Expected DHT key."
    valueExpected = error "Expected DHT value."

testCall :: Operation -> (DhtHandle -> DhtResult IO a) -> Assertion
testCall op f = do
    DhtResultVar var <- f $ newDummyDht "Dummy DHT implementation."
    possiblyResult <- tryReadMVar var
    case possiblyResult of
        Nothing -> failedWithMissingResult
        Just r -> case r of
            Left ex -> case fromException ex :: Maybe Operation of
                Nothing -> failedWithException ex
                Just op' -> assertSameCall op'
            Right _ -> failedWithUnexpectedResult
  where
    assertSameCall = assertEqual
        "Called function and function being called should be the same" op
    failedWithUnexpectedResult = assertFailure $ "Call to " <> show op
        <> " had produced unexpected result"
    failedWithException ex = assertFailure $ "Call to " <> show op
        <> " failed with unexpected exception: " <> show ex
    failedWithMissingResult = assertFailure $ "Call to " <> show op
        <> " hadn't returned a filled DhtResult, as it was expected."

data Operation = Insert | Join | Leave | Lookup
  deriving (Data, Eq, Typeable)

instance Exception Operation

instance Show Operation where
    showsPrec n = showsPrec n . operationToString
      where
        operationToString = List.map Char.toLower . showConstr . toConstr

newDummyDht :: String -> DhtHandle
newDummyDht str = DhtHandle DhtHandle'
    { state  = str
    , join   = \_state             -> called Join
    , leave  = \_state             -> called Leave
    , lookup = \_state _key        -> called Lookup
    , insert = \_state _key _value -> called Insert
    }
  where
    called :: Operation -> DhtResult IO a
    called = fmap DhtResultVar . newMVar . Left . toException
