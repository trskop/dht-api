{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:       $HEADER$
-- Description:  All test cases aggregated and exported as tests :: [Test].
-- Copyright:    (c) 2015, Jan Šipr, Matej Kollár, Peter Trško
-- License:      BSD3
--
-- Stability:    stable
-- Portability:  NoImplicitPrelude
--
-- All test cases aggregated and exported as @tests :: ['Test']@.
module TestCase (tests)
    where

import Test.Framework (Test, testGroup)

import qualified TestCase.Data.DHT as DHT (tests)
import qualified TestCase.Data.DHT.Type.Result as DHT.Type.Result (tests)


tests :: [Test]
tests =
    [ testGroup "Data.DHT.Type.Result" DHT.Type.Result.tests
    , testGroup "Data.DHT" DHT.tests
    ]
