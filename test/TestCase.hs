{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:       $HEADER$
-- Description:  All test cases aggregated and exported as tests :: [Test].
-- Copyright:    (c) 2015 Jan Šipr, Matej Kollár; 2015-2018 Peter Trško
-- License:      BSD3
--
-- Stability:    stable
-- Portability:  NoImplicitPrelude
--
-- All test cases aggregated and exported as @tests :: ['Test']@.
module TestCase (tests)
    where

import Test.Framework (Test, testGroup)

import qualified TestCase.Data.DHT.Core as DHT.Core (tests)
import qualified TestCase.Data.DHT.Type.Result as DHT.Type.Result (tests)


tests :: [Test]
tests =
    [ testGroup "Data.DHT.Type.Result" DHT.Type.Result.tests
    , testGroup "Data.DHT.Core" DHT.Core.tests
    ]
