{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:       $HEADER$
-- Description:  Test cases for Data.DHT.Core module.
-- Copyright:    (c) 2015 Jan Šipr, Matej Kollár, Peter Trško
-- License:      BSD3
--
-- Stability:    stable
-- Portability:  GHC specific languge extensions.
--
-- Test cases for "Data.DHT.Core" module.
module TestCase.Data.DHT.Type.NodeId (tests)
  where

import Prelude (Bounded(maxBound))

import Data.Function (($))
import qualified Data.List as List (replicate)
import Text.Show (Show(show))

import Test.HUnit ((@?=))
import Test.Framework (Test)
import Test.Framework.Providers.HUnit (testCase)

import Data.DHT.Type.NodeId (NodeId(NodeId))


tests :: [Test]
tests =
    [ testCase "000...0" $ show (NodeId 0)           @?= List.replicate 16 '0'
    , testCase "fff...f" $ show (maxBound :: NodeId) @?= List.replicate 16 'f'
    ]
