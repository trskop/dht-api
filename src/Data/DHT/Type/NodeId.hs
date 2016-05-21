{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:       $HEADER$
-- Description:  TODO
-- Copyright:    (c) 2015 Jan Šipr, Matej Kollár, Peter Trško
-- License:      BSD3
--
-- Stability:    experimental
-- Portability:  GHC specific language extensions.
--
-- TODO
module Data.DHT.Type.NodeId
  where

import Prelude (Bounded)

import Data.Data (Data, Typeable)
import Data.Eq (Eq)
import Data.Ord (Ord)
import Data.Word (Word64)
import GHC.Generics (Generic)
import Text.Printf (printf)
import Text.Show (Show(showsPrec), showString)


newtype NodeId = NodeId Word64
  deriving (Bounded, Data, Eq, Generic, Ord, Typeable)

instance Show NodeId where
    showsPrec _ (NodeId n) = showString (printf "%016x" n)
