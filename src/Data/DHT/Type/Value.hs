{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:       $HEADER$
-- Description:  Data type describing values that can be stored in DHT.
-- Copyright:    (c) 2015, Jan Šipr, Matej Kollár, Peter Trško
-- License:      BSD3
--
-- Stability:    experimental
-- Portability:  DeriveDataTypeable, DeriveGeneric, NoImplicitPrelude
--
-- Data type describing values that can be stored in DHT.
module Data.DHT.Type.Value
--    (
--    )
  where

import Data.Typeable (Typeable)
import GHC.Generics (Generic)


data DhtValue
  deriving (Generic, Typeable)
