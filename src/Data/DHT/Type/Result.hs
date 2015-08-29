{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:       $HEADER$
-- Description:  Abstract API for DHT implementations.
-- Copyright:    (c) 2015, Jan Šipr, Matej Kollár, Peter Trško
-- License:      BSD3
--
-- Stability:    experimental
-- Portability:  DeriveDataTypeable, DeriveGeneric, NoImplicitPrelude
--
-- Abstract API for DHT implementations.
module Data.DHT.Type.Result
--    (
--    )
  where

import Data.Typeable (Typeable)
import GHC.Generics (Generic)


-- | Result of DHT operation.
data DhtResult a
  deriving (Generic, Typeable)
