{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:       $HEADER$
-- Description:  Abstract API for DHT implementations.
-- Copyright:    (c) 2015, Jan Šipr, Matej Kollár, Peter Trško
-- License:      BSD3
--
-- Stability:    experimental
-- Portability:  NoImplicitPrelude
--
-- Abstract API for DHT implementations.
module Data.DHT.Type.Handle
--    (
--    )
    where

import Data.Typeable (Typeable)
import GHC.Generics (Generic)

import Data.DHT.Type.Key (DhtKey)


data DhtResult a
data DhtValue

data DhtHandle = forall s. DhtHandle (DhtHandle' s)

-- | Low-level DHT handle that is used by DHT implementations, but not consumers.
data DhtHandle' s = DhtHandle'
    { join :: s -> DhtResult ()
    , leave :: s -> DhtResult ()
    , lookup :: s -> DhtKey -> DhtResult DhtValue
    , insert :: s -> DhtKey -> DhtValue -> DhtResult ()
    }
  deriving (Generic, Typeable)
