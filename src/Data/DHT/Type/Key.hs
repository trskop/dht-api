{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:       $HEADER$
-- Description:  Data type for keys used in DHT.
-- Copyright:    (c) 2015, Jan Šipr, Matej Kollár, Peter Trško
-- License:      BSD3
--
-- Stability:    experimental
-- Portability:  NoImplicitPrelude
--
-- Data type for keys used in DHT.
module Data.DHT.Type.Key
--    (
--    )
  where

import Data.Data (Data, Typeable)
import Data.Eq (Eq)
import Data.Function ((.))
import Data.Ord (Ord)
import Data.String (IsString(fromString))
import GHC.Generics (Generic)
import Text.Show (Show(showsPrec))

import Data.ByteString (ByteString)
import Data.Hashable (Hashable)


-- | DHT is a map from keys to values, this data type represents the keys
-- accepted by DHT implementation.
newtype DhtKey = DhtKey ByteString
  deriving (Data, Eq, Generic, Ord, Typeable)

instance Show DhtKey where
    showsPrec n (DhtKey bs) = showsPrec n bs

instance IsString DhtKey where
    fromString = DhtKey . fromString

instance Hashable DhtKey -- Uses GHC Generics.

-- | Unpack 'DhtKey' in to (strict) 'ByteString'.
toByteString :: DhtKey -> ByteString
toByteString (DhtKey bs) = bs
