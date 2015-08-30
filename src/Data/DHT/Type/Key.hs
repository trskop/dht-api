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
-- Portability:  NoImplicitPrelude
--
-- Abstract API for DHT implementations.
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


newtype DhtKey = DhtKey ByteString
  deriving (Data, Eq, Generic, Ord, Typeable)

instance Show DhtKey where
    showsPrec n (DhtKey bs) = showsPrec n bs

instance IsString DhtKey where
    fromString = DhtKey . fromString

toByteString :: DhtKey -> ByteString
toByteString (DhtKey bs) = bs
