{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:       $HEADER$
-- Description:  Data type for keys used in DHT.
-- Copyright:    (c) 2015 Jan Šipr, Matej Kollár; 2015-2016 Peter Trško
-- License:      BSD3
--
-- Stability:    experimental
-- Portability:  GHC specific language extensions.
--
-- Data type for keys used in DHT.
module Data.DHT.Type.Key
    ( DhtKey(..)
    )
  where

import Data.Data (Data, Typeable)
import Data.Eq (Eq)
import Data.Function ((.))
import Data.Ord (Ord)
import Data.String (IsString(fromString))
import GHC.Generics (Generic)
import Text.Show (Show(showsPrec), showString)

import Data.ByteString (ByteString)
import Data.Hashable (Hashable)
import Data.Text (Text)


-- | DHT is a map from keys to values, this data type represents the keys
-- accepted by DHT implementation.
data DhtKey
    = DhtKeyText !Text
    -- ^ Text is treated differently, since its up to a specific hash
    -- implementation to interpret text encoding correctly. In example it may
    -- treat 'Text' as UTF-8 encoded string of bytes.
    | DhtKey !ByteString
  deriving (Data, Eq, Generic, Ord, Typeable)

instance Show DhtKey where
    showsPrec _ k = showString "<<DhtKey " . showContent k . showString ">>"
      where
        showContent = \case
            DhtKeyText t -> showsPrec 0 t
            DhtKey bs    -> showsPrec 0 bs

instance IsString DhtKey where
    fromString = DhtKeyText . fromString

instance Hashable DhtKey -- Uses GHC Generics.
