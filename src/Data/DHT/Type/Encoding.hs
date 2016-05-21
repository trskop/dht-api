{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-} -- For Semigroup instance.
{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:       $HEADER$
-- Description:  Data type for encoded values stored in DHT.
-- Copyright:    (c) 2016 Peter Trško
-- License:      BSD3
--
-- Stability:    experimental
-- Portability:  GHC specific language extensions.
--
-- Data type for encoded values stored in DHT.
module Data.DHT.Type.Encoding
    ( Encoding(..)
    )
  where

import Data.Monoid (Monoid(mempty))
import Data.Semigroup (Semigroup)
import Data.Typeable (Typeable)

import Data.Default.Class (Default(def))
#if MIN_VERSION_bytestring(0,10,2)
import qualified Data.ByteString.Builder as ByteString (Builder)
#else
import qualified Data.ByteString.Lazy.Builder as ByteString (Builder)
#endif


newtype Encoding = Encoding {fromEncoding :: ByteString.Builder}
  deriving (Semigroup, Monoid, Typeable)

instance Default Encoding where
    def = mempty
