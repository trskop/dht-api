{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
-- |
-- Module:       $HEADER$
-- Description:  Abstract API for DHT implementations.
-- Copyright:    (c) 2015 Jan Šipr, Matej Kollár; 2015-2016 Peter Trško
-- License:      BSD3
--
-- Stability:    experimental
-- Portability:  GHC specific language extensions.
--
-- Abstract API for DHT implementations.
module Data.DHT.Type.Handle
    ( DhtHandle(..)
    , DhtHandle'(..)
    , withDhtHandle
    , forDhtHandle
    )
  where

import Prelude (Num((+)))

import Data.Function ((.), ($))
import Data.Ord (Ord((>)))
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import System.IO (IO)
import Text.Show (Show(showsPrec), showParen, showString)

import Data.DHT.Type.Encoding (Encoding)
import Data.DHT.Type.Key (DhtKey)
import Data.DHT.Type.Result (DhtResult)


-- | Abstract DHT handle of existing DHT instance.
data DhtHandle =
    forall s h. (Show s, Show h) => DhtHandle !(DhtHandle' s h)
    -- ^ Mostly for debugging purposes we need to be able to print some basic
    -- information about 'DhtHandle'. It is up to DHT implementation to provide
    -- enough insight using 'Show' instance of its state. In example it can
    -- contain things like DHT instance name and DHT node ID.
  deriving (Typeable)

instance Show DhtHandle where
    showsPrec n = withDhtHandle $ \_ s -> showParen (n > applicationPrecedence)
        $ showString "DhtHandle " . showsPrec (applicationPrecedence + 1) s
      where
        applicationPrecedence = 10

-- | Low-level DHT handle that is used by DHT implementations, but not consumers.
data DhtHandle' s h = DhtHandle'
    { state :: !s
    , hash :: !(s -> DhtKey -> h)
    , join :: !(s -> DhtResult IO ())
    , leave :: !(s -> DhtResult IO ())
    , lookup :: !(s -> h -> DhtResult IO Encoding)
    , insert :: !(s -> h -> Encoding -> DhtResult IO ())
    }
  deriving (Generic, Typeable)

-- | Helper function that simplifies unwrapping 'DhtHandle'. Flipped version of
-- 'forDhtHandle'.
withDhtHandle
    :: (forall s h. (Show s, Show h) => DhtHandle' s h -> s -> a)
    -- ^ Function that needs access to implementation specific 'DhtHandle''.
    -- 'Show' is the only thing we are allowed to know about internal state.
    -> DhtHandle
    -> a
withDhtHandle f (DhtHandle h@DhtHandle'{state = s}) = f h s

-- | Helper function that simplifies unwrapping 'DhtHandle'. Flipped version of
-- 'withDhtHandle'.
forDhtHandle
    :: DhtHandle
    -> (forall s h. (Show s, Show h) => DhtHandle' s h -> s -> a)
    -- ^ Function that needs access to implementation specific 'DhtHandle''.
    -- 'Show' is the only thing we are allowed to know about internal state.
    -> a
forDhtHandle (DhtHandle h@DhtHandle'{state = s}) f = f h s
