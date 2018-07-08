{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
-- |
-- Module:       $HEADER$
-- Description:  Abstract API for DHT implementations.
-- Copyright:    (c) 2015 Jan Šipr, Matej Kollár; 2015-2018 Peter Trško
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

import Data.DHT.Type.Result (DhtResult)


-- | Abstract DHT handle of existing DHT instance.
data DhtHandle k a =
    forall s h. (Show s, Show h) => DhtHandle !(DhtHandle' s h k a)
    -- ^ Mostly for debugging purposes we need to be able to print some basic
    -- information about 'DhtHandle'. It is up to DHT implementation to provide
    -- enough insight using 'Show' instance of its state. In example it can
    -- contain things like DHT instance name and DHT node ID.
  deriving (Typeable)

--instance (Typeable k, Typeable a) => Show (DhtHandle k a) where
instance Show (DhtHandle k a) where
    showsPrec n = withDhtHandle $ \_ s -> showParen (n > applicationPrecedence)
        $ showString "DhtHandle " . showsPrec (applicationPrecedence + 1) s
      where
        applicationPrecedence = 10

-- | Low-level DHT handle that is used by DHT implementations, but not consumers.
data DhtHandle' s h k a = DhtHandle'
    { state :: !s
    -- ^ Internal state of the DHT implementation.

    , hash :: !(s -> k -> h)
    -- ^ Having hash function being part of the interface allows implementation
    -- to decide where the hashing function is evaluated. In example, having
    -- @hash = const id@ will force @h = k@, and DHT implementation will be
    -- handling the hashing.

    , join :: !(s -> DhtResult IO ())
    -- ^ Join the DHT overlay.

    , leave :: !(s -> DhtResult IO ())
    -- ^ Leave the DHT overlay.

    , lookup :: !(s -> h -> DhtResult IO a)

    , insert :: !(s -> h -> a -> DhtResult IO ())
    }
  deriving (Generic, Typeable)

-- | Helper function that simplifies unwrapping 'DhtHandle'. Flipped version of
-- 'forDhtHandle'.
withDhtHandle
    :: forall k a r
    .  (forall s h. (Show s, Show h) => DhtHandle' s h k a -> s -> r)
    -- ^ Function that needs access to implementation specific 'DhtHandle''.
    -- 'Show' is the only thing we are allowed to know about internal state.
    -> DhtHandle k a
    -> r
withDhtHandle f (DhtHandle h@DhtHandle'{state = s}) = f h s
{-# INLINE withDhtHandle #-}

-- | Helper function that simplifies unwrapping 'DhtHandle'. Flipped version of
-- 'withDhtHandle'.
forDhtHandle
    :: forall k a r
    .  DhtHandle k a
    -> (forall s h. (Show s, Show h) => DhtHandle' s h k a -> s -> r)
    -- ^ Function that needs access to implementation specific 'DhtHandle''.
    -- 'Show' is the only thing we are allowed to know about internal state.
    -> r
forDhtHandle (DhtHandle h@DhtHandle'{state = s}) f = f h s
{-# INLINE forDhtHandle #-}
