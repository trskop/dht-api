{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
module Data.DHT.Core
    (
    -- * DHT Handle
    --
    -- | Specific DHT implementation should provide equivalent of
    -- @open\/create\/new@. Such function returns 'DhtHandle'.
      DhtHandle

    -- * DHT Operations
    , DhtResult

    , join
    , leave
    , lookup
    , insert
    )
  where

import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Function (($), (.))

import Data.DHT.Type.Handle (DhtHandle)
import qualified Data.DHT.Type.Handle as Internal
    ( forDhtHandle
    , hash
    , insert
    , join
    , leave
    , lookup
    , withDhtHandle
    )
import Data.DHT.Type.Result (DhtResult)


-- | Join DHT overlay.
join :: MonadIO m => DhtHandle k a -> DhtResult m ()
join = liftIO . Internal.withDhtHandle Internal.join

-- | Leave DHT overlay.
leave :: MonadIO m => DhtHandle k a -> DhtResult m ()
leave = liftIO . Internal.withDhtHandle Internal.leave

-- | Lookup specified 'DhtKey' in DHT and provide its value as a result, if it
-- is available.
lookup :: MonadIO m => DhtHandle k a -> k -> DhtResult m a
lookup h k = liftIO $ Internal.forDhtHandle h $ \h' s ->
    Internal.lookup h' s (Internal.hash h' s k)

-- | Insert 'DhtKey' with associated value of type 'Encoding' in DHT.
insert :: MonadIO m => DhtHandle k a -> k -> a -> DhtResult m ()
insert h k e = liftIO $ Internal.forDhtHandle h $ \h' s ->
    Internal.insert h' s (Internal.hash h' s k) e
