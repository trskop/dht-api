{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:       $HEADER$
-- Description:  Data type for DHT operation result.
-- Copyright:    (c) 2015 Jan Šipr, Matej Kollár, Peter Trško
-- License:      BSD3
--
-- Stability:    experimental
-- Portability:  GHC specific language extensions.
--
-- Data type for DHT operation result.
module Data.DHT.Type.Result
    ( DhtResult
    , DhtResultVar(..)

    -- * Smart Constructors
    , dhtResult
    , exception
    , exception_
    , result
    , result_

    -- * Processing
    --
    -- | Functions for handling 'DhtResult's.
    , wait
    , wait'
    , wait_

    -- * Running Producers
    --
    -- | Running actions that are producing 'DhtResult's.
    , withFutureResult
    , withFutureResult'
    )
  where

import Control.Concurrent.MVar
    ( MVar
    , newEmptyMVar
    , newMVar
    , readMVar
    , tryPutMVar
    )
import Control.Exception (Exception(toException), SomeException)
import Control.Monad (Monad(return), liftM, void)
import Data.Either (Either(Left, Right))
import Data.Function ((.), ($))
import Data.Functor ((<$>))
import Data.Maybe (Maybe(Just, Nothing))
import Data.Tuple (snd)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import System.IO (IO)

import Control.Monad.Catch (MonadThrow(throwM))
import Control.Monad.IO.Class (MonadIO(liftIO))


-- | Result of DHT operation is a future 'DhtResultVar'.
type DhtResult m a = m (DhtResultVar a)

-- | Synchronization variable with DHT operation result. It can be either
-- expected result or 'SomeException'. This way it is possible to detect
-- exceptions in those places where operation result is processed, and not
-- rely on exception propagation.
newtype DhtResultVar a = DhtResultVar (MVar (Either SomeException a))
  deriving (Generic, Typeable)

-- | Smart constructor for 'DhtResult'.
dhtResult
    :: Maybe (Either SomeException a)
    -- ^ When 'Nothing', then 'DhtResult' is created empty, otherwise it is
    -- filled with @'Either' 'SomeException' result@.
    -> DhtResult IO a
dhtResult v = DhtResultVar <$> case v of
    Nothing -> newEmptyMVar
    Just r  -> newMVar r

-- | Run action that produces either exception or result. Example of running
-- asynchronous task:
--
-- @
-- asynchronously
--     :: ('Exception' e)
--     => ((Either e a -> m ()) -> IO 'Control.Concurrent.ThreadId')
--     -> IO (ThreadId, DhtResultVar a)
-- asynchronously f = 'withFutureResult'' $ \putResult ->
--     'Control.Concurrent.forkIO' $ f putResult `onException` (putResult . Left)
-- @
withFutureResult'
    :: (Exception e, MonadIO m)
    => ((Either e a -> m ()) -> m b)
    -- ^ Action to perform. It gets a function for publishing its result as an
    -- argument. If it tries to produce multiple values, then only the first
    -- one is accepted and the rest is thrown away.
    -> m (b, DhtResultVar a)
withFutureResult' f = do
    var <- liftIO $ dhtResult Nothing
    r <- f $ liftIO . void . tryPutResult var
    return (r, var)
  where
    tryPutResult (DhtResultVar v) x = tryPutMVar v $ case x of
        Left  e -> Left (toException e)
        Right r -> Right r

-- | Run action that produces either exception or result. This is a simplified
-- version of 'withFutureResult'':
--
-- @
-- 'withFutureResult' = fmap snd . 'withFutureResult''
-- @
withFutureResult
    :: (Exception e, MonadIO m)
    => ((Either e a -> m ()) -> m ())
    -- ^ Action to perform. It gets function for publishing its result as an
    -- argument. If it tries to produce multiple values, then only the first
    -- one is accepted and the rest is thrown away.
    -> DhtResult m a
withFutureResult = liftM snd . withFutureResult'

-- | Create 'DhtResult' filled with specified exception.
exception :: (Exception e, MonadIO m) => e -> DhtResult m a
exception = liftIO . dhtResult . Just . Left . toException

-- | Create 'DhtResult' filled with specified exception. This is type
-- restricted variant of 'exception':
--
-- @
-- 'exception_' = 'exception'
-- @
exception_ :: (Exception e, MonadIO m) => e -> DhtResult m ()
exception_ = exception

-- | Create 'DhtResult' filled with specified value.
result :: MonadIO m => a -> DhtResult m a
result = liftIO . dhtResult . Just . Right

-- | Create 'DhtResult' filled with @()@:
--
-- @
-- 'result_' = 'result' ()
-- @
result_ :: MonadIO m => DhtResult m ()
result_ = result ()

-- | Get result of DHT operation or block if it's not yet available. See also
-- 'wait' and 'wait_'.
wait'
    :: MonadIO m
    => (SomeException -> m a)
    -- ^ Handle exception received from DHT operation.
    -> DhtResultVar a
    -- ^ Future of DHT operation.
    -> m a
wait' exceptionHandler (DhtResultVar var) = do
    r <- liftIO $ readMVar var
    case r of
        Left ex -> exceptionHandler ex
        Right a -> return a
{-# INLINEABLE wait' #-}
{-# SPECIALIZE
    wait' :: (SomeException -> IO a) -> DhtResultVar a -> IO a
  #-}
{-# SPECIALIZE
    wait' :: (SomeException -> IO ()) -> DhtResultVar () -> IO ()
  #-}

-- | Get result of DHT operation or block if it's not yet available. When
-- exception is received, then this function throws it:
--
-- @
-- 'wait' = 'wait'' 'throwM'
-- @
--
-- See also 'wait_' and 'wait''.
wait :: (MonadThrow m, MonadIO m) => DhtResultVar a -> m a
wait = wait' throwM
{-# INLINE wait #-}

-- | Block until DHT operation finishes. When exception is received, then this
-- function throws it:
--
-- @
-- 'wait_' = 'wait'
-- @
--
-- See also 'wait' and 'wait''.
wait_ :: (MonadThrow m, MonadIO m) => DhtResultVar () -> m ()
wait_ = wait
{-# INLINE wait_ #-}
