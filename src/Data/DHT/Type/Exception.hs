{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:       $HEADER$
-- Description:  Top-level exception wrapper for exceptions thrown by DHT
--               implementation.
-- Copyright:    (c) 2015 Jan Šipr, Matej Kollár; 2015-2018 Peter Trško
-- License:      BSD3
--
-- Stability:    experimental
-- Portability:  GHC specific language extensions.
--
-- Top-level exception wrapper for exceptions thrown by DHT implementation.
module Data.DHT.Type.Exception
  where

import Control.Exception (Exception)
import qualified Control.Exception as Exception (throw, throwIO)
import Data.Typeable (Typeable)
import System.IO (IO)
import Text.Show (Show(showsPrec))


-- | Exceptions form hierarchy with 'DhtException' on top.
data DhtException = forall e. IsDhtException e => DhtException e
  deriving (Typeable)

instance Show DhtException where
  showsPrec n (DhtException e) = showsPrec n e

instance Exception DhtException

-- | Mark exception as 'DhtException'.
class Exception e => IsDhtException e

-- | Raise a 'DhtException' in pure context. See also 'Exception.throw'.
throw :: IsDhtException e => e -> a
throw = Exception.throw

-- | Raise a 'DhtException' in 'IO' context. See also 'Exception.throwIO'.
throwIO :: IsDhtException e => e -> IO a
throwIO = Exception.throwIO
