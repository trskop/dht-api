{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:       $HEADER$
-- Description:  TODO
-- Copyright:    (c) 2016 Peter Trško
-- License:      BSD3
--
-- Stability:    experimental
-- Portability:  GHC specific language extensions.
--
-- TODO
module Data.DHT.Type.Hash
    ( DhtHash(..)
    , succNum
    , predNum
    )
  where

import Prelude (Bounded(maxBound, minBound), Integral, Num((-), (+)))

import Control.Applicative (liftA2)
import Control.Arrow ((***))
import Data.Bool (Bool, (&&), (||), otherwise)
import Data.Eq (Eq)
import Data.Functor (Functor(fmap))
import Data.Ord (Ord((<=)))
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Text.Show (Show)


data Bound a = Including a | Excluding a
  deriving (Eq, Generic, Ord, Show, Typeable)

instance Functor Bound where
    fmap f = \case
        Including a -> Including (f a)
        Excluding a -> Excluding (f a)

-- | Evaluate 'Bound' type. Used to convert interval into another interval
-- where both bounds are inclusive.
bound
    :: (a -> a)
    -- ^ Functiona applied to in 'Excluding' case to a bound value. Usually
    -- 'succ' for min bound and 'pred' for max bound.
    -> Bound a
    -> a
bound f = \case
    Including a -> a
    Excluding a -> f a

-- | DHT hash space is circular, i.e. 'maxBound' is immediately followed by
-- 'minBound' and vice versa.
class (Bounded a, Eq a, Ord a, Show a) => DhtHash a where
    {-# MINIMAL succ, pred #-}

    -- |
    -- @
    -- 'succ' 'maxBound' = 'minBound'
    -- @
    succ :: a -> a

    -- |
    -- @
    -- 'pred' 'minBound' = 'maxBound'
    -- @
    pred :: a -> a

    -- | Check if element is inside a specified interval. Note that this
    -- function handles the circular notion of hash space correctly. In example
    -- lets have a hash space 0, 1, ... 9 then for example this predicate holds
    -- 0 ∈ (7, 1].
    inInterval :: (Bound a, Bound a) -> a -> Bool
    inInterval bs
      | minb <= maxb = unsafeInInterval bs'
      | otherwise =
        -- We need to split the interval when bounds include the point where
        -- "end" and "beginning" of the DHT circle meet. In example, lets have
        -- a hash space 0, 1, ... 9, then interval (7, 1] is first converted in
        -- to inclusive interval [8, 1] and then split in to two sub-intervals
        -- [8, 9] and [0, 1].
        unsafeInInterval (minb, maxBound)
        <||> unsafeInInterval (minBound, maxb)
      where
        -- Bounds (minb, maxb) need to be always inclusive, hence the
        -- conversion.
        bs'@(minb, maxb) = (bound succ *** bound pred) bs
        (<||>) = liftA2 (||)

    -- | Assumes that @lowerBound < upperBound@ in
    -- @'unsafeInInterval' (lowerBound, upperBound)@.
    unsafeInInterval :: (a, a) -> a -> Bool
    unsafeInInterval (minb, maxb) a = minb <= a && a <= maxb

succNum :: (Bounded a, Integral a) => a -> a
succNum = (+ 1)

predNum :: (Bounded a, Integral a) => a -> a
predNum n = n - 1
