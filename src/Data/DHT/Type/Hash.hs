{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:       $HEADER$
-- Description:  TODO
-- Copyright:    (c) 2016-2018 Peter Trško
-- License:      BSD3
--
-- Stability:    experimental
-- Portability:  GHC specific language extensions.
--
-- TODO
module Data.DHT.Type.Hash
    ( DhtHash(..)
    , Bound(..)
    , succNum
    , predNum
    , isWholeSpace
    )
  where

import Prelude (Bounded(maxBound, minBound), Integral, Num((-), (+)))

import Control.Applicative (liftA2)
import Control.Arrow ((***))
import Data.Bool (Bool(False, True), (&&), (||), otherwise)
import Data.Eq (Eq((/=), (==)))
import Data.Function (const)
import Data.Functor (Functor(fmap))
import Data.Ord (Ord((<=)))
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Text.Show (Show)


-- | Represents one bound of an interval.
--
-- @
-- [x, y] = ('Including' x, 'Including' y)
-- [x, y) = ('Including' x, 'Excluding' y)
-- (x, y] = ('Excluding' x, 'Including' y)
-- (x, y) = ('Excluding' x, 'Excluding' y)
-- @
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
    --
    -- Following hold:
    --
    -- @
    -- forall x,y ∈ I.
    --     x \< y =\> ((y, x) = I \\ [x, y]
    --             ∧ (y, x] = I \\ (x, y]
    --             ∧ [y, x) = I \\ [x, y)
    --             ∧ [y, x] = I \\ (x, y))
    -- @
    --
    -- @
    -- forall x ∈ I.
    --       (x, x] = [x, x) = I
    --     ∧ [x, x] = {x}
    --     ∧ (x, x) = I \\ {x}
    -- @
    inInterval :: (Bound a, Bound a) -> a -> Bool
    inInterval bs = case bs of
        (Including b1, Excluding b2)
          | b1 == b2  -> const True     -- [x, x) = I
          | otherwise -> inInterval'

        (Excluding b1, Including b2)
          | b1 == b2  -> const True     -- (x, x] = I
          | otherwise -> inInterval'

        (Including b1, Including b2)
          | b1 == b2  -> (== b1)        -- [x, x] = {x}
          | otherwise -> inInterval'

        (Excluding b1, Excluding b2)
          | b1 == b2  -> (/= b1)        -- (x, x) = I \ {x}
          | otherwise -> inInterval'
      where
        inInterval'
          | minb <= maxb = unsafeInInterval bs'
          | otherwise    =
            -- We need to split the interval when bounds include the point
            -- where "end" and "beginning" of the DHT circle meet. In example,
            -- lets have a hash space 0, 1, ... 9, then interval (7, 1] is
            -- first converted in to inclusive interval [8, 1] and then split
            -- in to two sub-intervals [8, 9] and [0, 1].
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

succNum :: Integral a => a -> a
succNum = (+ 1)

predNum :: Integral a => a -> a
predNum n = n - 1

-- | Predicate that checks if the bounds represent the whole identifier space
-- @I@, which can be represented as:
--
-- @
-- forall x ∈ I. (x, x] = [x, x) = I
-- @
isWholeSpace :: Eq a => (Bound a, Bound a) -> Bool
isWholeSpace = \case
    (Including b1, Excluding b2) -> b1 == b2
    (Excluding b1, Including b2) -> b1 == b2
    _                            -> False
