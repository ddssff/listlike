{-# LANGUAGE CPP
            ,MultiParamTypeClasses
            ,FunctionalDependencies
            ,FlexibleInstances #-}

{-
Copyright (C) 2007 John Goerzen <jgoerzen@complete.org>

All rights reserved.

For license and copyright information, see the file COPYRIGHT
-}

{- |
   Module     : Data.ListLike.FoldableLL
   Copyright  : Copyright (C) 2007 John Goerzen
   License    : BSD3

   Maintainer : David Fox <dsf@seereason.com>, Andreas Abel
   Stability  : stable
   Portability: portable

Generic tools for data structures that can be folded.

Written by John Goerzen, jgoerzen\@complete.org

-}
module Data.ListLike.FoldableLL
    (-- * FoldableLL Class
     FoldableLL(..),
     -- * Utilities
     fold, foldMap, foldM, sequence_, mapM_
    ) where
import Prelude hiding (foldl, foldr, foldr1, sequence_, mapM_, foldMap)
import qualified Data.Foldable as F
import Data.Maybe
import qualified Data.List as L

{- | This is the primary class for structures that are to be considered
foldable.  A minimum complete definition provides 'foldl' and 'foldr'.

Instances of 'FoldableLL' can be folded, and can be many and varied.

These functions are used heavily in "Data.ListLike". -}
class FoldableLL full item | full -> item where
    {-# MINIMAL (foldl, foldr)
              | (foldl, genericIndexMaybe)
      #-}

    {- | Left-associative fold -}
    foldl :: (a -> item -> a) -> a -> full -> a

    {- | Strict version of 'foldl'. -}
    foldl' :: (a -> item -> a) -> a -> full -> a
    -- This implementation from Data.Foldable
    foldl' f a xs = foldr f' id xs a
        where f' x k z = k $! f z x

    -- | A variant of 'foldl' with no base case.  Requires at least 1
    -- list element.
    foldl1 :: (item -> item -> item) -> full -> item
    -- This implementation from Data.Foldable
    foldl1 f xs = fromMaybe (error "fold1: empty structure")
                    (foldl mf Nothing xs)
           where mf Nothing y = Just y
                 mf (Just x) y = Just (f x y)

    {- | Right-associative fold -}
    foldr :: (item -> b -> b) -> b -> full -> b
    foldr f z xs = gor i0
      where
        i0 :: Integer
        i0 = 0
        gor i = case genericIndexMaybe xs i of
            Nothing -> z
            Just x -> f x (gor $! i + 1)

    -- | Strict version of 'foldr'
    foldr' :: (item -> b -> b) -> b -> full -> b
    -- This implementation from Data.Foldable
    foldr' f a xs = foldl f' id xs a
        where f' k x z = k $! f x z

    -- | Like 'foldr', but with no starting value
    foldr1 :: (item -> item -> item) -> full -> item
    -- This implementation from Data.Foldable
    foldr1 f xs = fromMaybe (error "foldr1: empty structure")
                    (foldr mf Nothing xs)
           where mf x Nothing = Just x
                 mf x (Just y) = Just (f x y)

    -- | @genericIndexMaybe@ is a safe, generic version of
    -- 'Data.ListLike.index'. When the index is out of bounds it returns
    -- 'Nothing'; otherwise it returns 'Just' the item at that index.
    genericIndexMaybe :: (Integral index)=> full -> index -> Maybe item
    genericIndexMaybe xs i = integerIndexMaybe xs (toInteger i)
      where
        {- Use Integer as concrete index representation to avoid bizarre behavior
           with exotic Integral instances. -}
        integerIndexMaybe ys j
            | i < 0 = Nothing
            | otherwise = integerIfoldr
                (\ j' x mx -> if j == j' then Just x else mx) Nothing ys


{- | Combine the elements of a structure using a monoid.
     @'fold' = 'foldMap' id@ -}
fold :: (FoldableLL full item, Monoid item) => full -> item
fold = foldMap id

{- | Map each element to a monoid, then combine the results -}
foldMap :: (FoldableLL full item, Monoid m) => (item -> m) -> full -> m
foldMap f = foldr (mappend . f) mempty

instance FoldableLL [a] a where
    foldl = L.foldl
    foldl1 = L.foldl1
    foldl' = L.foldl'
    foldr = L.foldr
    foldr1 = L.foldr1
    foldr' = F.foldr'
{-
instance (F.Foldable f) => FoldableLL (f a) a where
    foldl = F.foldl
    foldl1 = F.foldl1
    foldl' = F.foldl'
    foldr = F.foldr
    foldr1 = F.foldr1
    foldr' = F.foldr'
-}

-- Based on http://stackoverflow.com/a/12881193/1333025
{- | Monadic version of left fold, similar to 'Control.Monad.foldM'. -}
foldM :: (Monad m, FoldableLL full item) => (a -> item -> m a) -> a -> full -> m a
foldM f z xs = foldr (\x rest a -> f a x >>= rest) return xs z

{- | A map in monad space, discarding results. -}
mapM_ :: (Monad m, FoldableLL full item) => (item -> m b) -> full -> m ()
mapM_ func = foldr ((>>) . func) (return ())

{- | Evaluate each action, ignoring the results.
   Same as @'mapM_' 'id'@. -}
sequence_ :: (Monad m, FoldableLL full (m item)) => full -> m ()
sequence_ = mapM_ id

{-| @genericElemIndex x xs@ returns 'Just' the index of the leftmost item equal to
    @x@ in @xs@ if it is found; otherwise 'Nothing'. -}
genericElemIndex ::
    (FoldableLL full item, Eq item, Integral index)=> item -> full -> Maybe index
genericElemIndex = genericFindIndex . (==)

{-| @genericElemIndex p xs@ returns 'Just' the index of the leftmost item in @xs@
    that satisfies @p@, 'Nothing' if @p@ is unsatisfied. -}
genericFindIndex ::
    (FoldableLL full item, Integral index)=>
    (item -> Bool) -> full -> Maybe index
genericFindIndex p =
    integerIfoldr (\ i x mi -> if p x then Just (fromInteger i) else mi) Nothing

{-| @integerIfoldr@ is equivalent to 'foldr', but also passes an 'Integer'
    0-based index to the function.
    Not exported; used to define @genericFindIndex@ and @genericIndexMaybe@. -}
integerIfoldr ::
    (FoldableLL full item)=> (Integer -> item -> a -> a) -> a -> full -> a
integerIfoldr f z ys = foldr (\ y k j -> f j y (k $! j + 1)) (\_-> z) ys 0
