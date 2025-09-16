{-# LANGUAGE GeneralizedNewtypeDeriving
           , FlexibleInstances
           , MultiParamTypeClasses
           , TypeFamilies
  #-}

module BenchmarkInfrastructure (
MyList (MyList),
MySeq (MySeq),
MyVector (MyVector),
) where

import qualified Data.ListLike as LL
import qualified Data.ListLike.Base as LL
import qualified Data.ListLike.FoldableLL as LL
import GHC.Exts (IsList (..), build)
import Control.DeepSeq (NFData (..))
import qualified Data.Sequence as S
import qualified Data.Vector as V
import qualified Data.List as L
import Data.Functor.Const (Const (Const))
import Data.Monoid (Monoid (..))
import Data.Semigroup (Semigroup (..))


newtype MyList a = MyList [a]
  deriving (Monoid, Semigroup, NFData)
{-^ 'List' with minimal 'LL.ListLike' and 'LL.FoldableLL' definitions.
Used to find out whether default methods perform well on cons lists.
-}

instance LL.ListLike (MyList a) a where
    cons x (MyList xs) = MyList (x : xs)
    uncons (MyList xs) = case xs of
        [] -> Nothing
        x : xs' -> Just (x, MyList xs')
    --- Required by MINIMAL pragma ---
    null = LL.foldr (\ _ _ -> False) True
    singleton = (`LL.cons` mempty)

instance IsList (MyList a) where
    type Item (MyList a) = a
    -- No cheating on these to make it free!
    fromList = LL.fromList'
    toList = LL.toList'

instance LL.FoldableLL (MyList a) a where
    foldr f z (MyList xs) = LL.foldr f z xs
    foldl f z (MyList xs) = LL.foldl f z xs


newtype MySeq a = MySeq (S.Seq a)
  deriving (Monoid, Semigroup, NFData)
{-^ 'S.Seq' with minimal 'LL.ListLike' and 'LL.FoldableLL' definitions.
Used to find whether default methods perform well on jack-of-all-trades structures.
-}

instance LL.ListLike (MySeq a) a where
    singleton x = MySeq (LL.singleton x)
    genericLength (MySeq xs) = LL.genericLength xs
    -- null (MySeq xs) = LL.null xs
    uncons (MySeq xs) = case S.viewl xs of
        S.EmptyL -> Nothing
        x S.:< xs -> Just (x, MySeq xs)
    index (MySeq xs) i = LL.index xs i
    genericDrop n (MySeq xs) = MySeq (LL.genericDrop n xs)
    genericTake n (MySeq xs) = MySeq (LL.genericTake n xs)

instance IsList (MySeq a) where
    type Item (MySeq a) = a
    fromListN n xs = MySeq (fromListN n xs)
    fromList = LL.fromList'
    toList = LL.toList'

instance LL.FoldableLL (MySeq a) a where
    foldr f z (MySeq xs) = LL.foldr f z xs
    foldl f z (MySeq xs) = LL.foldl f z xs


newtype MyVector a = MyVector (V.Vector a)
  deriving (Monoid, Semigroup, NFData)
{-^ 'V.Vector' with minimal 'LL.ListLike' and 'LL.FoldableLL' definitions.
Used to find whether default methods perform well on arrays.
-}

instance LL.ListLike (MyVector a) a where
    genericLength (MyVector xs) = LL.genericLength xs
    -- null (MyVector xs) = LL.null xs
    index (MyVector xs) i = LL.index xs i
    -- Required
    singleton x = MyVector (LL.singleton x)
    uncons (MyVector xs) = case LL.uncons xs of
        Nothing -> Nothing
        Just (x, xs) -> Just (x, MyVector xs)
    -- With old default inits, each run of 'inits' (not the whole 'inits' benchmark) takes about 6 minutes.
    genericDrop n (MyVector xs) = MyVector (LL.genericDrop n xs)
    genericTake n (MyVector xs) = MyVector (LL.genericTake n xs)

instance IsList (MyVector a) where
    type Item (MyVector a) = a
    fromListN n xs = MyVector (fromListN n xs)
    fromList xs = MyVector (fromList xs)
    toList = LL.toList'

instance LL.FoldableLL (MyVector a) a where
    foldr f z (MyVector xs) = LL.foldr f z xs
    foldl f z (MyVector xs) = LL.foldl f z xs
