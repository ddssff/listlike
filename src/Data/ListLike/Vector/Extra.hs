{-| This is an interal module of generic Vector functions that are used by the
    instances.
-}

{-# LANGUAGE CPP
           , BangPatterns
           , FlexibleContexts
           , MultiParamTypeClasses
           , TypeFamilies
  #-}

module Data.ListLike.Vector.Extra (
isPrefixOf,
isSuffixOf,
fromListLike,
map,
mapM,
inits,
tails,
intersperse,
groupBy,
sortBy,
unlines,
unwords,
) where


import qualified Prelude as P
import           Prelude
    ( Num (..), Ord (..), Eq (..)
    , Bool (..), (&&), otherwise
    , (.), ($), ($!)
    )
import qualified Data.Vector.Generic as V
import qualified Data.ListLike.Base as LL
import qualified Data.ListLike.FoldableLL as LL
-- import           Data.ListLike.String
-- import           Data.String (IsString)
import           GHC.Exts (IsList(..), build)
import qualified Data.List as L

{- It would be really nice to wrap the polymorphic vector in a newtype,
derive a Vector instance for it, define FoldableLL, ListLike, Semigroup and Monoid instances for the newtype, and then use it to derive all the instances of the concrete vector types. Unfortunately the combination of type families and multi param type classes seems to make that impossible, even with deriving strategies and standalone deriving.
-}


------ Folds ------

isPrefixOf ::
    (V.Vector v a, Eq (v a))=>
    v a -> v a -> Bool
isPrefixOf = \ needle haystack ->
    V.length needle <= V.length haystack  &&
    needle == V.unsafeSlice 0 (V.length needle) haystack

isSuffixOf ::
    (V.Vector v a, Eq (v a))=>
    v a -> v a -> Bool
isSuffixOf = \ needle haystack ->
    V.length needle <= V.length haystack &&
    needle == V.unsafeSlice (V.length haystack - V.length needle) (V.length needle) haystack

fromListLike :: (V.Vector v a, LL.ListLike la a)=> v a -> la
fromListLike = \ v -> fromListN (V.length v) $ V.toList v
{-# INLINABLE fromListLike #-}

map :: (V.Vector v a, LL.ListLike items item)=> (a -> item) -> v a -> items
map f v = fromListN (V.length v) $ L.map f $ V.toList v
{-# INLINABLE map #-}

mapM ::
    (V.Vector v a, LL.ListLike items item)=>
    (P.Applicative m)=> (a -> m item) -> v a -> m items
mapM = \ func v -> P.fmap (fromListN (V.length v)) $ P.traverse func $ V.toList v
{-# INLINABLE mapM #-}


------ ListLike operations ------

inits :: (V.Vector v a, LL.ListLike vectors (v a))=> v a -> vectors
inits v = fromListN (V.length v + 1) (build (\ f z ->
  let
    loop n =
        V.take n v `f`
        if n < V.length v
        then loop (n + 1)
        else z
  in loop 0))
{-# INLINABLE inits #-}

tails :: (V.Vector v a, LL.ListLike vectors (v a))=> v a -> vectors
tails v = fromListN (V.length v + 1) (build (\ f z ->
  let
    loop n =
        V.drop n v `f`
        if n < V.length v
        then loop (n + 1)
        else z
  in loop 0))
{-# INLINABLE tails #-}

intersperse :: (V.Vector v a)=> a -> v a -> v a
intersperse = \ sep v -> V.fromListN (2 * V.length v - 1) $ L.intersperse sep $ V.toList v
-- Note: V.fromListN (-1) [] = empty
{-# INLINABLE intersperse #-}

groupBy :: (V.Vector v a, LL.ListLike vectors (v a))=> (a -> a -> Bool) -> v a -> vectors
{-^ This definition has more consistent performance at different ratios of run size to input size and for different result types. -}
groupBy = \ eq vec -> fromList (build (\ f z ->
  let
    loop v = case V.uncons v of
        P.Nothing -> z
        P.Just (x, xs) ->
            case count (eq x) xs of
                n -> V.unsafeTake (n + 1) v `f` loop (V.unsafeDrop (n + 1) v)
  in loop vec))
{-# INLINABLE groupBy #-}


sortBy :: (V.Vector v a)=> (a -> a -> P.Ordering) -> v a -> v a
sortBy = \ cmp v -> V.fromListN (V.length v) $ L.sortBy cmp $ V.toList v
{-# INLINABLE sortBy #-}

unlines, unwords ::
    (V.Vector v P.Char, LL.ListLike vectors (v P.Char))=> vectors -> v P.Char
unlines = let eol = V.singleton '\n' in V.concat . L.intersperse eol . toList
unwords = let sp = V.singleton ' ' in V.concat . L.intersperse sp . toList


------ Helpers ------

count :: (V.Vector v a)=> (a -> P.Bool) -> v a -> P.Int
count = \ p xs -> V.foldr (\ x k acc -> if p x then k $! acc + 1 else acc) P.id xs 0
