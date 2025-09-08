{-# LANGUAGE FlexibleContexts
           , ScopedTypeVariables
           , TypeOperators
  #-}

module BenchmarkInfrastructure.FoldableLL.Methods (
bench_FoldableLL_on,
bench_FoldableLL_Num_on,
) where

import Test.Tasty.Bench
import qualified Data.ListLike.FoldableLL as LL
import qualified Data.ListLike.Base as LL
import GHC.Exts (IsList(..), coerce)
import Control.DeepSeq
import Control.Exception (evaluate)
import qualified Data.Monoid as Monoid
import qualified Data.Semigroup as Semigroup


bench_FoldableLL_on :: (LL.ListLike items item, NFData item)=> items -> [Benchmark]
bench_FoldableLL_on xs =
       (bench "head via foldr" $ nf head_via_foldr xs) :
       (bench "head via foldMap" $ nf head_via_foldMap xs) :
       (bench "last via foldl" $ nf last_via_foldl xs) :
       (bench "last via foldMap" $ nf last_via_foldMap xs) :
       (bench "length via foldl'" $ nf length_via_foldl' xs) :
       (bench "length via foldMap" $ nf length_via_foldMap xs) :
       []

bench_FoldableLL_Num_on ::
    (LL.ListLike nums num, Item nums ~ num, Num num, Ord num, NFData num, NFData nums)=>
    nums -> [Benchmark]
bench_FoldableLL_Num_on xs =
       (bench "sum via foldMap" $ nf sum_via_foldMap xs) :
       (bench "sum via foldl'" $ nf sum_via_foldl' xs) :
       (bench "any (> 250) via foldMap" $ nf (any_via_foldMap (> 250)) xs) :
       (bench "any (> 250) via foldr" $ nf (any_via_foldr (> 250)) xs) :
       (bench "all (< 250) via foldMap" $ nf (all_via_foldMap (< 250)) xs) :
       (bench "all (< 250) via foldr" $ nf (all_via_foldr (< 250)) xs) :
       []


head_via_foldr :: (LL.FoldableLL list item)=> list -> item
head_via_foldr = LL.foldr (\ x _ -> x) (error "head")

head_via_foldMap :: (LL.FoldableLL list item)=> list -> item
head_via_foldMap =
    maybe (error "head") id . Monoid.getFirst . LL.foldMap (Monoid.First . Just)

last_via_foldl :: (LL.FoldableLL list item)=> list -> item
last_via_foldl = LL.foldl (\ _ x -> x) (error "last")

last_via_foldMap :: (LL.FoldableLL list item)=> list -> item
last_via_foldMap =
    maybe (error "last") id . Monoid.getLast . LL.foldMap (Monoid.Last . Just)

length_via_foldl' :: (LL.FoldableLL list x)=> list -> Int
length_via_foldl' = LL.foldl' (\ len _ -> len + 1) 0

length_via_foldMap :: (LL.FoldableLL list x)=> list -> Int
length_via_foldMap = Semigroup.getSum . LL.foldMap (\_-> Semigroup.Sum 1)

all_via_foldMap ::
    forall items item. (LL.FoldableLL items item)=> (item -> Bool) -> items -> Bool
all_via_foldMap = coerce (LL.foldMap :: (item -> Monoid.All) -> items -> Monoid.All)

any_via_foldMap ::
    forall items item. (LL.FoldableLL items item)=> (item -> Bool) -> items -> Bool
any_via_foldMap = coerce (LL.foldMap :: (item -> Monoid.Any) -> items -> Monoid.Any)

all_via_foldr :: (LL.FoldableLL items item)=> (item -> Bool) -> items -> Bool
all_via_foldr p = LL.foldr (\ x b -> p x && b) True

any_via_foldr :: (LL.FoldableLL items item)=> (item -> Bool) -> items -> Bool
any_via_foldr p = LL.foldr (\ x b -> p x || b) False

sum_via_foldMap :: (LL.FoldableLL nums num, Num num)=> nums -> num
sum_via_foldMap = Monoid.getSum . LL.foldMap Monoid.Sum

sum_via_foldl' :: (LL.FoldableLL nums num, Num num)=> nums -> num
sum_via_foldl' = LL.foldl' (+) 0
