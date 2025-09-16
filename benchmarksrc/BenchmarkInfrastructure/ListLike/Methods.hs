{-# LANGUAGE FlexibleContexts
           , ScopedTypeVariables
  #-}

module BenchmarkInfrastructure.ListLike.Methods (
bench_ListLike_Int_on,
bench_ListLike_Num_on,
bench_ListLike_on,
bench_ListLike_polymophic_on,
) where

import GHC.Exts (coerce)
import qualified Data.ListLike.Base as LL
import Test.Tasty.Bench
import Data.ListLike.Instances ()
import Data.ListLike.Vector.Vector ()
import Data.Sequence (Seq)
import Data.Vector (Vector)
import Control.DeepSeq
import Control.Exception (evaluate)
import Data.Functor.Const (Const (..))
import Data.Functor.Identity (Identity (..))
import Data.Proxy

type NameProxy = Const String

bench_ListLike_on :: (LL.ListLike list x, NFData list, NFData x, Ord x)=> list -> [Benchmark]
bench_ListLike_on xs =
    bench_head xs :
    bench_last xs :
    bench_uncons xs :
    bench_length xs :
    bench_isPrefixOf_self xs :
    bench_isPrefixOf_prefix xs :
    bench_isSuffixOf_self xs :
    bench_isSuffixOf_suffix xs :
    bench_stripPrefix_prefix xs :
    bench_stripSuffix_suffix xs :
    bench_elemIndex xs :
    bench_elemIndices_List xs :
    bench_findIndex xs :
    bench_findIndices_List xs :
    bench_reverse xs :
    bench_sort xs :
    bench_sortBy_flip_compare xs :
    bench_tail xs :
    bench_init xs :
    bench_drop xs :
    bench_take xs :
    bench_splitAt xs :
    bench_dropWhile xs :
    bench_dropWhileEnd xs :
    bench_takeWhile xs :
    bench_span xs :
    bench_nubBy_eq xs :
    bench_deleteBy_eq xs :
    bench_unionBy_eq_self xs :
    bench_intersectBy_eq_self xs :
    bench_inits_List xs :
    bench_tails_List xs :
    bench_index xs :
    bench_genericLength xs :
    bench_group_List xs :
    bench_groupBy_LT_List xs :
    bench_concatMap_List xs :
    bench_concatMap_Seq xs :
    bench_concatMap_Vector xs :
    []

bench_ListLike_polymophic_on ::
    ( LL.ListLike (t x) x
    , LL.ListLike (t (t x)) (t x)
    , NFData x, NFData (t x), NFData (t (t x))
    , Ord x
    )=> t x -> [Benchmark]
bench_ListLike_polymophic_on xs =
    bench_inits_self xs :
    bench_tails_self xs :
    bench_group_self xs :
    bench_groupBy_LT_self xs :
    []

bench_ListLike_Num_on ::
    forall num nums. (LL.ListLike nums num, Integral num, NFData num, NFData nums)=>
    nums -> [Benchmark]
bench_ListLike_Num_on xs =
    bench_maximum xs :
    bench_minimum xs :
    bench_snoc xs :
    bench_intersperse xs :
    bench_map_rigid xs :
    bench_map_List xs :
    bench_map_Vector xs :
    bench_rigidMap xs :
    bench_map_via_mapM_rigid xs :
    bench_map_via_mapM_List xs :
    bench_rigidMap_via_rigidMapM xs :
    bench_filter xs :
    bench_partition xs :
    (bench "replicate 400 3" $ nf (LL.replicate 400 :: num -> nums) 4) :
    []

bench_ListLike_Int_on :: (LL.ListLike ints Int, NFData ints)=> ints -> [Benchmark]
bench_ListLike_Int_on xs =
    bench_elem xs :
    bench_not_elem xs :
    bench_notElem xs :
    bench_not_notElem xs :
    []



-- empty
-- singleton
-- cons
-- snoc
-- append
bench_head :: (LL.ListLike items item, NFData items, NFData item)=> items -> Benchmark
bench_head = bench "head" . nf LL.head
bench_uncons :: (LL.ListLike items item, NFData items, NFData item)=> items -> Benchmark
bench_uncons = bench "uncons" . nf LL.uncons
bench_tail :: (LL.ListLike items item, NFData items)=> items -> Benchmark
bench_tail = bench "tail" . nf LL.tail
bench_init :: (LL.ListLike items item, NFData items)=> items -> Benchmark
bench_init = bench "init" . nf LL.init
-- null
-- map

bench_map_rigid ::
    forall num nums. (LL.ListLike nums num, Num num, NFData num, NFData nums)=>
    nums -> Benchmark
bench_map_rigid = bench "map rigid (+2)" . nf map_plus_2
  where
    map_plus_2 :: nums -> nums
    map_plus_2 = LL.map (+2)

bench_map_List ::
    forall num nums. (LL.ListLike nums num, Num num, NFData num, NFData nums)=>
    nums -> Benchmark
bench_map_List = bench "map List (+2)" . nf map_plus_2
  where
    map_plus_2 :: nums -> [num]
    map_plus_2 = LL.map (+2)

bench_map_Vector ::
    forall num nums. (LL.ListLike nums num, Num num, NFData num, NFData nums)=>
    nums -> Benchmark
bench_map_Vector = bench "map Vector (+2)" . nf map_plus_2
  where
    map_plus_2 :: nums -> Vector num
    map_plus_2 = LL.map (+2)

bench_rigidMap ::
    forall num nums. (LL.ListLike nums num, Num num, NFData num, NFData nums)=>
    nums -> Benchmark
bench_rigidMap = bench "rigidMap (+2)" . nf (LL.rigidMap (+2))

bench_map_via_mapM_rigid ::
    forall num nums. (LL.ListLike nums num, Num num, NFData num, NFData nums)=>
    nums -> Benchmark
bench_map_via_mapM_rigid = bench "map via mapM rigid (+2)" . nf map_plus_2
  where
    map_plus_2 :: (LL.ListLike nums num)=> nums -> nums
    map_plus_2 = map_via_mapM (+2)

bench_map_via_mapM_List ::
    forall num nums. (LL.ListLike nums num, Num num, NFData num, NFData nums)=>
    nums -> Benchmark
bench_map_via_mapM_List = bench "map via mapM List (+2)" . nf map_plus_2
  where
    map_plus_2 :: (LL.ListLike nums num)=> nums -> [num]
    map_plus_2 = map_via_mapM (+2)

bench_rigidMap_via_rigidMapM ::
    (LL.ListLike nums num, Num num, NFData num, NFData nums)=> nums -> Benchmark
bench_rigidMap_via_rigidMapM =
    bench "rigidMap via ridigMapM (+2)" . nf (rigidMap_via_rigidMapM (+2))

-- concat
-- concatMap
bench_concatMap_List :: forall items item.
    ( LL.ListLike items item, NFData items, NFData item
    )=> items -> Benchmark
bench_concatMap_List =
    bench "concatMap (replicate 3) to List" .
    nf (LL.concatMap (LL.replicate 3) :: items -> [item])

bench_concatMap_Seq :: forall items item.
    ( LL.ListLike items item, NFData items, NFData item
    )=> items -> Benchmark
bench_concatMap_Seq =
    bench "concatMap (replicate 3) to Seq" .
    nf (LL.concatMap (LL.replicate 3) :: items -> Seq item)

bench_concatMap_Vector :: forall items item.
    ( LL.ListLike items item, NFData items, NFData item
    )=> items -> Benchmark
bench_concatMap_Vector =
    bench "concatMap (replicate 3) to Vector" .
    nf (LL.concatMap (LL.replicate 3) :: items -> Vector item)

-- rigidConcatMap
-- any
-- all
-- elem
bench_elem, bench_notElem, bench_not_elem, bench_not_notElem ::
    (LL.ListLike nums num, Num num, NFData num, NFData nums, Eq num)=> nums -> Benchmark
bench_elem = bench "elem 4000" . nf (LL.elem 4000)
bench_not_elem = bench "not . elem 4000" . nf (not . LL.elem 4000)
bench_notElem = bench "notElem 4000" . nf (LL.notElem 4000)
bench_not_notElem = bench "not . notElem 4000" . nf (not . LL.notElem 4000)
bench_find ::
    (LL.ListLike items item, NFData items, NFData item, Eq item)=> items -> Benchmark
bench_find xs = env
    (evaluate $ force $ xs `LL.index` (LL.length xs `div` 2)) $ \ x ->
    bench "find (== midpoint)" $ nf (LL.find (== x)) xs

-- replicate
bench_last :: (LL.ListLike items item, NFData items, NFData item)=> items -> Benchmark
bench_last = bench "last" . nf LL.last
bench_length :: (LL.ListLike list x)=> list -> Benchmark
bench_length = bench "length" . nf LL.length
bench_maximum :: (LL.ListLike items item, Ord item, NFData item)=> items -> Benchmark
bench_maximum = bench "maximum" . nf LL.maximum
bench_minimum :: (LL.ListLike items item, Ord item, NFData item)=> items -> Benchmark
bench_minimum = bench "minimum" . nf LL.minimum

bench_isPrefixOf_self :: (LL.ListLike items item, Eq item, NFData item)=> items -> Benchmark
bench_isPrefixOf_self = bench "isPrefixOf x x" . nf (\ xs -> LL.isPrefixOf xs xs)

bench_isPrefixOf_prefix :: (LL.ListLike items item, Eq item, NFData item, NFData items)=> items -> Benchmark
bench_isPrefixOf_prefix ys = env
  (evaluate $ force $ LL.take (LL.length ys `div` 2) ys) $ \ needle ->
  bench "isPrefixOf x/2 x" $ nf (LL.isPrefixOf needle) ys

-- isSuffixOf
bench_isSuffixOf_self :: (LL.ListLike items item, Eq item, NFData item)=> items -> Benchmark
bench_isSuffixOf_self = bench "isSuffixOf x x" . nf (\ xs -> LL.isSuffixOf xs xs)

bench_isSuffixOf_suffix :: (LL.ListLike items item, Eq item, NFData item, NFData items)=> items -> Benchmark
bench_isSuffixOf_suffix ys = env
  (evaluate $ force $ LL.drop (LL.length ys `div` 2) ys) $ \ needle ->
  bench "isSuffixOf x/2 x" $ nf (LL.isSuffixOf needle) ys

-- isInfixOf

bench_stripPrefix_prefix ::
    (LL.ListLike items item, Eq item, NFData item, NFData items)=> items -> Benchmark
bench_stripPrefix_prefix ys = env
  (evaluate $ force $ LL.take (LL.length ys `div` 2) ys) $ \ needle ->
  bench "stripPrefix x/2 x" $ nf (LL.stripPrefix needle) ys

bench_stripSuffix_suffix ::
    (LL.ListLike items item, Eq item, NFData item, NFData items)=> items -> Benchmark
bench_stripSuffix_suffix ys = env
  (evaluate $ force $ LL.take (LL.length ys `div` 2) ys) $ \ needle ->
  bench "stripSuffix x/2 x" $ nf (LL.stripSuffix needle) ys

bench_snoc :: (LL.ListLike nums num, Num num, NFData num, NFData nums)=> nums -> Benchmark
bench_snoc = bench "snoc" . nf (`LL.snoc` 2)
bench_reverse :: forall list x. (LL.ListLike list x, NFData list)=> list -> Benchmark
bench_reverse = bench "reverse" . nf LL.reverse
bench_intersperse ::
    (LL.ListLike nums num, Num num, NFData num, NFData nums)=> nums -> Benchmark
bench_intersperse = bench "intersperse 123" . nf (LL.intersperse 123)
bench_take :: forall list x. (LL.ListLike list x, NFData list)=> list -> Benchmark
bench_take xs =
    env (evaluate $ force $ LL.length xs `div` 2) $ \ n ->
    bench "take half" $ nf (LL.take n) xs
bench_drop :: forall list x. (LL.ListLike list x, NFData list)=> list -> Benchmark
bench_drop = bench "drop 5000" . nf (LL.drop 5000)
bench_splitAt :: forall list x. (LL.ListLike list x, NFData list)=> list -> Benchmark
bench_splitAt = bench "splitAt 5000" . nf (LL.splitAt 5000)

bench_dropWhile ::
    (LL.ListLike items item, NFData items, NFData item, Eq item)=> items -> Benchmark
bench_dropWhile xs =
    env (evaluate $ force $ xs `LL.index` (LL.length xs `div` 2)) $ \ x ->
    bench "dropWhile (/= midpoint)" $ nf (LL.dropWhile (/= x)) xs

bench_dropWhileEnd ::
    (LL.ListLike items item, NFData items, NFData item, Eq item)=> items -> Benchmark
bench_dropWhileEnd xs =
    env (evaluate $ force $ xs `LL.index` (LL.length xs `div` 2)) $ \ x ->
    bench "dropWhileEnd (/= midpoint)" $ nf (LL.dropWhileEnd (/= x)) xs

bench_takeWhile ::
    (LL.ListLike items item, NFData items, NFData item, Eq item)=> items -> Benchmark
bench_takeWhile xs =
    env (evaluate $ force $ xs `LL.index` (LL.length xs `div` 2)) $ \ x ->
    bench "takeWhile (/= midpoint)" $ nf (LL.takeWhile (/= x)) xs

-- break
bench_span ::
    (LL.ListLike items item, NFData items, NFData item, Eq item)=> items -> Benchmark
bench_span xs =
    env (evaluate $ force $ xs `LL.index` (LL.length xs `div` 2)) $ \ x ->
    bench "span (/= midpoint)" $ nf (LL.span (/= x)) xs

bench_group_List ::
    forall items item.
    (LL.ListLike items item, NFData items, Eq item)=>
    items -> Benchmark
bench_group_List = bench "List group" . nf (LL.group :: items -> [items])

bench_group_self ::
    forall t a.
    ( LL.ListLike (t a) a
    , LL.ListLike (t (t a)) (t a), NFData (t (t a))
    , NFData a, Eq a
    )=> t a -> Benchmark
bench_group_self = bench "self group" . nf (LL.group :: t a -> t (t a))

bench_groupBy_LT_List ::
    forall items item.
    (LL.ListLike items item, NFData items, Ord item)=>
    items -> Benchmark
bench_groupBy_LT_List = bench "List groupBy (<)" . nf (LL.groupBy (<) :: items -> [items])

bench_groupBy_LT_self ::
    forall t a.
    ( LL.ListLike (t a) a
    , LL.ListLike (t (t a)) (t a), NFData (t (t a))
    , NFData a, Ord a
    )=> t a -> Benchmark
bench_groupBy_LT_self = bench "self groupBy (<)" . nf (LL.groupBy (<) :: t a -> t (t a))

-- inits
bench_inits_List :: forall list x. (LL.ListLike list x, NFData list)=> list -> Benchmark
bench_inits_List = bench "List inits" . nf (LL.inits :: list -> [list])

bench_inits_self ::
    forall t x.
    (LL.ListLike (t x) x, LL.ListLike (t (t x)) (t x), NFData (t x), NFData (t (t x)))=>
    t x -> Benchmark
bench_inits_self = bench "self inits" . nf (LL.inits :: t x -> t (t x))

bench_tails_List :: forall list x. (LL.ListLike list x, NFData list)=> list -> Benchmark
bench_tails_List = bench "List tails" . nf (LL.tails :: list -> [list])

bench_tails_self ::
    forall t x.
    (LL.ListLike (t x) x, LL.ListLike (t (t x)) (t x), NFData (t x), NFData (t (t x)))=>
    t x -> Benchmark
bench_tails_self = bench "self tails" . nf (LL.tails :: t x -> t (t x))

bench_filter ::
    forall num nums. (LL.ListLike nums num, Integral num, NFData num, NFData nums)=>
    nums -> Benchmark
bench_filter = bench "filter odd" . nf (LL.filter odd)

bench_partition ::
    forall num nums. (LL.ListLike nums num, Integral num, NFData num, NFData nums)=>
    nums -> Benchmark
bench_partition = bench "partition odd" . nf (LL.partition odd)

bench_index :: forall list x. (LL.ListLike list x, NFData x)=> list -> Benchmark
bench_index xs = env (evaluate $ force $ LL.length xs `div` 2) $ \ n ->
    bench "index midpoint" $ nf (`LL.index` n) xs

bench_elemIndex ::
    forall list x. (LL.ListLike list x, NFData x, Ord x)=> list -> Benchmark
bench_elemIndex xs = env
    (evaluate $ force $ LL.index xs (LL.length xs `div` 4)) $ \ x ->
    bench "elemIndex quarter-point" $ nf (LL.elemIndex x) xs

bench_elemIndices_List ::
    forall list x. (LL.ListLike list x, NFData x, Ord x)=> list -> Benchmark
bench_elemIndices_List xs = env
    (evaluate $ force $ LL.index xs (LL.length xs `div` 4)) $ \ x ->
    bench "elemIndices quarter-point" $ nf (LL.elemIndices x :: list -> [Int]) xs


bench_findIndex ::
    forall list x. (LL.ListLike list x, NFData x, Ord x)=> list -> Benchmark
bench_findIndex xs = env
    (evaluate $ force $ LL.index xs (LL.length xs `div` 4)) $ \ x ->
    bench "findIndex GE quarter-point" $ nf (LL.findIndex (>= x)) xs

bench_findIndices_List ::
    forall list x. (LL.ListLike list x, NFData x, Ord x)=> list -> Benchmark
bench_findIndices_List xs = env
    (evaluate $ force $ LL.index xs (LL.length xs `div` 4)) $ \ x ->
    bench "findIndices GE quarter-point" $ nf (LL.findIndices (>= x) :: list -> [Int]) xs

-- sequence
-- mapM
-- rigidMapM
bench_nub ::
    forall items item. (LL.ListLike items item, Eq item, NFData items)=>
    items -> Benchmark
bench_nub = bench "nub" . nf LL.nub
-- delete
-- deleteFirsts
-- union
-- intersect
bench_sort ::
    forall items item. (LL.ListLike items item, Ord item, NFData items)=>
    items -> Benchmark
bench_sort = bench "sort" . nf LL.sort
-- toList'
-- fromList'
-- fromListLike

bench_nubBy_eq ::
    forall items item. (LL.ListLike items item, Eq item, NFData items)=>
    items -> Benchmark
bench_nubBy_eq = bench "nubBy (==)" . nf (LL.nubBy (==))

-- deleteBy
bench_deleteBy_eq ::
    forall list x. (LL.ListLike list x, NFData list, NFData x, Eq x)=> list -> Benchmark
bench_deleteBy_eq xs = env (evaluate $ force $ xs `LL.index` (LL.length xs `div` 2)) $ \ x ->
    bench "deleteBy (==) midpoint" $ nf (LL.deleteBy (==) x) xs

-- deleteFirstsBy

bench_unionBy_eq_self ::
    forall list x. (LL.ListLike list x, NFData list, NFData x, Eq x)=> list -> Benchmark
bench_unionBy_eq_self =
    bench "unionBy (==) x x" . nf (\ xs -> LL.unionBy (==) xs xs)

bench_intersectBy_eq_self ::
    forall list x. (LL.ListLike list x, NFData list, NFData x, Eq x)=> list -> Benchmark
bench_intersectBy_eq_self =
    bench "intersectBy (==) x x" . nf (\ xs -> LL.intersectBy (==) xs xs)

bench_sortBy_flip_compare ::
    forall items item. (LL.ListLike items item, Ord item, NFData items)=>
    items -> Benchmark
bench_sortBy_flip_compare = bench "sortBy (flip compare)" . nf (LL.sortBy (flip compare))
-- insertBy
bench_genericLength :: forall list x. (LL.ListLike list x)=> list -> Benchmark
bench_genericLength = bench "genericLength" . nf (LL.genericLength :: list -> Int)
-- genericDrop
-- genericTake
-- genericSplitAt
-- genericReplicate

map_via_mapM ::
    forall items item items' item'.
    ( LL.ListLike items item, LL.ListLike items' item')=>
    (item -> item') -> items -> items'
map_via_mapM = coerce (LL.mapM :: (item -> Identity item') -> items -> Identity items')
{-# INLINE map_via_mapM #-}

rigidMap_via_rigidMapM ::
    forall items item. ( LL.ListLike items item)=>
    (item -> item) -> items -> items
rigidMap_via_rigidMapM =
    coerce (LL.rigidMapM :: (item -> Identity item) -> items -> Identity items)
{-# INLINE rigidMap_via_rigidMapM #-}
