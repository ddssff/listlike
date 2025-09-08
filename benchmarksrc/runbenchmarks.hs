{-# LANGUAGE FlexibleContexts
           , ScopedTypeVariables
  #-}

import BenchmarkInfrastructure
import BenchmarkInfrastructure.FoldableLL.Methods
import BenchmarkInfrastructure.ListLike.Methods
import qualified Data.ListLike as LL
import Test.Tasty.Bench
import Control.DeepSeq
import Control.Exception (evaluate)
import Data.ByteString (ByteString)
import Data.Vector (Vector)
import qualified Data.Vector.Storable as Storable (Vector)
import qualified Data.Vector.Unboxed as Unboxed (Vector)
import Data.Sequence (Seq)
import Data.Functor.Const
import Data.Proxy
import Data.Word (Word8)
import Data.Semigroup (stimes)

main :: IO ()
main = defaultMain $ mempty
    -- <> list_benchmarks
    <> (runBenchmarksOnInts (Const "MyList Int" :: NameProxy (MyList Int)) all_benchmarks : [])
    <> (runBenchmarksOnInts (Const "MySeq Int" :: NameProxy (MySeq Int)) all_benchmarks : [])
    <> (runBenchmarksOnInts (Const "MyVector Int" :: NameProxy (MyVector Int)) all_benchmarks : [])
    -- <> vector_benchmarks
    -- <> unboxed_benchmarks
    -- <> storable_benchmarks
    -- <> seq_benchmarks
    -- <> byteString_benchmarks


type NameProxy = Const String

runBenchmarksOnInts ::(LL.ListLike ints Int, NFData ints)=>
    NameProxy ints -> (ints -> [Benchmark]) -> Benchmark
runBenchmarksOnInts nameProxy@(Const name) benchmarks = env
    (evaluate $ force $ LL.fromList ([1..5000] <> [5000, 4999..1])`asProxyTypeOf` nameProxy)
    (bgroup name . benchmarks)

runBenchmarksOnBytes :: (LL.ListLike bytes Word8, NFData bytes)=>
    NameProxy bytes -> (bytes -> [Benchmark]) -> Benchmark
runBenchmarksOnBytes nameProxy@(Const name) benchmarks = env
    (evaluate $ force $ LL.fromList (stimes 200 ([0..255] <> [255, 254..0]))`asProxyTypeOf` nameProxy)
    (bgroup name . benchmarks)

nonpolymorphic_benchmarks ::
    (LL.ListLike ints Int, NFData ints)=> ints -> [Benchmark]
nonpolymorphic_benchmarks = mempty
    <> bench_FoldableLL_on
    <> bench_FoldableLL_Num_on
    <> bench_ListLike_on
    <> bench_ListLike_Num_on
    <> bench_ListLike_Int_on

all_benchmarks ::
    ( LL.ListLike (t Int) Int, NFData (t Int)
    , LL.ListLike (t (t Int)) (t Int), NFData (t (t Int))
    )=> t Int -> [Benchmark]
all_benchmarks = mempty
    <> nonpolymorphic_benchmarks
    <> bench_ListLike_polymophic_on

byte_benchmarks ::
    (LL.ListLike bytes Word8, NFData bytes)=> bytes -> [Benchmark]
byte_benchmarks = mempty
    <> bench_FoldableLL_on
    <> bench_FoldableLL_Num_on
    <> bench_ListLike_Num_on
    <> bench_ListLike_on

byteString_benchmarks =
    runBenchmarksOnBytes (Const "ByteString" :: NameProxy ByteString) byte_benchmarks : []

list_benchmarks =
    runBenchmarksOnInts (Const "[Int]" :: NameProxy [Int]) all_benchmarks : []

vector_benchmarks =
    runBenchmarksOnInts (Const "Vector Int" :: NameProxy (Vector Int)) all_benchmarks : []

unboxed_benchmarks =
    runBenchmarksOnInts (Const "Unboxed Vector Int" :: NameProxy (Unboxed.Vector Int))
        nonpolymorphic_benchmarks : []

storable_benchmarks =
    runBenchmarksOnInts (Const "Storable Vector Int" :: NameProxy (Storable.Vector Int))
        nonpolymorphic_benchmarks : []

seq_benchmarks =
    runBenchmarksOnInts (Const "Seq Int" :: NameProxy (Seq Int))
        nonpolymorphic_benchmarks : []
