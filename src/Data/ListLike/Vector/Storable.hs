{-# LANGUAGE MultiParamTypeClasses
            ,TypeFamilies
            ,FlexibleInstances #-}
{-# OPTIONS -Wno-orphans #-}

module Data.ListLike.Vector.Storable ()

where

import           Prelude as P
import qualified Data.Vector.Storable as V
import           Data.Vector.Storable ((!))
import qualified Data.ListLike.Vector.Extra as Extra
import           Data.ListLike.Base
import           Data.ListLike.FoldableLL
import           Data.ListLike.String
import           Data.String (IsString)

import           Foreign.Storable (Storable)


instance Storable a => FoldableLL (V.Vector a) a where
    foldl = V.foldl
    foldl' = V.foldl'
    foldl1 = V.foldl1
    foldr = V.foldr
    foldr' = V.foldr'
    foldr1 = V.foldr1

instance Storable a => ListLike (V.Vector a) a where
    empty = V.empty
    singleton = V.singleton
    cons = V.cons
    snoc = V.snoc
    append = mappend
    head = V.head
    last = V.last
    tail = V.tail
    init = V.init
    null = V.null
    length = V.length
    map = Extra.map
    rigidMap = V.map
    reverse = V.reverse
    intersperse = Extra.intersperse
    sortBy = Extra.sortBy
    concat = V.concat . toList
    rigidConcatMap = V.concatMap
    any = V.any
    all = V.all
    maximum = V.maximum
    minimum = V.minimum
    replicate = V.replicate
    take = V.take
    drop = V.drop
    --splitAt =
    takeWhile = V.takeWhile
    dropWhile = V.dropWhile
    span = V.span
    break = V.break
    --group = groupBy (==)
    inits = Extra.inits
    tails = Extra.tails
    isPrefixOf = Extra.isPrefixOf
    isSuffixOf = Extra.isSuffixOf
    elem = V.elem
    notElem = V.notElem
    find = V.find
    filter = V.filter
    index = (!)
    findIndex = V.findIndex
    toList' = V.toList
    fromList' = V.fromList
    fromListLike = Extra.fromListLike
    groupBy = Extra.groupBy
    genericLength = fromInteger . fromIntegral . V.length
    genericTake i = V.take (fromIntegral i)
    genericDrop i = V.drop (fromIntegral i)
    --genericSplitAt i =
    genericReplicate i = V.replicate (fromIntegral i)

    sequence  = fmap fromList . P.sequenceA  . toList
    mapM = Extra.mapM

instance IsString (V.Vector Char) where
    fromString = fromList

instance StringLike (V.Vector Char) where
    toString = toList
    --words =
    --lines =
    unwords = let sp = V.singleton ' ' in V.concat . intersperse sp . toList
    unlines = let eol = V.singleton '\n' in V.concat . intersperse eol . toList
