{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}  -- for GHC >= 9.4

{-# OPTIONS -Wno-orphans #-}

-- | ListLike instance for any type supporting the @Data.Vector.Generic@
-- interface.  To avoid collisions with other Vector instances, this module
-- must be imported directly.
module Data.ListLike.Vector.Generic ()

where

import           Prelude as P
import qualified Data.Vector.Generic as V
import           Data.Vector.Generic ((!))
import qualified Data.ListLike.Vector.Extra as Extra
import           Data.ListLike.Base
import           Data.ListLike.FoldableLL
import           Data.ListLike.String
import           Data.String (IsString)
import           GHC.Exts (IsList(..))

instance {-# OVERLAPPABLE #-} V.Vector v a => FoldableLL (v a) a where
    foldl = V.foldl
    foldl' = V.foldl'
    foldl1 = V.foldl1
    foldr = V.foldr
    foldr' = V.foldr'
    foldr1 = V.foldr1

#if 0
instance {-# OVERLAPPABLE #-} (Monoid (v a), Eq (v a), V.Vector v a) => IsList (v a) where
    type Item (v a) = a
    toList = V.toList
    fromList = V.fromList
    fromListN = V.fromListN
#endif

instance {-# OVERLAPPABLE #-} (IsList (v a), Item (v a) ~ a, Monoid (v a), Eq (v a), V.Vector v a) => ListLike (v a) a where
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
    --group =
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

instance (Eq (v Char), V.Vector v Char) => IsString (v Char) where
    fromString = V.fromList

instance (Eq (v Char), V.Vector v Char) => StringLike (v Char) where
    toString = V.toList
    --words =
    --lines =
    unwords = Extra.unwords
    unlines = Extra.unlines
