{-# LANGUAGE NoImplicitPrelude, MultiParamTypeClasses, FlexibleInstances #-}
-- | 'Data.ListLike.ListLike' instances for 'Data.DList.DList'
module Data.ListLike.DList () where
import qualified Prelude as P
import Data.ListLike
import Data.ListLike.Base
import Data.ListLike.FoldableLL
import Data.ListLike.IO
import Data.ListLike.String
import Data.DList (DList(..))
import qualified Data.DList as D
import Data.Foldable (Foldable)
import qualified Data.Foldable as F
import Data.Traversable (Traversable)
import qualified Data.Traversable as T
import Data.String (IsString)
import qualified Data.String as S
import Control.Category
import Data.Char (Char(..))

instance FoldableLL (DList a) a where
  foldl = F.foldl
  foldr = D.foldr
  foldl1 = F.foldl1
  foldr1 = F.foldr1
  foldl' = F.foldl'
  foldr' = F.foldr'

instance ListLike (DList a) a where
  empty = D.empty
  singleton = D.singleton
  cons = D.cons
  snoc = D.snoc
  append = D.append
  head = D.head
  tail = D.tail
  rigidMap = D.map
  null = null . D.toList
  toList = D.toList
  fromList = D.fromList
  replicate = D.replicate

instance StringLike (DList Char) where
  toString = D.toList
  fromString = D.fromList
  lines = map D.fromList . S.lines . D.toList
  words = map D.fromList . S.words . D.toList
  unlines = D.fromList . S.unlines . map D.toList
  unwords = D.fromList . S.unwords . map D.toList
