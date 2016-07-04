-- | Work in progress.
{-# LANGUAGE MultiParamTypeClasses
            ,FlexibleInstances #-}

module Data.ListLike.Chars

where

import           Prelude as P
import           Control.DeepSeq
import           Control.Monad
import           Data.String as String (IsString(fromString))
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as TI
import qualified Data.Text.Lazy.Builder as Builder
import           Data.Text.Encoding (decodeUtf8)
import           Data.ListLike.Base as LL
import           Data.ListLike.FoldableLL
import           Data.ListLike.IO
import           Data.ListLike.String as LL
import           Data.ListLike.Text

newtype Chars = Chars {unChars :: Builder.Builder} deriving (Show, Eq, Ord)

instance Monoid Chars where
    mempty = Chars mempty
    mappend a b = Chars $ mappend (unChars a) (unChars b)

instance String.IsString Chars where
  fromString = Chars . String.fromString

instance FoldableLL Chars Char where
    foldl f r0 = Data.ListLike.FoldableLL.foldl f r0 . unChars
    foldr f r0 = Data.ListLike.FoldableLL.foldr f r0 . unChars

instance ListLike Chars Char where
    singleton = Chars . Builder.singleton
    uncons b = case LL.uncons (unChars b) of
                 Nothing -> Nothing
                 Just (c, s) -> Just (c, Chars s)
    null = LL.null . unChars

instance ListLikeIO Chars Char where
    hGetLine h = Chars <$> hGetLine h
    hGetContents h =  Chars <$> hGetContents h
    hGet h n = Chars <$> hGet h n
    hGetNonBlocking h n = Chars <$> hGetNonBlocking h n
    hPutStr h = hPutStr h . unChars

instance StringLike Chars where
    toString = toString . unChars
    fromString = Chars . LL.fromString

instance NFData Chars where
    rnf = rnf . unChars
