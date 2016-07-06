-- | Work in progress.
{-# LANGUAGE CPP
            ,MultiParamTypeClasses
            ,FlexibleInstances #-}

module Data.ListLike.Chars

where

#if !MIN_VERSION_base(4,8,0)
import           Control.Applicative
#endif
import           Control.DeepSeq
import           Control.Monad
import           Data.String as String (IsString(fromString))
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as TI
import qualified Data.Text.Lazy.Builder as Builder
import           Data.Text.Encoding (decodeUtf8)
import           Data.ListLike.Base as LL
import           Data.ListLike.FoldableLL as LL
import           Data.ListLike.IO
import           Data.ListLike.String as LL
import           Data.ListLike.Text

data Chars
    = B Builder.Builder
    | T T.Text
    deriving (Show, Eq, Ord)

builder :: Chars -> Builder.Builder
builder (B x) = x
builder (T s) = Builder.fromLazyText s
{-# INLINE builder #-}

instance Monoid Chars where
    mempty = B mempty
    mappend a b = B $ mappend (builder a) (builder b)

instance String.IsString Chars where
  fromString = B . String.fromString

instance FoldableLL Chars Char where
    foldl f r0 (B b) = LL.foldl f r0 . Builder.toLazyText $ b
    foldl f r0 (T s) = LL.foldl f r0 $ s
    foldr f r0 (B b) = LL.foldr f r0 . Builder.toLazyText $ b
    foldr f r0 (T s) = LL.foldr f r0 $ s
    --
    foldl' f r0 (B b) = LL.foldl' f r0 . Builder.toLazyText $ b
    foldl' f r0 (T s) = LL.foldl' f r0 $ s
    foldl1 f (B b) = LL.foldl1 f . Builder.toLazyText $ b
    foldl1 f (T s) = LL.foldl1 f $ s
    foldr' f r0 (B b) = LL.foldr' f r0 . Builder.toLazyText $ b
    foldr' f r0 (T s) = LL.foldr' f r0 $ s
    foldr1 f (B b) = LL.foldr1 f . Builder.toLazyText $ b
    foldr1 f (T s) = LL.foldr1 f $ s

instance ListLike Chars Char where
    singleton = B . Builder.singleton
    uncons (B b) =
        case LL.uncons (Builder.toLazyText b) of
          Nothing -> Nothing
          Just (c, s) -> Just (c, T s)
    uncons (T s) =
        case LL.uncons s of
          Nothing -> Nothing
          Just (c, s') -> Just (c, T s')
    null (B b) = LL.null . Builder.toLazyText $ b
    null (T t) = LL.null t

instance ListLikeIO Chars Char where
    hGetLine h = T <$> hGetLine h
    hGetContents h = T <$> hGetContents h
    hGet h n = T <$> hGet h n
    hGetNonBlocking h n = T <$> hGetNonBlocking h n
    hPutStr h (B b) = hPutStr h . Builder.toLazyText $ b
    hPutStr h (T s) = hPutStr h $ s

instance StringLike Chars where
    toString (B b) = toString . Builder.toLazyText $ b
    toString (T s) = toString $ s
    fromString = B . Builder.fromLazyText . LL.fromString

instance NFData Chars where
    rnf (B b) = rnf . Builder.toLazyText $ b
    rnf (T s) = rnf s
