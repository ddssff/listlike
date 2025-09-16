{-# LANGUAGE ScopedTypeVariables
            ,TypeFamilies
            ,MultiParamTypeClasses
            ,FunctionalDependencies
            ,FlexibleInstances
            ,BangPatterns
            ,FlexibleContexts
            ,ConstraintKinds
            ,CPP #-}
{-# LANGUAGE TypeOperators #-}  -- for GHC >= 9.4

{-
Copyright (C) 2007 John Goerzen <jgoerzen@complete.org>

All rights reserved.

For license and copyright information, see the file COPYRIGHT

-}

{- |
   Module     : Data.ListLike.Base
   Copyright  : Copyright (C) 2007 John Goerzen
   License    : BSD3

   Maintainer : David Fox <dsf@seereason.com>, Andreas Abel
   Stability  : stable
   Portability: portable

Generic operations over list-like structures

Written by John Goerzen, jgoerzen\@complete.org
-}

module Data.ListLike.Base
    (
    ListLike(..), ListOps,
    toList, fromList,
    InfiniteListLike(..),
    zip, zipWith, sequence_
    ) where

import Prelude
  ( Applicative(..), Bool(..), Char, Eq(..), Int, Integer, Integral
  , Maybe(..), Monad, Monoid(..), Num(..), Ord(..), Ordering(..)
  , ($), ($!), (.), (&&), (||), (++), asTypeOf, error, flip, fst, snd
  , id, maybe, max, min, not, otherwise
  , fmap, sequenceA, traverse
  )
#if MIN_VERSION_base(4,17,0)
import Data.Type.Equality -- GHC 9.4: type equality ~ is just an operator now
#endif

import qualified Data.List as L
import Data.ListLike.FoldableLL
    ( FoldableLL(foldr, foldr1, foldl, foldl'), fold, foldMap, sequence_ )
import qualified Control.Applicative as A
import Data.Monoid ( All(All, getAll), Any(Any, getAny) )
import Data.Semigroup ( stimes )
import Data.Maybe ( fromMaybe, isJust, listToMaybe )
import GHC.Exts (IsList(Item, fromList, fromListN, toList), build)

{- | The class implementing list-like functions.

It is worth noting that types such as 'Data.Map.Map' can be instances of
'ListLike'.  Due to their specific ways of operating, they may not behave
in the expected way in some cases.  For instance, 'cons' may not increase
the size of a map if the key you have given is already in the map; it will
just replace the value already there.

Implementators must define at least:

* singleton

* head

* tail

* null or genericLength
-}
class (IsList full, item ~ Item full, FoldableLL full item, Monoid full) =>
    ListLike full item | full -> item where

    ------------------------------ Creation
    {- | The empty list -}
    empty :: full
    empty = mempty

    {- | Creates a single-element list out of an element -}
    singleton :: item -> full

    ------------------------------ Basic Functions

    {- | Like (:) for lists: adds an element to the beginning of a list -}
    cons :: item -> full -> full
    cons item l = append (singleton item) l

    {- | Adds an element to the *end* of a 'ListLike'. -}
    snoc :: full -> item -> full
    snoc l item = append l (singleton item)

    {- | Combines two lists.  Like (++). -}
    append :: full -> full -> full
    append = mappend

    {- | Extracts the first element of a 'ListLike'. -}
    head :: full -> item
    -- MyList 6 ns; MySeq 13 ns; MyVector 14 ns
    head = maybe (error "head") fst . uncons

    -- head was defined in terms of head and foldr may be defined in terms of head so these maay be problematic. But index apparently is fastest:

    -- MyList 6 ns; MySeq 7 ns; MyVector 7 ns
    -- head = (`index` 0)

    -- MyList 9 ns; MySeq 13 ns; MyVector 12 ns
    -- head = foldr (\ x _ -> x) (emptyListLikeError "head")

    {- | Extract head and tail, return Nothing if empty -}
    uncons :: full -> Maybe (item, full)
    uncons x = if null x then Nothing else Just (head x, tail x) -- please don't

    {- | Extracts the last element of a 'ListLike'. -}
    last :: full -> item
    -- MyList 124 ms; MySeq 170 us; MyVector 119 us
    -- last l = case genericLength l of
    --               (0::Integer) -> error "Called last on empty list"
    --               1 -> head l
    --               _ -> last (tail l)

    -- MyList 24 us; MySeq 7 ns; MyVector 2 us
    last = foldl (\ _ x -> x) (emptyListLikeError "last")

    {- | Gives all elements after the head. -}
    tail :: full -> full
    -- MyList 11 us; MySeq 23 us; MyVector 7 us
    tail = maybe (error "tail") snd . uncons

    {- | All elements of the list except the last one.  See also 'inits'. -}
    init :: full -> full
    {-- -- MyList 45 us; MySeq 655 us; MyVector 27 ms
    init l
        | null l = error "init: empty list"
        | null xs = empty
        | otherwise = cons (head l) (init xs)
        where xs = tail l
    --}

    -- MyList 110 us; MySeq 540 us; MyVector 313 us
    -- init = fromList . L.init . toList

    {-- -- MyList 49 us; MySeq 428 us; MyVector 26 ms
    init l = case uncons l of
        Nothing -> emptyListLikeError "init"
        Just (x, xs) -> init1 x xs
      where
        init1 x xs = case uncons xs of
            Nothing -> mempty
            Just (x', xs') -> x `cons` init1 x' xs'
    --}

    -- {-- -- MyList 47 us; MySeq 432 us; MyVector 259 us
    init l = fromList (build (\ f z ->
      let
        initGo x xs = case uncons xs of
            Nothing -> z
            Just (x', xs') -> x `f` initGo x' xs'
      in
        case uncons l of
          Nothing -> emptyListLikeError "init"
          Just (x, xs) -> initGo x xs))
    --}

    {- | Tests whether the list is empty. -}
    null :: full -> Bool
    -- necessary for MINIMAL pragma regardless of benchmarks
    null x = genericLength x == (0::Integer)

    {- | Length of the list.  See also 'genericLength'. -}
    length :: full -> Int
    length = genericLength

    ------------------------------ List Transformations

    {- | Apply a function to each element, returning any other
         valid 'ListLike'.  'rigidMap' will always be at least
         as fast, if not faster, than this function and is recommended
         if it will work for your purposes.  See also 'mapM'. -}
    map :: ListLike full' item' => (item -> item') -> full -> full'
    -- MyList rigid 92 us, List 99 us, Vector 115 ms
    -- MySeq rigid 793 us, List 389 us, Vector 95 ms
    -- MyVector rigid 107 ms, List 408 us, Vector 107 ms
    -- map func inp
    --     | null inp = empty
    --     | otherwise = cons (func (head inp)) (map func (tail inp))

    -- MyList rigid 111 us, List 88 us, Vector 123 us
    -- MySeq rigid 676 us, List 289 us, Vector 400 us
    -- MyVector rigid 338 us, List 236 us, Vector 339 us
    map func = fromList . L.map func . toList

    {- | Like 'map', but without the possibility of changing the type of
       the item.  This can have performance benefits for things such as
       ByteStrings, since it will let the ByteString use its native
       low-level map implementation. -}
    rigidMap :: (item -> item) -> full -> full
    rigidMap = map

    {- | Reverse the elements in a list. -}
    reverse :: full -> full
    -- MyList 27 us; MySeq 416 us; MyVector 28 ms
    {--
    reverse l = rev l empty
        where rev rl a
                | null rl = a
                | otherwise = rev (tail rl) (cons (head rl) a)
    --}

    -- MyList 27 us, MySeq 261 us, MyVector 62 us
    reverse = fromList . reverseToList
      where
        reverseToList xs = build (\ f z -> foldl (flip f) z xs)

    {- | Add an item between each element in the structure -}
    intersperse :: item -> full -> full
    -- MyList 83 us; MySeq 950 us; MyVec 99 ms
    {--
    intersperse sep l
        | null l = empty
        | null xs = singleton x
        | otherwise = cons x (cons sep (intersperse sep xs))
        where x = head l
              xs = tail l
    --}

    -- MyList 159 us; MySeq 838 us; MyVec 419 us
    intersperse sep = fromList . L.intersperse sep . toList

    ------------------------------ Reducing Lists (folds)
    -- See also functions in FoldableLLL

    ------------------------------ Special folds
    {- | Flatten the structure. -}
    concat :: (ListLike full' full{-, Monoid full-}) => full' -> full
    concat = fold

    {- | Map a function over the items and concatenate the results.
         See also 'rigidConcatMap'.-}
    concatMap :: (ListLike full' item') =>
                 (item -> full') -> full -> full'
    -- to List: MyList 436 us; MySeq 429 us; MyVector 431 us
    -- to Seq: MyList 553 us; MySeq 569 us; MyVector 594 us
    -- to Vector: MyList 279 us; MySeq 326 us; MyVector 313 us
    concatMap = foldMap

    -- to List: MyList 421 us; MySeq 637 us; MyVector 546 us
    -- to Seq: MyList 549 us; MySeq 809 us; MyVector 819 us
    -- to Vector: MyList 276 us; MySeq 775 us; MyVector 757 us
    -- concatMap f = mconcat . L.map f . toList

    {- | Like 'concatMap', but without the possibility of changing
         the type of the item.  This can have performance benefits
         for some things such as ByteString. -}
    rigidConcatMap :: (item -> full) -> full -> full
    rigidConcatMap = concatMap

    {- | True if any items satisfy the function -}
    any :: (item -> Bool) -> full -> Bool
    any p = getAny . foldMap (Any . p)

    {- | True if all items satisfy the function -}
    all :: (item -> Bool) -> full -> Bool
    all p = getAll . foldMap (All . p)

    {- | The maximum value of the list -}
    maximum :: Ord item => full -> item
    -- MyList 183 us; MySeq 166 us; MyVector 192 us
    -- maximum = foldr1 max

    -- MyList 30 us; MySeq 82 us; MyVec 29 us
    maximum = fromMaybe (emptyListLikeError "maximum") . foldl' maybeMax Nothing
      where
        maybeMax (Just y) x = Just $! max x y
        maybeMax Nothing x = Just x

    {- | The minimum value of the list -}
    minimum :: Ord item => full -> item
    -- minimum = foldr1 min

    minimum = fromMaybe (emptyListLikeError "minimum") . foldl' maybeMin Nothing
      where
        maybeMin (Just y) x = Just $! min x y
        maybeMin Nothing x = Just x

    ------------------------------ Infinite lists
    {- | Generate a structure with the specified length with every element
    set to the item passed in.  See also 'genericReplicate' -}
    replicate :: Int -> item -> full
    replicate = genericReplicate

    ------------------------------ Sublists
    {- | Takes the first n elements of the list.  See also 'genericTake'. -}
    take :: Int -> full -> full
    take = genericTake

    {- | Drops the first n elements of the list.  See also 'genericDrop' -}
    drop :: Int -> full -> full
    drop = genericDrop

    {- | Equivalent to @('take' n xs, 'drop' n xs)@.  See also 'genericSplitAt'. -}
    splitAt :: Int -> full -> (full, full)
    splitAt = genericSplitAt

    {- | Returns all elements at start of list that satisfy the function. -}
    takeWhile :: (item -> Bool) -> full -> full
    -- MyList 30 us; MySeq 278 us; MyVector 7 ms
    -- takeWhile func l
    --     | null l = empty
    --     | func x = cons x (takeWhile func (tail l))
    --     | otherwise = empty
    --     where x = head l

    -- takeWhile func = fst . span func

    -- MyList 30 us; MySeq 221 us; MyVector 124 ms
    -- takeWhile func = fromList . takeWhile func . toList

    -- MyList 30 us; MySeq 139 us; MyVector 49 us;
    takeWhile p l =
        fromList (build (\ f z -> foldr (\ x xs -> if p x then x `f` xs else z) z l))

    {- | Drops all elements from the start of the list that satisfy the
       function. -}
    dropWhile :: (item -> Bool) -> full -> full
    -- List 14 us; Seq 149 us; Vector 149 us
    -- dropWhile func l
    --     | null l = empty
    --     | func (head l) = dropWhile func (tail l)
    --     | otherwise = l

    -- dropWhile func = snd . span func

    -- List 15 us; Seq 100 us; Vector 77 us
    dropWhile func l = case uncons l of
        Nothing -> mempty
        Just (x, xs)
            | func x -> dropWhile func xs
            | otherwise -> l

    {- | Drops all elements from the end of the list that satisfy the
       function. -}
    dropWhileEnd :: (item -> Bool) -> full -> full
    -- MyList 63 us; MySeq 164 us; MyVector 6 ms
    dropWhileEnd func = foldr (\x xs -> if func x && null xs then empty else cons x xs) empty
    -- MyList 103 us; MySeq 409 us; MyVector 269 us
    -- dropWhileEnd func = fromList . L.dropWhileEnd func . toList

    {- | The equivalent of @('takeWhile' f xs, 'dropWhile' f xs)@ -}
    span :: (item -> Bool) -> full -> (full, full)
    -- MyList 87 us; MySeq 433 us; MyVector 25 ms
    -- span func l
    --     | null l = (empty, empty)
    --     | func x = (cons x ys, zs)
    --     | otherwise = (empty, l)
    --    where (ys, zs) = span func (tail l)
    --          x = head l

    -- MyList 90 us; MySeq 363 us; MyVector 25 ms
    -- span func l = case uncons l of
    --     Nothing -> (mempty, mempty)
    --     Just (x, xs)
    --         | func x ->
    --           let (takes, drops) = span func xs
    --           in (cons x takes, drops)
    --         | otherwise -> (mempty, l)

    -- MyList 43 us; MySeq 241 us; MyVector 127 us
    span func l = (takeWhile func l, dropWhile func l)

    -- MyList 148 us; MySeq 719 us; MyVector 380 us
    -- span func l = case L.span func (toList l) of
    --     (takes, drops) -> (fromList takes, fromList drops)

    {- | The equivalent of @'span' ('not' . f)@ -}
    break :: (item -> Bool) -> full -> (full, full)
    break p = span (not . p)

    {- | Split a list into sublists, each which contains equal arguments.
       For order-preserving types, concatenating these sublists will produce
       the original list. See also 'groupBy'. -}
    group :: (ListLike full' full, Eq item) => full -> full'
    group = groupBy (==)

    {- | All initial segments of the list, shortest first -}
    inits :: (ListLike full' full) => full -> full'
    -- MyList List 364 ms, self 370 ms; MySeq List 1.3 s, self 2.4 s; MyVector List 428 ms, self 1.4 s
    -- inits = fromList . L.map fromList . L.inits . toList

    -- MyList List 372 ms, self 479 ms; MySeq List 626 ms, self 1.5 s; MyVector List 324 ms, self 1.2 s
    inits l = fromList $ build (\ f z ->
        ifoldr (\ i xs xss -> fromListN i xs `f` xss) z $ L.inits $ toList l)
      where
        ifoldr f z xs = L.foldr (\ x loop !i -> f i x (loop ( i + 1))) (\_-> z) xs 0

    {- | All final segnemts, longest first -}
    tails :: ListLike full' full => full -> full'
    -- MyList List 54 ms, self 63 ms; MySeq List 119 ms, self 199 ms; MyVec List 32.3 ms, self 149 ms
    -- tails l
    --     | null l = singleton empty
    --     | otherwise = cons l (tails (tail l))

    -- MyList List 56 ms, self 66 ms; MySeq List 127 ms, self 128 ms; MyVec List 35 ms, self 150 ms
    -- tails l = l `cons` case uncons l of
    --     Nothing -> mempty
    --     Just ~(_, xs) -> tails xs

    -- MyList List 56 ms, self 66 ms; MySeq List 120 ms self 121 ms; MyVec List 34 ms, self 37 ms
    tails l = fromList ( build (\ f z ->
      let
        tailsGo xs = xs `f` case uncons xs of
            Nothing -> z
            Just ~(_, xs') -> tailsGo xs'
      in tailsGo l))


    ------------------------------ Predicates
    {- | True when the first list is at the beginning of the second. -}
    isPrefixOf :: Eq item => full -> full -> Bool
    -- MyList 21 us; MySeq 293 us; MyVector 594 us
    -- isPrefixOf needle haystack
    --     | null needle = True
    --     | null haystack = False
    --     | otherwise = (head needle) == (head haystack) &&
    --                   isPrefixOf (tail needle) (tail haystack)

    -- MyList 42 us; MySeq 224 us; MyVector 166 us
    -- isPrefixOf needle haystack = L.isPrefixOf (toList needle) (toList haystack)

    -- MyList 12 us; MySeq 96 us; MyVector 71 us
    -- isPrefixOf needle = isJust . stripPrefix needle

    -- MyList 11 us; MySeq 111 us; MyVector 65 us
    -- isPrefixOf needle haystack = foldr match null haystack needle
    --   where
    --     match h loop needle' = case uncons needle' of
    --         Nothing -> True
    --         Just (n, eedle') -> n == h  &&  loop eedle'

    -- {-- -- MyList 11 us; MySeq 109 us; MyVector 65 us
    isPrefixOf needle = foldr match (\_-> True) needle
      where
        match n loop haystack = case uncons haystack of
            Nothing -> False
            Just (h, aystack) -> n == h  &&  loop aystack
    --}

    -- MyList 27 us; MySeq 132 us; MyVec 76 us
    -- isPrefixOf needle haystack = foldr match (\_-> True) needle (toList haystack)
    --   where
    --     match _ _ [] = False
    --     match n loop (h : aystack) = n == h  &&  loop aystack

    {- | True when the first list is at the beginning of the second. -}
    isSuffixOf :: Eq item => full -> full -> Bool
    -- MyList 41 us; MySeq 739 us; MyVector 263 us
    -- isSuffixOf needle haystack = isPrefixOf (reverse needle) (reverse haystack)

    -- MyList 37 us; MySeq 47 us; MyVector 28 us
    isSuffixOf needle haystack = L.isPrefixOf (toListReverse needle) (toListReverse haystack)
      where
        toListReverse xs = build (\ f z -> foldl (flip f) z xs)

    -- MyList 44 us; MySeq 45 us; MyVector 29 us
    -- isSuffixOf needle haystack = foldl match null haystack (toListReverse needle)
    --   where
    --     match _ _ [] = True
    --     match loop k (e : ldeen) = e == k  &&  loop ldeen

    --     toListReverse xs = build (\ f z -> foldl (flip f) z xs)

    {- | True when the first list is wholly containted within the second -}
    isInfixOf :: Eq item => full -> full -> Bool
    isInfixOf needle haystack =
        any (isPrefixOf needle) thetails
        where thetails = asTypeOf (tails haystack) [haystack]

    ------------------------------ Conditionally modify based on predicates
    {- | Remove a prefix from a listlike if possible -}
    stripPrefix :: Eq item => full -> full -> Maybe full
    -- MyList 30 us; MySeq 206 us; MyVector 147 us
    -- stripPrefix xs ys = if xs `isPrefixOf` ys
    --                         then Just $ drop (length xs) ys
    --                         else Nothing

    -- MyList 18 us; MySeq 110 us; MyVector 66 us
    stripPrefix needle = foldr match Just needle
      where
        match n loop haystack = case uncons haystack of
            Just (h, aystack) | n == h -> loop aystack
            _ -> Nothing


    {- | Remove a suffix from a listlike if possible -}
    stripSuffix :: Eq item => full -> full -> Maybe full
    -- MyList 29 us; MySeq 15 ns; MyVector 15 us
    stripSuffix xs ys = if xs `isSuffixOf` ys
                            then Just $ take (length ys - length xs) ys
                            else Nothing

    ------------------------------ Searching
    {- | True if the item occurs in the list -}
    elem :: Eq item => item -> full -> Bool
    elem i = any (== i)

    {- | True if the item does not occur in the list -}
    notElem :: Eq item => item -> full -> Bool
    notElem i = all (/= i)

    {- | Take a function and return the first matching element, or Nothing
       if there is no such element. -}
    find :: (item -> Bool) -> full -> Maybe item
    {---- MyList 180 us; MySeq 320 us; MyVector: 281 us
    find f l = case findIndex f l of
                    Nothing -> Nothing
                    Just x -> Just (index l x)
    --}

    -- MyList 10 us; MySeq 22 us; MyVector 9 us
    find p = foldr (\ x mx -> if p x then Just x else mx) Nothing

    {- | Returns only the elements that satisfy the function. -}
    filter :: (item -> Bool) -> full -> full
    -- MyList 229 us; MySeq 774 us; MyVector 8 ms
    -- filter func l
    --     | null l = empty
    --     | func (head l) = cons (head l) (filter func (tail l))
    --     | otherwise = filter func (tail l)

    -- MyList 243 us; MySeq 525 us; MyVector 414 us
    -- filter p = fromList . L.filter p . toList

    filter p = fst . partition p

    {- | Returns the lists that do and do not satisfy the function.
       Same as @('filter' p xs, 'filter' ('not' . p) xs)@ -}
    partition :: (item -> Bool) -> full -> (full, full)
    -- MyList 474 us; MySeq 1.1 ms; MyVector 855 us
    -- partition p xs = (filter p xs, filter (not . p) xs)

    -- MyList 585 us; MySeq 1.27 ms; MyVector 669 us
    partition p xs = case partition p (toList xs) of
        (ts, fs) -> (fromList ts, fromList fs)

    -- MyList 368 us; MySeq 776 us; MyVec 100 ms
    -- partition p = foldr
    --         (\ x ~(ts, fs) -> if p x then (cons x ts, fs) else (ts, cons x fs))
    --         (mempty, mempty)

    ------------------------------ Indexing
    {- | The element at 0-based index i.  Raises an exception if i is out
         of bounds.  Like (!!) for lists. -}
    index :: full -> Int -> item
    -- MyList 7 us; MySeq 97 us; 65 us
    -- index l i
    --     | null l = error "index: index not found"
    --     | i < 0 = error "index: index must be >= 0"
    --     | i == 0 = head l
    --     | otherwise = index (tail l) (i - 1)

    -- MyList 6 us; MySeq 20 us; MyVector 1 us  -- Why is the vector test so fast with foldr-based index?
    index l i
        | i < 0
        = error "index: index must be >= 0"
        | otherwise
        = foldr
            (\ x loop !i' -> if i' == 0 then x else loop (i' - 1))
            (\_-> error "index: index not found")
            l
            i

    {- | Returns the index of the element, if it exists. -}
    elemIndex :: Eq item => item -> full -> Maybe Int
    -- MyList 9 us; MySeq 23 us; MyVector 8 us
    -- elemIndex e l = findIndex (== e) l

    -- MyList 9 us; MySeq 24 us; MyVector 8 us
    -- I would use this one because some structures can provide better than O(n) `elemIndices`, but `findIndex` is an O(n) right fold for every structure.
    elemIndex e = listToMaybe . elemIndices e

    {- | Returns the indices of the matching elements.  See also
       'findIndices' -}
    elemIndices :: (Eq item, ListLike result Int) => item -> full -> result
    -- MyList 34 us; MySeq 92 us; MyVector 30 us
    elemIndices i l = findIndices (== i) l

    {- | Take a function and return the index of the first matching element,
         or Nothing if no element matches -}
    findIndex :: (item -> Bool) -> full -> Maybe Int
    -- MyList 7 us; MySeq 18 us; MyVector 8 us
    findIndex f = listToMaybe . findIndices f

    {- | Returns the indices of all elements satisfying the function -}
    findIndices :: (ListLike result Int) => (item -> Bool) -> full -> result
    {--
    -- MyList 402 us; MySeq 720 us; MyVector 622 us
    findIndices p xs = map snd $ filter (p . fst) $ thezips
        where thezips = asTypeOf (zip xs [0..]) [(head xs, 0::Int)]
    --}

    -- MyList 61 us; MySeq 109 us; MyVector 55 us
    findIndices p l = fromList $ build (\ f z -> foldr (findWithIndex p f) (\_-> z) l 0)
      where
        findWithIndex q f x loop !i
            | q x = i `f` loop (i + 1)
            | otherwise = loop (i + 1)

    ------------------------------ Monadic operations
    {- | Evaluate each action in the sequence and collect the results -}
    sequence :: (Applicative m, ListLike fullinp (m item)) =>
                fullinp -> m full
    -- sequence = foldr (A.liftA2 cons) (pure empty)
    sequence = mapM id

    {- | A map in monad space.  Same as @'sequence' . 'map'@

         See also 'rigidMapM' -}
    mapM :: (Applicative m, ListLike full' item') =>
            (item -> m item') -> full -> m full'
    -- to self: MyList 140 us; MySeq 725 us; MyVector 93 ms
    -- to List: MyList 147 us; MySeq 360 us; MyVector 316 us
    -- mapM func l = sequence mapresult
    --         where mapresult = asTypeOf (map func l) []

    -- to self: MyList 133 us; MySeq 689 us; 345 us
    -- to List: MyList 115 us; MySeq 303 us; 249 us
    mapM func = fmap fromList . traverse func . toList

    {- | Like 'mapM', but without the possibility of changing the type
         of the item.  This can have performance benefits with some types. -}
    rigidMapM :: Monad m => (item -> m item) -> full -> m full
    rigidMapM = mapM


    ------------------------------ "Set" operations
    {- | Removes duplicate elements from the list.  See also 'nubBy' -}
    nub :: Eq item => full -> full
    nub = nubBy (==)

    {- | Removes the first instance of the element from the list.
       See also 'deleteBy' -}
    delete :: Eq item => item -> full -> full
    delete = deleteBy (==)

    {- | List difference.  Removes from the first list the first instance
       of each element of the second list.  See '(\\)' and 'deleteFirstsBy' -}
    deleteFirsts :: Eq item => full -> full -> full
    deleteFirsts = foldl (flip delete)

    {- | List union: the set of elements that occur in either list.
         Duplicate elements in the first list will remain duplicate.
         See also 'unionBy'. -}
    union :: Eq item => full -> full -> full
    union = unionBy (==)

    {- | List intersection: the set of elements that occur in both lists.
         See also 'intersectBy' -}
    intersect :: Eq item => full -> full -> full
    intersect = intersectBy (==)

    ------------------------------ Ordered lists
    {- | Sorts the list.  On data types that do not preserve ordering,
         or enforce their own ordering, the result may not be what
         you expect.  See also 'sortBy'. -}
    sort :: Ord item => full -> full
    sort = sortBy compare

    {- | Inserts the element at the last place where it is still less than or
         equal to the next element.  On data types that do not preserve
         ordering, or enforce their own ordering, the result may not
         be what you expect.  On types such as maps, this may result in
         changing an existing item.  See also 'insertBy'. -}
    insert :: Ord item => item -> full -> full
    insert = insertBy compare

    ------------------------------ Conversions

    {- | Converts the structure to a list.  This is logically equivolent
         to 'fromListLike', but may have a more optimized implementation.
         These two functions are now retired in favor of the methods of
         IsList, but they are retained here because some instances still
         use this implementation. -}
    toList' :: full -> [item]
    -- toList' = fromListLike

    -- build gives significant improvements when streaming as List.
    toList' l = build (\ f z ->
      -- Don't use foldr in case foldr f z = L.foldr f z . toList'
      let
        goToList xs = case uncons xs of
            Nothing -> z
            Just (x, xs') -> x `f` goToList xs'
      in goToList l)
    {-# INLINE toList' #-}

    {- | Generates the structure from a list. -}
    fromList' :: [item] -> full
    fromList' = L.foldr cons mempty
    {-# INLINE fromList' #-}

    {- | Converts one ListLike to another.  See also 'toList''.
         Default implementation is @fromListLike = map id@ -}
    fromListLike :: ListLike full' item => full -> full'
    -- fromListLike = map id
    fromListLike = fromList . toList
    {-# INLINE fromListLike #-}

    ------------------------------ Generalized functions
    {- | Generic version of 'nub' -}
    -- This code is adapted from Data.List in base.
    nubBy :: (item -> item -> Bool) -> full -> full
    {-- MyList 54 ms; MySeq 447 ms; MyVector 393 ms
    nubBy eq l = nubBy' l mempty
      where
        nubBy' ys xs =
          case uncons ys of
            Nothing -> mempty
            Just (y, ys')
              | elem_by y xs -> nubBy' ys' xs
              | otherwise -> cons y (nubBy' ys' (cons y xs))
        elem_by :: item -> full -> Bool
        elem_by y xs =
          case uncons xs of
            Nothing -> False
            Just (x, xs') -> x `eq` y || elem_by y xs'
    --}

    {-- MyList 48 ms; MySeq 132 ms; MyVector 65 ms
    nubBy eq l = nubBy' l mempty
      where
        nubBy' ys xs =
          case uncons ys of
            Nothing -> mempty
            Just (y, ys')
              | elem_by y xs -> nubBy' ys' xs
              | otherwise -> cons y (nubBy' ys' (cons y xs))
        elem_by :: item -> full -> Bool
        elem_by y xs = any (`eq` y) xs
    --}

    -- MyList 55 ms; MySeq 86 ms; MyVector 72 ms
    -- nubBy eq = fromList . L.nubBy eq . toList

    -- {-- MyList 47 ms; MySeq 57 ms; MyVector 48 ms
    nubBy eq l = fromList (build (\ f z -> foldr (check f) (\_-> z) l []))
      where
        check :: forall r. (item -> r -> r) -> item -> ([item] -> r) -> [item] -> r
        check f x loop seen
            | any (`eq` x) seen
            = loop seen
            | otherwise
            = x `f` loop (x : seen)
    --}

{-
    nubBy f l
        | null l = empty
        | otherwise =
            cons (head l) (nubBy f (filter (\y -> not (f (head l) y)) (tail l)))
-}

    {- | Generic version of 'deleteBy' -}
    deleteBy :: (item -> item -> Bool) -> item -> full -> full
    -- {-- -- MyList 39 us; MySeq 363 us; MyVector 20 ms
    deleteBy func i l
        | null l = empty
        | otherwise =
            if func i (head l)
               then tail l
               else cons (head l) (deleteBy func i (tail l))
    --}
    -- Deleting individual elements is probably hopeless for arrays.
    -- MyList 95 us; MySeq 529 us; MyVector 289 us
    -- deleteBy func x = fromList . L.deleteBy func x . toList

    {- | Generic version of 'deleteFirsts' -}
    deleteFirstsBy :: (item -> item -> Bool) -> full -> full -> full
    deleteFirstsBy func = foldl (flip (deleteBy func))

    {- | Generic version of 'union' -}
    unionBy :: (item -> item -> Bool) -> full -> full -> full
    {--
    -- MyList 288 ms; MySeq 671 ms; MyVector 2 s
    unionBy func x y =
        append x $ foldl (flip (deleteBy func)) (nubBy func y) x
    --}

    {--
    -- MyList 285 ms; MySeq 667 ms; MyVector 2 s
    unionBy func x y =
        append x $ deleteFirstsBy func (nubBy func y) x
    --}

    -- MyList 54 ms; MySeq 91 ms; MyVector 74 ms
    unionBy func x y = fromList (L.unionBy func (toList x) (toList y))

    {- | Generic version of 'intersect' -}
    intersectBy :: (item -> item -> Bool) -> full -> full -> full
    intersectBy func xs ys = filter (\x -> any (func x) ys) xs

    {- | Generic version of 'group'. -}
    groupBy :: (ListLike full' full, Eq item) =>
                (item -> item -> Bool) -> full -> full'
    {--
    -- List: MyList (==) 241 us, (<) 125 us; MySeq (==) 699 us, (<) 521 us; MyVector (==) 800 us, (<) 304 us
    -- self: MyList (==) 243 us, (<) 125 us; MySeq (==) 1.3 ms, (<) 534 us; MyVector (==) 96 ms, (<) 301 us
    groupBy eq l
        | null l = empty
        | otherwise = cons (cons x ys) (groupBy eq zs)
                      where (ys, zs) = span (eq x) xs
                            x = head l
                            xs = tail l
    --}
    -- groupBy is tough, because consing x to the span may be O(n), but you can't include x in the span because eq x x may be False. The obvious solution is to use the consing version for list-type structures and check and then slice for structures with O(1) slice. But that doesn't present a good default. The check and then slice version is not lazy enough for consing structures.

    -- {--
    -- List (==): MyList 391 us; MySeq 625 us; MyVec 587 us
    -- List (<): MyList 285 us; MySeq 1 ms; MyVec 585 us
    -- self (==): MyList 433 us; MySeq 1.3 ms; MyVec 822 us
    -- self (<): MyList 334 us; MySeq 835 us; MyVec 530 us
    groupBy eq = fromList . fmap fromList . L.groupBy eq . toList
    --}

    {--
    -- List (==): MyList 245 us us; MySeq 354 us; MyVec 285 us
    -- List (<): MyList 225 us; MySeq 407 us; MyVec 503 us
    -- self (==): MyList 262 us; MySeq 627 us; MyVec 337 us
    -- self (<): MyList 261 us; MySeq 909 us; MyVec 663 us
    groupBy eq l = fromList (build (\ f z ->
      let
        groupGo xs = case uncons xs of
            Nothing -> z
            Just (x, xs') ->
                case genericSplitAt (count (eq x) xs) xs' of
                    (eqs, neqs) -> eqs `f` groupGo neqs
      in groupGo l))
      where
        count :: (item -> Bool) -> full -> Integer
        count p xs = foldr (\ x loop !n -> if p x then loop (n + 1) else n) id xs 0
    --}

    {- | Sort function taking a custom comparison function -}
    sortBy :: (item -> item -> Ordering) -> full -> full
    {- Going through List rather than using insertion sort changes sort time for
       a 10,000 Int Vector from 1 minute to 200 microseconds. -}
    sortBy cmp = fromList . L.sortBy cmp . toList

    {- | Like 'insert', but with a custom comparison function -}
    insertBy :: (item -> item -> Ordering) -> item ->
                full -> full
    -- insertBy is hopeless for structures that don't have efficient cons.
    insertBy cmp x ys =
        case uncons ys of
            Nothing -> singleton x
            Just (ys_head,ys_tail) -> case cmp x ys_head of
                        GT -> cons ys_head (insertBy cmp x ys_tail)
                        _ ->  cons x ys

    ------------------------------ Generic Operations
    {- | Length of the list -}
    genericLength :: Num a => full -> a
    {-- -- MyList 10 us; MySeq 174 us; MyVector 118 us
    genericLength l = calclen 0 l
        where calclen !accum cl =
                  if null cl
                     then accum
                     else calclen (accum + 1) (tail cl)
    --}

    -- MyList 10 us; MySeq 69 us; MyVector 2.5 us
    genericLength = foldl' (\ len _ -> len + 1) 0

    {- | Generic version of 'take' -}
    genericTake :: Integral a => a -> full -> full
    {-- -- MyList 26 us; MySeq 295 us; MyVector 7 ms
    genericTake !n l
        | n <= 0 = empty
        | null l = empty
        | otherwise = cons (head l) (genericTake (n - 1) (tail l))
    --}

    -- {-- -- MyList 55 us; MySeq 264 us; MyVector 154 us
    -- But really, for an array type you should have to provide a more efficient take.
    genericTake n = fromList . L.genericTake n . toList
    --}

    {- | Generic version of 'drop' -}
    genericDrop :: Integral a => a -> full -> full
    -- MyList 15 us; MySeq 100 us; MyVector 70 us
    genericDrop n l
        | n <= 0 = l
        | null l = l
        | otherwise = genericDrop (n - 1) (tail l)

    {- | Generic version of 'splitAt' -}
    genericSplitAt :: Integral a => a -> full -> (full, full)
    genericSplitAt n l = (genericTake n l, genericDrop n l)

    {- | Generic version of 'replicate' -}
    genericReplicate :: Integral a => a -> item -> full
    -- MyList 2 us; 7 us; 4 us
    {--
    genericReplicate count x
        | count <= 0 = empty
        | otherwise = map (\_ -> x) [1..count]
    --}

    -- MyList 5 us; 1 us; 2 us
    genericReplicate count x
        | count <= 0 = mempty
        | otherwise = stimes count (singleton x)

    {-# MINIMAL (singleton, uncons, null) |
                (singleton, uncons, genericLength) |
                (singleton, head, tail, null) |
                (singleton, head, tail, genericLength) #-}


-- | A version of 'ListLike' with a single type parameter, the item
-- type is obtained using the 'Item' type function from 'IsList'.
type ListOps full = (ListLike full (Item full))

{-
instance (ListLike full item) => Monad full where
    m >>= k = foldr (append . k) empty m
    m >> k = foldr (append . (\_ -> k)) empty m
    return x = singleton x
    fail _ = empty

instance (ListLike full item) => M.MonadPlus full where
    mzero = empty
    mplus = append
-}

{- | An extension to 'ListLike' for those data types that are capable
of dealing with infinite lists.  Some 'ListLike' functions are capable
of working with finite or infinite lists.  The functions here require
infinite list capability in order to work at all. -}
class (ListLike full item) => InfiniteListLike full item | full -> item where
    {- | An infinite list of repeated calls of the function to args -}
    iterate :: (item -> item) -> item -> full
    iterate f x = cons x (iterate f (f x))

    {- | An infinite list where each element is the same -}
    repeat :: item -> full
    repeat x = xs
        where xs = cons x xs

    {- | Converts a finite list into a circular one -}
    cycle :: full -> full
    cycle xs
        | null xs = error "ListLike.cycle: empty list"
        | otherwise = xs' where xs' = append xs xs'

--------------------------------------------------
-- This instance is here due to some default class functions

instance ListLike [a] a where
    empty = []
    singleton x = [x]
    cons x l = x : l
    snoc l x = l ++ [x]
    append = (++)
    -- Andreas Abel, 2023-10-10, issue #32:
    -- Use implementation of 'head' and 'tail' in terms of 'uncons' to avoid the x-partial warning under GHC 9.8
    uncons []       = Nothing
    uncons (x : xs) = Just (x, xs)
    last = L.last
    init = L.init
    null = L.null
    length = L.length
    map f = fromList . L.map f
    rigidMap = L.map
    reverse = L.reverse
    intersperse = L.intersperse
    -- fromListLike = toList
    concat = L.concat . toList
    -- concatMap func = fromList . L.concatMap func
    rigidConcatMap = L.concatMap
    any = L.any
    all = L.all
    maximum = L.maximum
    minimum = L.minimum
    -- fold
    -- foldMap
    replicate = L.replicate
    take = L.take
    drop = L.drop
    splitAt = L.splitAt
    takeWhile = L.takeWhile
    dropWhile = L.dropWhile
    span = L.span
    break = L.break
    group = fromList . L.group
    inits = fromList . L.inits
    tails = fromList . L.tails
    isPrefixOf = L.isPrefixOf
    isSuffixOf = L.isSuffixOf
    isInfixOf = L.isInfixOf
    stripPrefix = L.stripPrefix
    elem = L.elem
    notElem = L.notElem
    find = L.find
    filter = L.filter
    partition = L.partition
    index = (L.!!)
    elemIndex = L.elemIndex
    elemIndices item = fromList . L.elemIndices item
    findIndex = L.findIndex
    sequence = sequenceA . toList
    -- mapM = M.mapM
    nub = L.nub
    delete = L.delete
    deleteFirsts = (L.\\)
    union = L.union
    intersect = L.intersect
    sort = L.sort
    groupBy func = fromList . L.groupBy func
    unionBy = L.unionBy
    intersectBy = L.intersectBy
    sortBy = L.sortBy
    insert = L.insert
    genericLength = L.genericLength


--------------------------------------------------
-- These utils are here instead of in Utils.hs because they are needed
-- by default class functions

emptyListLikeError :: [Char] -> a
emptyListLikeError function_name = error (function_name L.++ ": empty structure")
{-# NOINLINE emptyListLikeError #-}

{- | Takes two lists and returns a list of corresponding pairs. -}
zip :: (ListLike full item,
          ListLike fullb itemb,
          ListLike result (item, itemb)) =>
          full -> fullb -> result
zip = zipWith (,)

{- | Takes two lists and combines them with a custom combining function -}
zipWith :: (ListLike full item,
            ListLike fullb itemb,
            ListLike result resultitem) =>
            (item -> itemb -> resultitem) -> full -> fullb -> result
zipWith f a b
    | null a = empty
    | null b = empty
    | otherwise = cons (f (head a) (head b)) (zipWith f (tail a) (tail b))
