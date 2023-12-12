{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE InstanceSigs #-}
module SortedList where
    import Data.List (sort)
    import Test.Tasty.QuickCheck (Arbitrary (arbitrary, shrink), elements, Gen, sample, vector, vectorOf, choose)

    data SortedList a = SortedList [a] deriving (Eq, Show)
    instance (Arbitrary a, Ord a) => Arbitrary (SortedList a) where
        arbitrary :: Arbitrary a => Gen (SortedList a)
        arbitrary = do
            size <- choose (0, 10)
            list <- vectorOf size arbitrary
            return (SortedList (sort list))
        --arbitrary = elements [SortedList [], SortedList [const arbitrary]]

    empty :: SortedList a
    empty = SortedList []

    length :: SortedList a -> Int
    length (SortedList l) = Prelude.length l

    null :: SortedList a -> Bool
    null (SortedList l) = Prelude.null l

    tail :: SortedList a -> SortedList a
    tail (SortedList l) = SortedList (Prelude.tail l)

    head :: SortedList a -> a
    head (SortedList l) = Prelude.head l

    last :: SortedList a -> a
    last (SortedList l) = Prelude.last l

    insert :: (Ord a) => a -> SortedList a -> SortedList a
    insert v (SortedList l) = SortedList (sort (v : l))

    (++) :: (Ord a) => SortedList a -> SortedList a -> SortedList a
    (++) (SortedList l1) (SortedList l2) = SortedList (sort (l1 Prelude.++ l2))

    isSorted :: (Ord a) => SortedList a -> Bool
    isSorted (SortedList []) = True
    isSorted (SortedList (x:l)) = fst $ foldl step (True, x) l
        where step (b, x) y = (b && (x <= y), y)

    makeSortedListFromList' :: (Ord a) => [a] -> SortedList a -> SortedList a
    makeSortedListFromList' [] acc = acc
    makeSortedListFromList' (i:l) acc = makeSortedListFromList' l (insert i acc)

    makeSortedListFromList :: (Ord a) => [a] -> SortedList a
    makeSortedListFromList l = makeSortedListFromList' l empty
