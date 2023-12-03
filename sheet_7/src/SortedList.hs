{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}

module SortedList where
    import Data.List (sort)
    import Test.Tasty.QuickCheck (Arbitrary (arbitrary), elements)

    data SortedList = SortedList [Int] deriving (Eq, Show)
    instance Arbitrary SortedList where
        arbitrary = elements [SortedList [], SortedList [0], SortedList [1, 2, 3, 4, 5]]

    empty :: SortedList
    empty = SortedList []

    length :: SortedList -> Int
    length (SortedList l) = Prelude.length l

    null :: SortedList -> Bool
    null (SortedList l) = Prelude.null l

    tail :: SortedList -> SortedList
    tail (SortedList l) = SortedList (Prelude.tail l)

    head :: SortedList -> Int
    head (SortedList l) = Prelude.head l

    last :: SortedList -> Int
    last (SortedList l) = Prelude.last l

    insert :: Int -> SortedList -> SortedList
    insert v (SortedList l) = SortedList (sort (v : l))

    (++) :: SortedList -> SortedList -> SortedList
    (++) (SortedList l1) (SortedList l2) = SortedList (sort (l1 Prelude.++ l2))

    isSorted :: SortedList -> Bool
    isSorted (SortedList []) = True
    isSorted (SortedList (x:l)) = fst $ foldl step (True, x) l
        where step (b, x) y = (b && (x <= y), y)
