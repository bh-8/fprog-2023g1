{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}

module Deque where
    import Data.Maybe ()
    import Test.Tasty.QuickCheck (Arbitrary (arbitrary), elements)

    data Deque = Deque [Int] deriving (Eq, Show)
    instance Arbitrary Deque where
        arbitrary = elements [Deque [], Deque [0], Deque [1, 2, 3, 4, 5]]

    pushFront :: Int -> Deque -> Deque
    pushFront i (Deque l) = Deque (i : l)

    pushEnd :: Int -> Deque -> Deque
    pushEnd i (Deque l) = Deque (l ++ [i])

    peekFront :: Deque -> Maybe Int
    peekFront (Deque l) = if null l then Nothing else Just (head l)

    peekEnd :: Deque -> Maybe Int
    peekEnd (Deque l) = if null l then Nothing else Just (last l)

    popFront :: Deque -> Deque
    popFront (Deque l) = if null l then Deque [] else Deque (tail l)

    popEnd :: Deque -> Deque
    popEnd (Deque l) = if null l then Deque [] else Deque (init l)

    isEmpty :: Deque -> Bool
    isEmpty (Deque l) = null l

    makeDequeFromList :: [Int] -> Deque
    makeDequeFromList = Deque
