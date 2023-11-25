module Deque where
    import Data.Maybe ()
    data Deque a = End | Deque a (Deque a) deriving Show

    -- O(1)
    pushFront :: a -> Deque a -> Deque a
    pushFront i End = Deque i End
    pushFront i d = Deque i d

    -- O(n)
    pushEnd :: a -> Deque a -> Deque a
    pushEnd i End = Deque i End
    pushEnd i (Deque c End) = Deque c (Deque i End)
    pushEnd i (Deque c d) = Deque c (pushEnd i d)

    -- O(1)
    peekFront :: Deque a -> Maybe a
    peekFront End = Nothing
    peekFront (Deque i _) = Just i

    -- O(n)
    peekEnd :: Deque a -> Maybe a
    peekEnd End = Nothing
    peekEnd (Deque c End) = Just c
    peekEnd (Deque _ d) = peekEnd d

    -- O(1)
    popFront :: Deque a -> Deque a
    popFront End = End
    popFront (Deque _ d) = d

    -- O(n)
    popEnd :: Deque a -> Deque a
    popEnd End = End
    popEnd (Deque _ End) = End
    popEnd (Deque c d) = Deque c (popEnd d)

    makeDequeFromList :: [a] -> Deque a
    makeDequeFromList l = foldl (flip pushFront) End (reverse l)
