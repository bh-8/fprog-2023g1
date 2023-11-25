module MyQueue where
    import Deque (Deque (Deque, End), pushEnd, popFront, peekFront)
    
    createQueue :: Deque a
    createQueue = End

    enqueue :: a -> Deque a -> Deque a
    enqueue = pushEnd

    dequeue :: Deque a -> Deque a
    dequeue = popFront

    peekQueue :: Deque a -> Maybe a
    peekQueue = peekFront
