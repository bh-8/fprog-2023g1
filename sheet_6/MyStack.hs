module MyStack where
    import Deque (Deque (Deque, End), pushFront, popFront, peekFront)
    
    createStack :: Deque a
    createStack = End

    push :: a -> Deque a -> Deque a
    push = pushFront

    pop :: Deque a -> Deque a
    pop = popFront

    peekStack :: Deque a -> Maybe a
    peekStack = peekFront
