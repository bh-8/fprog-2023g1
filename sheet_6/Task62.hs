import Deque

testPush :: Deque Integer
testPush = pushFront 1 (pushEnd 4 (Deque 2 (Deque 3 End)))
testPeekFront :: Maybe Integer
testPeekFront = peekFront (Deque 1 (Deque 2 (Deque 3 (Deque 4 End))))
testPeekEnd :: Maybe Integer
testPeekEnd = peekEnd (Deque 1 (Deque 2 (Deque 3 (Deque 4 End))))
testPop :: Deque Integer
testPop = popFront (popEnd (Deque 1 (Deque 2 (Deque 3 (Deque 4 End)))))
