import MyStack
import MyQueue

testStackPush = push 1 (push 2 (push 3 createStack))
testStackPeek = peekStack testStackPush
testStackPop = pop testStackPush

testQueueEnqueue = enqueue 1 (enqueue 2 (enqueue 3 createQueue))
testQueuePeek = peekQueue testQueueEnqueue
testQueueDequeue = dequeue testQueueEnqueue
