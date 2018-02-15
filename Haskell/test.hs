module Stack (Stack, empty, isEmpty, push, top, pop) where
 
empty :: Stack a
isEmpty :: Stack a -> Bool
push :: a -> Stack a -> Stack a
top :: Stack a -> a
pop :: Stack a -> (a,Stack a)
 
newtype Stack a = StackImpl [a] deriving(Show, Eq, Ord) -- opaque!
empty = StackImpl []
isEmpty (StackImpl s) = null s
push x (StackImpl s) = StackImpl (x:s)
top (StackImpl s) = head s
pop (StackImpl (s:ss)) = (s,StackImpl ss)

test :: Integer-> Integer-> Stack Integer-> Stack Integer 
test 0 _ st= st
test num item st= ((test) (num-1) item (push item st))  