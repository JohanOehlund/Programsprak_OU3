--module Stack (Stack, emptySTACK, isEmpty, push, top, pop) where
import DAG


emptySTACK :: Stack a
isEmpty :: Stack a -> Bool
push :: a -> Stack a -> Stack a
top :: Stack a -> a
pop :: Stack a -> (a,Stack a)
 

newtype Stack a = StackImpl [a] deriving(Show, Eq, Ord) -- opaque!
emptySTACK = StackImpl []
isEmpty (StackImpl s) = null s
push x (StackImpl s) = StackImpl (x:s)
top (StackImpl s) = head s
pop (StackImpl (s:ss)) = (s,StackImpl ss)

test :: (Num a)=>Integer-> a-> Stack a-> Stack a 
test num item st
    | num > 0 = ((test) (num-1) item (push item st))
    | num < 0 = ((test) (num+1) item (push item st))
    | num== 0 = st
