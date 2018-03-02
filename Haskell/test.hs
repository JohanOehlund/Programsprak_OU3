--module Stack (Stack, emptySTACK, isEmpty, push, top, pop) where
 {-# LANGUAGE DeriveAnyClass #-}


emptySTACK :: Stack a
isEmpty :: Stack a -> Bool
push :: a -> Stack a -> Stack a
top :: Stack a -> a
pop :: Stack a -> (a,Stack a)
 
data Vertice a b= V{name::a,
                    wV::b} deriving (Eq, Ord,Show)


data Edge a b=  E {frV::(Vertice a b),
                   toV::(Vertice a b),
                    wE::b} deriving (Eq, Ord,Show)

data DAG a = EmptyDAG | DAGImpl [a] deriving (Eq, Show)

emptyDAG :: DAG a
emptyDAG = EmptyDAG

add_vertex :: (Eq a)=> DAG a-> a ->
                DAG a
add_vertex  EmptyDAG v = (DAGImpl [v])
add_vertex  (DAGImpl a) v = (DAGImpl (v:a))

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
