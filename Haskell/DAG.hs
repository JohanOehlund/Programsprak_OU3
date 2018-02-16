--------------------------------------------------------------------------------
-- | Own implementation of a directed acyclic graph (DAG).
-- 
-- Copyright  : (c) Johan Öhlund,
--            : (c) Arvid Ernstsson,2018
-- Maintainer : c15jod@cs.umu.se,
--            : c15aen@cs.umu.se
--
-- Project for the course Programspråk VT18, Umeå universitet.
--------------------------------------------------------------------------------



data Vertice a b= V{name::a,
                    wV::b} deriving (Eq, Show)


data Edge a b=  E {frV::(Vertice a b),
                   toV::(Vertice a b),
                    wE::b} deriving (Eq, Show)





empty :: DAG a b
add_vertex :: a -> DAG a b -> DAG a b
add_edge :: b -> DAG a b -> DAG a b

newtype DAG a b= DAGImpl ([a],[b]) deriving (Eq, Show)

empty = DAGImpl ([],[])
add_vertex v1 (DAGImpl (v,e)) = DAGImpl ((v1:v),e)   
add_edge   e1 (DAGImpl (v,e)) = DAGImpl (v,(e1:e))   


--topological_ordering

--weight_of_longest_path

{-
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
-}





{-
https://wiki.haskell.org/Abstract_data_type
-}
