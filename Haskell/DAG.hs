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
                    wV::b} deriving (Eq, Show,Ord)


data Edge a b=  E {frV::(Vertice a b),
                   toV::(Vertice a b),
                    wE::b} deriving (Eq, Show,Ord)

newtype DAG a b= DAGImpl ([a],[b]) deriving (Eq, Show)


isElem :: (Eq a)=> (Vertice a b) -> DAG (Vertice a b) (Edge a b) -> Bool
isElem v1 (DAGImpl ([],e)) = False
isElem (V n1 wt1) (DAGImpl (((V n2 wt2):xs),e)) = if n1 == n2 then True else isElem (V n1 wt1) (DAGImpl ((xs),e)) 


noCycle :: (Eq a) => (Vertice a b) -> [(Vertice a b)] -> [(Edge a b)] -> [Bool]
noCycle _ [] _ = [True]
noCycle (V n1 wt1) ((V n2 wt2):xs) e 
    | n1 == n2 = [False]
    | otherwise = ((noCycle) (V n1 wt1) (getElem (V n2 wt2) e) e) ++ ((noCycle) (V n1 wt1) (xs) e)

getElem :: (Eq a) => (Vertice a b) -> [(Edge a b)] -> [Vertice a b]
getElem _ [] = []
getElem (V n1 wt1) ((E (V n2 wt2) (V n3 wt3) wt):e) = if n1 == n2 then (V n3 wt3) 
    : (getElem (V n1 wt1) (e)) else (getElem (V n1 wt1) (e))

getEdgeList :: DAG (Vertice a b) (Edge a b) -> [(Edge a b)]
getEdgeList (DAGImpl (_,e)) = e

empty :: DAG (Vertice a b) (Edge a b)
empty = DAGImpl ([],[])

add_vertex :: (Eq a)=> (Vertice a b) -> DAG (Vertice a b) (Edge a b)-> DAG (Vertice a b) (Edge a b)
add_vertex v1 (DAGImpl (v,e)) = if ((isElem) v1 (DAGImpl (v,e))) then (DAGImpl (v,e)) else DAGImpl ((v1:v),e)   

add_edge :: (Eq a)=> (Edge a b) -> DAG (Vertice a b) (Edge a b)-> DAG (Vertice a b) (Edge a b)
add_edge  (E v1 v2 wt) (DAGImpl (v,e)) = if ((isElem) v1 (DAGImpl (v,e))) && ((isElem) v2 (DAGImpl (v,e))) && containsTrue(((noCycle) v1 (getElem v2 e) e))
                                        then DAGImpl (v,((E v1 v2 wt):e))  
                                        else DAGImpl (v,e)      





topological_ordering :: DAG (Vertice a b) (Edge a b) -> [(Vertice a b)]
topological_ordering  (DAGImpl (v,e)) = v

containsTrue :: [Bool] -> Bool
containsTrue list = if True `elem` list then True else False

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

let v1 = (V 'a' 1)
let v2 = (V 'b' 2)
let v3 = (V 'c' 3)

let q = add_vertex v1 empty 
let w = add_vertex v2 q 
let e = add_vertex v3 w 

let r = add_edge (E v1 v2 4) e
let t = add_edge (E v1 v3 5) r



https://wiki.haskell.org/Abstract_data_type
-}
