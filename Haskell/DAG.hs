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
                    wV::b} deriving (Eq, Ord)


data Edge a b=  E {frV::(Vertice a b),
                   toV::(Vertice a b),
                    wE::b} deriving (Eq, Ord)

newtype DAG a b= DAGImpl ([a],[b]) deriving (Eq, Show)

instance (Show a, Show b) => Show (Vertice a b) where
    show  (V a b)= "(V "++ show a ++ " " ++ show b ++ ")"

instance (Show a, Show b) => Show (Edge a b) where
    show  (E v1 v2 wt)= "(E "++ show v1 ++ " " ++ show v2 ++ show wt++")"

containsVert :: (Eq a)=> (Vertice a b) -> DAG (Vertice a b) (Edge a b) -> Bool
containsVert v1 (DAGImpl ([],e)) = False
containsVert (V n1 wt1) (DAGImpl (((V n2 wt2):xs),e)) = if n1 == n2 then True else containsVert (V n1 wt1) (DAGImpl ((xs),e)) 


noCycle :: (Eq a) => (Vertice a b) -> [(Vertice a b)] -> [(Edge a b)] -> [Bool]
noCycle _ [] _ = [True]
noCycle (V n1 wt1) ((V n2 wt2):xs) e 
    | n1 == n2 = [False]
    | otherwise = ((noCycle) (V n1 wt1) (getElem (V n2 wt2) e) e) ++ ((noCycle) (V n1 wt1) (xs) e)

getElem :: (Eq a) => (Vertice a b) -> [(Edge a b)] -> [Vertice a b]
getElem _ [] = []
getElem (V n1 wt1) ((E (V n2 wt2) (V n3 wt3) wt):e) = if n1 == n2 then (V n3 wt3) 
    : (getElem (V n1 wt1) (e)) else (getElem (V n1 wt1) (e))


topological_ordering :: (Eq a)=> DAG (Vertice a b) (Edge a b) -> [(Vertice a b)] -> [(Vertice a b)]
topological_ordering (DAGImpl ([],e)) _ = []
topological_ordering  (DAGImpl ((x:xs),e)) (x2:xs2)
        | ((isToEdge) x2 e) == True = (topological_ordering) (DAGImpl ((x:xs),e)) xs2
        | otherwise = x2:(topological_ordering) (DAGImpl (fVertList,fEdgeList)) fVertList
                    where fVertList = [res |res  <-(x:xs), ((compareVert) res  x2)]
                          fEdgeList = [res2|res2 <- e    , ((compareEdge) res2 x2)]

compareVert :: (Eq a)=> (Vertice a b) -> (Vertice a b) -> Bool
compareVert (V n1 _) (V n2 _)
    | n1 /= n2 = True
    | otherwise = False


compareEdge :: (Eq a)=> (Edge a b) ->(Vertice a b) -> Bool
compareEdge (E (V n2 _) _ _) (V n1 _)
    | n1 /= n2 = True
    | otherwise = False


empty :: DAG (Vertice a b) (Edge a b)
empty = DAGImpl ([],[])

add_vertex :: (Eq a)=> (Vertice a b) -> DAG (Vertice a b) (Edge a b)-> DAG (Vertice a b) (Edge a b)
add_vertex v1 (DAGImpl (v,e)) = if ((containsVert) v1 (DAGImpl (v,e))) then (DAGImpl (v,e)) else DAGImpl ((v1:v),e)   

add_edge :: (Eq a)=> (Edge a b) -> DAG (Vertice a b) (Edge a b)-> DAG (Vertice a b) (Edge a b)
add_edge  (E v1 v2 wt) (DAGImpl (v,e)) = if ((containsVert) v1 (DAGImpl (v,e))) && ((containsVert) v2 (DAGImpl (v,e))) && containsTrue(((noCycle) v1 (getElem v2 e) e))
                                        then DAGImpl (v,((E v1 v2 wt):e))  
                                        else DAGImpl (v,e)      







containsTrue :: [Bool] -> Bool
containsTrue list = if True `elem` list then True else False


getEdgeList :: DAG (Vertice a b) (Edge a b) -> [(Edge a b)]
getEdgeList (DAGImpl (_,e)) = e

getVertList :: DAG (Vertice a b) (Edge a b) -> [(Vertice a b)]
getVertList (DAGImpl (v,_)) = v


isFromEdge :: (Eq a)=> (Vertice a b) -> [(Edge a b)] -> Bool
isFromEdge _ [] = False
isFromEdge (V n1 wt1) ((E (V n2 wt2) (V n3 wt3) wt):xs)
    | n1 == n2 = True
    | otherwise = isFromEdge (V n1 wt1) xs


isToEdge :: (Eq a)=> (Vertice a b) -> [(Edge a b)] -> Bool
isToEdge _ [] = False
isToEdge (V n1 wt1) ((E (V n2 wt2) (V n3 wt3) wt):xs)
    | n1 == n3 = True
    | otherwise = isToEdge (V n1 wt1) xs

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
################################# TEST CODE ####################################
let v1 = (V 'a' 1)
let v2 = (V 'b' 2)
let v3 = (V 'c' 3)
let v4 = (V 'd' 4)
let v5 = (V 'e' 5)
let v6 = (V 'f' 6)

let dag1 = add_vertex v1 empty 
let dag2 = add_vertex v2 dag1 
let dag3 = add_vertex v3 dag2 
let dag4 = add_vertex v4 dag3
let dag5 = add_vertex v5 dag4
let dag6 = add_vertex v6 dag5

let dag7 =  add_edge (E v1 v2 10) dag6
let dag8 =  add_edge (E v2 v4 11) dag7
let dag9 =  add_edge (E v4 v5 12) dag8
let dag10 = add_edge (E v3 v6 13) dag9
let dag11 = add_edge (E v1 v4 15) dag10
let dag12 = add_edge (E v5 v3 15) dag11

topological_ordering dag12 (getVertList dag12)
################################################################################

https://wiki.haskell.org/Abstract_data_type
-}
