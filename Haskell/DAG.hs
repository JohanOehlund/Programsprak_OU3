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
data Vertex a= V{name::Integer,
                    wV::a} deriving (Ord)


data Edge a b=  E {frV::(Vertex a),
                   toV::(Vertex a),
                    wE::b} deriving (Ord)

data DAG a b= DAGImpl [Vertex a] [b] deriving (Show)

instance (Eq a,Eq b) => Eq (Edge a b) where
    (==) (E v1 v2 wt1) (E v3 v4 wt2) = (v1 == v3) && (v2 == v4)
    (/=) (E v1 v2 wt1) (E v3 v4 wt2) = (v1 /= v3) || (v2 /= v4)

instance (Eq a) => Eq (Vertex a) where
    (==) (V a1 b1) (V a2 b2)= (a1 == a2) && (b1 == b2)
    (/=) (V a1 b1) (V a2 b2)= (a1 /= a2) || (b1 /= b2)

instance (Show a) => Show (Vertex a) where
    show  (V a b)= "(V "++ show a ++ " " ++ show b ++ ")"

instance (Show a, Show b) => Show (Edge a b) where
    show  (E v1 v2 wt)= "(E "++ show v1 ++ " " ++ show v2 ++ show wt++")"

 
{-
topological_ordering :: (Eq a,Eq b)=> DAG (Vertex a) (Edge a b) -> [(Vertex a)] -> [(Vertex a)] -> [(Vertex a)]
topological_ordering (DAGImpl ([],_)) _ output  = output
topological_ordering (DAGImpl (((V n1 wt1):xs),_)) [] _  = []
topological_ordering (DAGImpl (((V n1 wt1):xs),e)) ((V n2 wt2):xs2) output
        | ((isToEdge) (V n2 wt2) e) == True = (topological_ordering) (DAGImpl (((V n1 wt1):xs),e)) xs2 ((V n2 wt2):output)
        | otherwise = (topological_ordering) (DAGImpl (fVertList,fEdgeList)) fVertList output
                    where fVertList = [res |res  <-((V n1 wt1):xs), ((/=) res  (V n2 wt2))]
                          fEdgeList = [res2|res2 <- e    , ((/=) ((getFromVert)res2) (V n2 wt2))]
-}
getFromVert :: (Edge a b) -> (Vertex a)
getFromVert (E v1 _ _) = v1

getToVert :: (Edge a b) -> (Vertex a)
getToVert (E _ v2 _) = v2




empty :: DAG a b
empty = DAGImpl [] []

--returns tuple with new dag and node idetifier.
add_vertex :: DAG a b -> a -> (DAG a b, Integer)
add_vertex (DAGImpl v e) wt = ((DAGImpl ((V newID wt):v) e),newID)
            where newID = getNewVID v 0
 
getNewVID :: [Vertex a] -> Integer -> Integer
getNewVID vList num 
    | (containsID) vList num = getNewVID vList (num+1)
    | otherwise = num 

containsID :: [Vertex a] -> Integer -> Bool
containsID [] _ = False
containsID ((V id1 _):xs) id2 = if id1==id2 then True else containsID xs id2

{-

add_edge :: (Eq a,Eq b)=> (Edge a b) -> DAG (Vertex a) (Edge a b)-> DAG (Vertex a) (Edge a b)
add_edge  (E v1 v2 wt) (DAGImpl (v,e)) = 
                if ((containsVert) v1 (DAGImpl (v,e))) && ((containsVert) v2 (DAGImpl (v,e))) 
                    && (length((topological_ordering) (DAGImpl (v,((E v1 v2 wt):e))) v []) > 0)
                then DAGImpl (v,((E v1 v2 wt):e))  
                else error "Can't add edge, creats a cycle."      

containsVert ::(Eq a,Eq b) => a -> DAG a b -> Bool
containsVert v1 (DAGImpl ([],e)) = False
containsVert v1 (DAGImpl ((v2:xs),e)) = if v1 == v2 
                                        then True 
                                        else containsVert v1 (DAGImpl ((xs),e)) 


containsTrue :: [Bool] -> Bool
containsTrue list = if True `elem` list then True else False


getEdgeList :: DAG (Vertex a) (Edge a b) -> [(Edge a b)]
getEdgeList (DAGImpl (_,e)) = e

getVertList :: DAG (Vertex a) (Edge a b) -> [(Vertex a)]
getVertList (DAGImpl (v,_)) = v


isFromEdge :: (Eq a,Eq b)=> (Vertex a) -> [(Edge a b)] -> Bool
isFromEdge _ [] = False
isFromEdge (V n1 wt1) ((E (V n2 wt2) (V n3 wt3) wt):xs)
    | n1 == n2 = True
    | otherwise = isFromEdge (V n1 wt1) xs


isToEdge :: (Eq a,Eq b) => (Vertex a) -> [Edge a b] -> Bool
isToEdge _ [] = False
isToEdge (V n1 wt1) ((E _ (V n2 wt2) wt):xs)
    | n1 == n2 = True
    | otherwise = isToEdge (V n1 wt1) xs

--weight_of_longest_path :: DAG (Vertice a b) (Edge a b)  -> (Vertice a b) -> (Vertice a b) -> ((Vertice a b) -> (Vertice a b)) -> ((Vertice a b) -> (Vertice a b)) -> b
--weight_of_longest_path




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



compareVert :: (Eq a,Eq b)=> (Vertice a b) -> (Vertice a b) -> Bool
compareVert (V n1 _) (V n2 _)
    | n1 /= n2 = True
    | otherwise = False

compareEdge :: (Eq a,Eq b)=> (Edge a b) ->(Vertice a b) -> Bool
compareEdge (E (V n2 _) _ _) (V n1 _)
    | n1 /= n2 = True
    | otherwise = False


noCycle :: (Eq a,Eq b) => (Vertice a b) -> [Vertice a b] -> [Edge a b] -> [Bool]
noCycle _ [] _ = [True]
noCycle v1 (v2:xs) e 
    | v1 == v2 = [False]
    | otherwise = ((noCycle) v1 (getElem v2 e) e) ++ ((noCycle) v1 (xs) e)

getElem :: (Eq a,Eq b) => (Vertice a b) -> [Edge a b] -> [Vertice a b]
getElem _ [] = []
getElem v1 ((E v2 v3 wt):e) = if v1 == v2 then v3 
    : (getElem v1 (e)) else (getElem v1 (e))


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

topological_ordering dag12 (getVertList dag12) []
################################################################################

https://wiki.haskell.org/Abstract_data_type
-}
