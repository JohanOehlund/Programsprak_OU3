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
{-# LANGUAGE FlexibleInstances #-}
module DAG(Vertex,Edge,DAG,VertID,add_vertex,add_edge,topological_ordering,weight_of_longest_path,empty,
            vertWT,edgeWT) where

import Data.Char
import Data.List

-- ############################################## Data and Types ####################################################### 
-- Comment: All macros and data types used for the DAG (directed acyclic graph)


type VertID= Integer

data Vertex a= V{name::Integer,
                    wV::a} 


data Edge a =  E {frV::(Vertex a),
                   toV::(Vertex a),
                    wE::a} 

data DAG a= DAGImpl [Vertex a] [Edge a] deriving (Show)

-- ##################################################################################################################### 


-- ########################################## Classes and instances #################################################### 
-- Comment: The own created CalcWeight is used to caculate the different types of weights of the DAG 
--          (directed acyclic graph). The instances includes instance of types (Ord,Show,Eq and CalcWeight). 

class (Eq a,Ord a) => CalcWeight a where 
    calcPath :: [Vertex a] -> [Edge a] -> (Vertex a -> a) -> ([Edge a]->Vertex a ->Vertex a -> a) -> [a]
    --add :: (Vertex a -> Vertex a -> [a])
    --getWT :: Vertex a-> [a]
    --compare' :: (a -> a -> Ordering)

instance (CalcWeight [Char]) where
    calcPath vList eList f g  = [(calcStringWeight vList eList f g)]
    --longestPath list = ((head) (sort list))

instance CalcWeight Char where
    calcPath vList eList f g  = (calcCharWeight vList eList f g)
    --longestPath list = ((head) (sort list))
    --add a1 a2 = [a1] ++ [a2]
    --getWT a1 = [a1]
    --compare' a1 a2 = compare a1 a2  

instance CalcWeight Integer where
    calcPath vList eList f g  = [sum (calcIntWeight vList eList f g)]
    --longestPath list = ((last) (sort list))
    --add a1 a2 = [a1 + a2]
    --getWT a1 = [a1] 
    --compare' a1 a2 = compare a1 a2

instance CalcWeight Double where
    calcPath vList eList f g  = [sum (calcIntWeight vList eList f g)]
    --longestPath list = ((last) (sort list))

instance (Show a) => Show (Vertex a) where
    show  (V a b)= "(V "++ show a ++ " " ++ show b ++ ")"

instance (Show a) => Show (Edge a) where
    show  (E v1 v2 wt)= "(E "++ show v1 ++ " " ++ show v2 ++" "++ show wt++")"

instance (Eq a) => Eq (Edge a) where
    (==) (E v1 v2 wt1) (E v3 v4 wt2) = (v1 == v3) && (v2 == v4)
    (/=) (E v1 v2 wt1) (E v3 v4 wt2) = (v1 /= v3) || (v2 /= v4)

instance (Eq a) => Eq (Vertex a) where
    (==) (V a1 b1) (V a2 b2)= (a1 == a2)
    (/=) (V a1 b1) (V a2 b2)= (a1 /= a2) 

instance (Ord a) => Ord (Vertex a) where
    (>=) (V a1 b1) (V a2 b2)= (>=) b1 b2
    (<)  (V a1 b1) (V a2 b2)= (<) b1 b2
    (<=) (V a1 b1) (V a2 b2)= (<=) b1 b2
    compare (V a1 b1) (V a2 b2)= compare b1 b2

instance (Ord a) => Ord (Edge a) where
    (>=) (E v1 v2 wt1) (E v3 v4 wt2)  = (>=) wt1 wt2
    (<=) (E v1 v2 wt1) (E v3 v4 wt2)  = (<=)  wt1 wt2
    (<)  (E v1 v2 wt1) (E v3 v4 wt2)  = (<)  wt1 wt2
    compare (E v1 v2 wt1) (E v3 v4 wt2)  = compare  wt1 wt2
-- ##################################################################################################################### 

longestPath :: (Ord a) =>  [[a]] -> [a]
longestPath list = ((last) (sort list))


{-
Function: empty
Comment: Creates an empty DAG (directed acyclic graph).
-}
empty :: DAG a
empty = DAGImpl [] []

{-
Function: add_vertex
Comment: Adds a new vertex to the DAG. The new DAG (with the new vertex) is returned in a tuple with
            the newly created vertex ID.  
-}
add_vertex :: DAG a -> a -> (DAG a, VertID)
add_vertex (DAGImpl v e) wt = ((DAGImpl ((V newID wt):v) e),newID)
            where newID = getNewVID v 1

{-
Function: add_edge
Comment: Adds a edge between two vertices in the DAG. The new DAG (with the new edge) is returned.  
-}
add_edge :: Eq a => DAG a -> VertID -> VertID -> a -> DAG a
add_edge (DAGImpl v e) id1 id2 wt = 
                if ((containsID v) id1) && ((containsID v) id2) 
                    && (length((topological_ordering) (DAGImpl v ((E getV1 getV2 wt):e)) v [] ) > 0)
                then (DAGImpl v ((E getV1 getV2 wt):e))
                else error "Can't add edge, creats a cycle."
                    where   getV1 = getVert (DAGImpl v e) id1
                            getV2 = getVert (DAGImpl v e) id2

{-
Function: topological_ordering
Comment: Performs a topological sorting of the DAG and returns a sorted list of vertices.  
-}
topological_ordering :: Eq a => DAG a -> [Vertex a] -> [Vertex a] -> [Vertex a]
topological_ordering (DAGImpl [] _) _ output  = output
topological_ordering (DAGImpl _ _) [] _  = error "DAG is not acyclic!"
topological_ordering (DAGImpl v e) (x2:xs2) output
        | ((isToEdge) x2 e) == True = (topological_ordering) (DAGImpl v e) xs2 output
        | otherwise = (topological_ordering) (DAGImpl fVertList fEdgeList) fVertList (x2:output)
                    where fVertList = [res |res  <- v , ((/=) res  x2)]
                          fEdgeList = [res2|res2 <- e , ((/=) ((getFromVert)res2) x2)]

{-
Function: weight_of_longest_path
Comment: Calculates the longest path between the two selected vertices in the DAG. 
-}
weight_of_longest_path dag id1 id2 f g = longestPath ((weight_of_longest_path') dag paths f g)
            where 
                paths = (clrPaths) (getPaths dag vert2 getNeigh [vert1])
                vert1 = getVert dag id1
                vert2 = getVert dag id2
                getNeigh = getNeighbours vert1 (getEdgeList dag) 

{-
Function: weight_of_longest_path'
Comment: Help function to weight_of_longest_path that recursive calculates a path between vertices.
-}
weight_of_longest_path' dag [] f g = []
weight_of_longest_path' dag (x:xs) f g = ((calcPath) x (getEdgeList dag) f g) : weight_of_longest_path' dag xs f g

{-
Function: calcCharWeight
Comment: Calculates a paths weight (where the weights have the type Char).
-}
calcCharWeight :: [a1] -> t -> (a1 -> a) -> (t -> a1 -> a1 -> a) -> [a]
calcCharWeight (x:[]) eList f g = [(f x)]
calcCharWeight (x:x1:xs) eList f g = ([(f x)] ++ [(g eList x x1)]) ++ calcCharWeight (x1:xs) eList f g

{-
Function: calcStringWeight
Comment: Calculates a paths weight (where the weights have the type String).
-}
calcStringWeight :: [a1] -> t -> (a1 -> [a]) -> (t -> a1 -> a1 -> [a]) -> [a]
calcStringWeight (x:[]) eList f g = (f x)
calcStringWeight (x:x1:xs) eList f g = (f x) ++ (g eList x x1) ++ calcStringWeight (x1:xs) eList f g

{-
Function: calcIntWeight
Comment: Calculates a paths weight (where the weights have the type Integer).
-}
calcIntWeight :: Num a => [a1] -> t -> (a1 -> a) -> (t -> a1 -> a1 -> a) -> [a]
calcIntWeight (x:[]) eList f g = [f x]
calcIntWeight (x:x1:xs) eList f g = (f x) + (g eList x x1) : calcIntWeight (x1:xs) eList f g


{-
Function: isToEdge
Comment: Returns True if the selected vertex has an edge that is pointed to the vertex else returns false.
-}
isToEdge :: Vertex a -> [Edge t] -> Bool
isToEdge _ [] = False
isToEdge (V n1 wt1) ((E _ (V n2 wt2) wt):xs)
    | n1 == n2 = True
    | otherwise = isToEdge (V n1 wt1) xs

 
{-
Function: getNewVID
Comment: Returns the next free vertex identifier.
-}
getNewVID :: [Vertex a] -> VertID -> VertID
getNewVID vList num 
    | (containsID) vList num = getNewVID vList (num+1)
    | otherwise = num 

{-
Function: containsID
Comment: Checks if a selected vertex id is free to use, returns True if id is in use else false.
-}
containsID :: [Vertex a] -> VertID -> Bool
containsID [] _ = False
containsID ((V id1 _):xs) id2 = if id1==id2 then True else containsID xs id2

{-
Function: getVert
Comment: Returns a vertex from the DAG (if possible) with the same id as the VertID.
-}
getVert :: DAG a-> VertID -> Vertex a
getVert (DAGImpl [] e) _ = error "No Vertex found!"
getVert (DAGImpl v e) id1 
    | ((getVertId)$(head) v) == id1 = (head) v
    | otherwise = ((getVert) (DAGImpl ((tail) v) e) id1)

{-
Function: getVert
Comment: Returns a edge from the DAG (if possible).
-}
getEdge :: (Eq a) => DAG a -> Vertex a -> Vertex a -> Edge a
getEdge (DAGImpl v []) _ _ = error "No edge found in getEdge!"
getEdge (DAGImpl v ((E v1 v2 wt):xs)) v3 v4
    | v1 == v3 && v2 == v4 = (E v1 v2 wt)
    | otherwise = getEdge (DAGImpl v xs) v3 v4


{-
Function: edgeWT
Comment: Returns the weight of a selected edge from the DAG (if possible).
-}
edgeWT :: (Eq a) => [Edge a]-> Vertex a -> Vertex a -> a
edgeWT [] _ _ = error "Could not find edge in edgeWT"
edgeWT (x:xs) v1 v2 = if ((getFromVert x) == v1) && ((getToVert x) == v2)  
                            then (getEdgeWT x)
                            else edgeWT xs v1 v2

{-
Function: getPaths
Comment: Returns all possible paths from vertex a to vertex b.
            if list is empty then it has no possible paths from vertex a to vertex b.
-}
getPaths :: (Eq a) => DAG a -> Vertex a -> [Vertex a] -> [Vertex a] -> [[Vertex a]]
getPaths _ toVert [] output = if head(output) == toVert then [reverse output] else [[]]
getPaths dag toVert (fNeigh:rNeigh) output
    | head(output) == toVert = [reverse output]
    | otherwise = getPaths dag toVert (getNeighbours fNeigh (getEdgeList dag)) (fNeigh : output)
                 ++ getPaths dag toVert rNeigh output

{-
Function: getNeighbours
Comment: Returns the closest neighbours to a vertex.
-}
getNeighbours :: (Eq a) => (Vertex a) -> [Edge a] -> [Vertex a]
getNeighbours _ [] = []
getNeighbours v1 ((E v2 v3 wt):e) = if v1 == v2 
                                        then (v3:(getNeighbours v1 (e)))
                                        else (getNeighbours v1 (e))

-- ################################################# Help functions #################################################### 
-- Comment: Smaller help functions that DAG.hs uses. 

clrPaths :: (Eq a) => [[Vertex a]] -> [[Vertex a]]
clrPaths [] = []
clrPaths (x:xs) = if x == [] then clrPaths xs else x :clrPaths xs

getEdgeWT :: (Edge a) -> a
getEdgeWT (E _ _ wt)= wt

vertWT :: (Vertex a) -> a
vertWT (V _ wt)= wt

getFromVert :: (Edge a) -> (Vertex a)
getFromVert (E v1 _ _) = v1

getToVert :: (Edge a) -> (Vertex a)
getToVert (E _ v2 _) = v2

getVertId :: (Vertex a) -> Integer
getVertId (V id1 _)= id1

getEdgeList :: DAG a-> [Edge a]
getEdgeList (DAGImpl _ e) = e
-- ##################################################################################################################### 

-- let x = (clrPaths) (getPaths dag13 (V 5 5)  (getNeighbours (V 1 1) (getEdgeList dag13))  [(V 1 1)])
-- let y = weight_of_longest_path' dag13 x (vertWT) (edgeWT)