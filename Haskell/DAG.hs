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
import Data.Char
import Data.List
import Data.Typeable
import Data.Either

type VertID= Integer

data Vertex a= V{name::Integer,
                    wV::a} deriving (Ord)


data Edge a =  E {frV::(Vertex a),
                   toV::(Vertex a),
                    wE::a} deriving (Ord)

data DAG a= DAGImpl [Vertex a] [Edge a] deriving (Show)

instance (Eq a) => Eq (Edge a) where
    (==) (E v1 v2 wt1) (E v3 v4 wt2) = (v1 == v3) && (v2 == v4)
    (/=) (E v1 v2 wt1) (E v3 v4 wt2) = (v1 /= v3) || (v2 /= v4)

instance (Eq a) => Eq (Vertex a) where
    (==) (V a1 b1) (V a2 b2)= (a1 == a2)
    (/=) (V a1 b1) (V a2 b2)= (a1 /= a2) 

instance (Show a) => Show (Vertex a) where
    show  (V a b)= "(V "++ show a ++ " " ++ show b ++ ")"

instance (Show a) => Show (Edge a) where
    show  (E v1 v2 wt)= "(E "++ show v1 ++ " " ++ show v2 ++" "++ show wt++")"

topological_ordering :: Eq a => DAG a -> [Vertex a] -> [Vertex a] -> [Vertex a]
topological_ordering (DAGImpl [] _) _ output  = output
topological_ordering (DAGImpl _ _) [] _  = error "DAG is not acyclic!"
topological_ordering (DAGImpl v e) (x2:xs2) output
        | ((isToEdge) x2 e) == True = (topological_ordering) (DAGImpl v e) xs2 output
        | otherwise = (topological_ordering) (DAGImpl fVertList fEdgeList) fVertList (x2:output)
                    where fVertList = [res |res  <- v , ((/=) res  x2)]
                          fEdgeList = [res2|res2 <- e , ((/=) ((getFromVert)res2) x2)]


isToEdge :: Vertex a -> [Edge t] -> Bool
isToEdge _ [] = False
isToEdge (V n1 wt1) ((E _ (V n2 wt2) wt):xs)
    | n1 == n2 = True
    | otherwise = isToEdge (V n1 wt1) xs

getFromVert :: (Edge a) -> (Vertex a)
getFromVert (E v1 _ _) = v1

getToVert :: (Edge a) -> (Vertex a)
getToVert (E _ v2 _) = v2


empty :: DAG a
empty = DAGImpl [] []


add_vertex :: DAG a -> a -> (DAG a, VertID)
add_vertex (DAGImpl v e) wt = ((DAGImpl ((V newID wt):v) e),newID)
            where newID = getNewVID v 1
 
getNewVID :: [Vertex a] -> VertID -> VertID
getNewVID vList num 
    | (containsID) vList num = getNewVID vList (num+1)
    | otherwise = num 

containsID :: [Vertex a] -> VertID -> Bool
containsID [] _ = False
containsID ((V id1 _):xs) id2 = if id1==id2 then True else containsID xs id2


add_edge :: Eq a => DAG a -> VertID -> VertID -> a -> DAG a
add_edge (DAGImpl v e) id1 id2 wt = 
                if ((notContainsVert) (DAGImpl v e) id1) && ((notContainsVert) (DAGImpl v e) id2) 
                    && (length((topological_ordering) (DAGImpl v ((E getV1 getV2 wt):e)) v [] ) > 0)
                then (DAGImpl v ((E getV1 getV2 wt):e))
                else error "Can't add edge, creats a cycle."
                    where   getV1 = getVert (DAGImpl v e) id1
                            getV2 = getVert (DAGImpl v e) id2




notContainsVert :: DAG a -> VertID -> Bool
notContainsVert (DAGImpl [] e) id1 = False
notContainsVert (DAGImpl v e)  id1 = if ((getVertId)$(head) v) == id1
                                    then True 
                                    else notContainsVert (DAGImpl ((tail) v) e) id1

getVertId :: (Vertex a) -> Integer
getVertId (V id1 _)= id1


getVertList :: DAG a-> [(Vertex a)]
getVertList (DAGImpl v _) = v

getEdgeList :: DAG a-> [Edge a]
getEdgeList (DAGImpl _ e) = e

nodeWT :: DAG a-> VertID -> a
nodeWT dag id1=  getVertWT $ getVert dag id1 

edgeWT :: (Num a, Eq a) => DAG a -> Vertex a -> Vertex a -> a
edgeWT dag v1 v2 = getEdgeWT $ getEdge dag v1 v2

getVert :: DAG a-> VertID -> Vertex a
getVert (DAGImpl [] e) _ = error "No Vertex found!"
getVert (DAGImpl v e) id1 
    | ((getVertId)$(head) v) == id1 = (head) v
    | otherwise = ((getVert) (DAGImpl ((tail) v) e) id1)

getEdge :: (Eq a,Num a) => DAG a -> Vertex a -> Vertex a -> Edge a
getEdge (DAGImpl v []) _ _ = error "No edge found in getEdge!"
getEdge (DAGImpl v ((E v1 v2 wt):xs)) v3 v4
    | v1 == v3 && v2 == v4 = (E v1 v2 wt)
    | otherwise = getEdge (DAGImpl v xs) v3 v4


getVertWT :: (Vertex a) -> a
getVertWT (V _ wt)= wt

getEdgeWT :: (Edge a) -> a
getEdgeWT (E _ _ wt)= wt


weight_of_longest_path dag id1 id2 f g = last $ sort(test dag paths f g)
       -- | show (typeOf (getVertWT vert1)) == "Integer" = Left ((test dag paths f g))
       -- | show (typeOf (getVertWT vert1)) == "Char" = Right (test2 dag paths f g) 
            where 
                paths = (clrPaths) (getPaths dag vert2 getNeigh [vert1])
                vert1 = getVert dag id1
                vert2 = getVert dag id2
                getNeigh = getToVerts vert1 (getEdgeList dag) 

test2 dag [] f g = []
test2 dag (x:xs) f g = calcCharWeight  dag x f g : test2 dag xs f g 


--test :: (Typeable a1, Num [a]) => t -> [[a1]] -> (a1 -> [a]) -> (t -> a1 -> a1 -> [a]) -> [[a]]
test dag [] f g = []
test dag (x:xs) f g = calcIntWeight  dag x f g : test dag xs f g 

calcCharWeight :: t -> [a1] -> (a1 -> [a]) -> (t -> a1 -> a1 -> [a]) -> [a]
calcCharWeight dag (x:[]) f g = (f x)
calcCharWeight dag (x:x1:xs) f g = ((f x) ++ (g dag x x1)) ++ calcCharWeight dag (x1:xs) f g

calcIntWeight :: Num a => t -> [a1] -> (a1 -> a) -> (t -> a1 -> a1 -> a) -> a
calcIntWeight dag (x:[]) f g = (f x)
calcIntWeight dag (x:x1:xs) f g = (f x) + (g dag x x1) + calcIntWeight dag (x1:xs) f g

getPaths :: (Eq a) => DAG a -> Vertex a -> [Vertex a] -> [Vertex a] -> [[Vertex a]]
getPaths _ toVert [] output = if head(output) == toVert then [reverse output] else [[]]
getPaths dag toVert (fNeigh:rNeigh) output
    | head(output) == toVert = [reverse output]
    | otherwise = getPaths dag toVert (getToVerts fNeigh (getEdgeList dag)) (fNeigh : output)
                 ++ getPaths dag toVert rNeigh output


getToVerts :: (Eq a) => (Vertex a) -> [Edge a] -> [Vertex a]
getToVerts _ [] = []
getToVerts v1 ((E v2 v3 wt):e) = if v1 == v2 
                                then (v3:(getToVerts v1 (e)))
                                else (getToVerts v1 (e))

clrPaths :: (Eq a) => [[Vertex a]] -> [[Vertex a]]
clrPaths [] = []
clrPaths (x:xs) = if x == [] then clrPaths xs else x :clrPaths xs

{-
compareVert :: (Eq a)=> (Vertex a) -> (Vertex a) -> Bool
compareVert (V n1 _) (V n2 _)
    | n1 /= n2 = True
    | otherwise = False

compareEdge :: (Eq a,Eq b)=> (Edge a) ->(Vertex a) -> Bool
compareEdge (E (V n2 _) _ _) (V n1 _)
    | n1 /= n2 = True
    | otherwise = False
    

test dag13 [(V 2 2)] (V 1 1) (V 2 2)
test dag13 [(V 2 2),(V 4 4)] (V 1 1) (V 4 4)

################################# TEST CODE ####################################

let dag1 = add_vertex empty      1 
let dag2 = add_vertex (fst dag1) 2 
let dag3 = add_vertex (fst dag2) 3 
let dag4 = add_vertex (fst dag3) 4 
let dag5 = add_vertex (fst dag4) 5 
let dag6 = add_vertex (fst dag5) 6 

let dag7  =  add_edge (fst dag6)  (snd dag1) (snd dag2) 10
let dag8  =  add_edge      dag7   (snd dag2) (snd dag4) 11
let dag9  =  add_edge      dag8   (snd dag4) (snd dag5) 12
let dag10 =  add_edge      dag9   (snd dag3) (snd dag6) 13
let dag11 =  add_edge      dag10  (snd dag1) (snd dag4) 14
let dag12 =  add_edge      dag11  (snd dag5) (snd dag3) 15
let dag13 =  add_edge      dag12  (snd dag2) (snd dag5) 15

let x = clrPaths (getPaths dag13 (V 5 5) [(V 2 2), (V 4 4)] [(V 1 1)])
calcIntWeight dag13 (head x) (getVertWT) (edgeWT)
weight_of_longest_path dag13 1 5 (getVertWT) (edgeWT)

getPaths dag13 (getToVerts (getVert dag13 1) (getEdgeList dag13)) (V 5 5) 

topological_ordering dag13 (getVertList dag13) []

weight_of_longest_path dag12 1 2 (nodeWT) (edgeWT)


################################################################################

################################# TEST CODE OLD ####################################
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
