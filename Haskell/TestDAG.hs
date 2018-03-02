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
module TestDAG (dagNum,dagChar,dagString) where

data Vertex a= V{name::Integer,
                    wV::a} 


data Edge a =  E {frV::(Vertex a),
                   toV::(Vertex a),
                    wE::a} 

data DAG a= DAGImpl [Vertex a] [Edge a] deriving (Show)

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


instance (Show a) => Show (Vertex a) where
    show  (V a b)= "(V "++ show a ++ " " ++ show b ++ ")"

instance (Show a) => Show (Edge a) where
    show  (E v1 v2 wt)= "(E "++ show v1 ++ " " ++ show v2 ++" "++ show wt++")"

dagNum = DAGImpl [(V 6 6),(V 5 5),(V 4 4),(V 3 3),(V 2 2),(V 1 1)] [(E (V 2 2) (V 5 5) 15),(E (V 5 5) (V 3 3) 15),(E (V 1 1) (V 4 4) 14),(E (V 3 3) (V 6 6) 13),(E (V 4 4) (V 5 5) 122),(E (V 2 2) (V 4 4) 11),(E (V 1 1) (V 2 2) 100)] 

dagChar = DAGImpl [(V 6 'j'),(V 5 'q'),(V 4 'h'),(V 3 'z'),(V 2 'd'),(V 1 'a')] [(E (V 2 'd') (V 5 'q') 'm'),(E (V 5 'q') (V 3 'z') 'u'),(E (V 1 'a') (V 4 'h') 'V'),(E (V 3 'z') (V 6 'j') 'A'),(E (V 4 'h') (V 5 'q') 'a'),(E (V 2 'd') (V 4 'h') 'c'),(E (V 1 'a') (V 2 'd') '1')]

dagString = DAGImpl [(V 6 "AG"),(V 5 "AF"),(V 4 "AE"),(V 3 "AD"),(V 2 "AC"),(V 1 "AB")] [(E (V 2 "AC") (V 5 "AF") "AN"),(E (V 5 "AF") (V 3 "AD") "AM"),(E (V 1 "AB") (V 4 "AE") "AL"),(E (V 3 "AD") (V 6 "AG") "AH"),(E (V 4 "AE") (V 5 "AF") "AJ"),(E (V 2 "AC") (V 4 "AE") "AI"),(E (V 1 "AB") (V 2 "AC") "AH")]

{-
##############################TEST CODE NUM ####################################

let dag1 = add_vertex empty      1 
let dag2 = add_vertex (fst dag1) 2 
let dag3 = add_vertex (fst dag2) 3 
let dag4 = add_vertex (fst dag3) 4 
let dag5 = add_vertex (fst dag4) 5 
let dag6 = add_vertex (fst dag5) 6 

let dag7  =  add_edge (fst dag6)  (snd dag1) (snd dag2) 100
let dag8  =  add_edge      dag7   (snd dag2) (snd dag4) 11
let dag9  =  add_edge      dag8   (snd dag4) (snd dag5) 122
let dag10 =  add_edge      dag9   (snd dag3) (snd dag6) 13
let dag11 =  add_edge      dag10  (snd dag1) (snd dag4) 14
let dag12 =  add_edge      dag11  (snd dag5) (snd dag3) 15
let dag13 =  add_edge      dag12  (snd dag2) (snd dag5) 15


weight_of_longest_path dag13 1 5 (getVertWT) (edgeWT) (calcIntWeight)

getPaths dag13 (getToVerts (getVert dag13 1) (getEdgeList dag13)) (V 5 5) 

topological_ordering dag13 (getVertList dag13) []

let x = clrPaths (getPaths dag13 (V 5 5) [(V 2 2), (V 4 4)] [(V 1 1)])
calcIntWeight dag13 (head x) (getVertWT) (edgeWT)


################################################################################

##############################TEST CODE Char####################################

let dag1 = add_vertex empty      'a' 
let dag2 = add_vertex (fst dag1) 'd' 
let dag3 = add_vertex (fst dag2) 'z' 
let dag4 = add_vertex (fst dag3) 'h' 
let dag5 = add_vertex (fst dag4) 'q' 
let dag6 = add_vertex (fst dag5) 'j' 

let dag7  =  add_edge (fst dag6)  (snd dag1) (snd dag2) '1'
let dag8  =  add_edge      dag7   (snd dag2) (snd dag4) 'c'
let dag9  =  add_edge      dag8   (snd dag4) (snd dag5) 'a'
let dag10 =  add_edge      dag9   (snd dag3) (snd dag6) 'A'
let dag11 =  add_edge      dag10  (snd dag1) (snd dag4) 'V'
let dag12 =  add_edge      dag11  (snd dag5) (snd dag3) 'u'
let dag13 =  add_edge      dag12  (snd dag2) (snd dag5) 'm'

weight_of_longest_path dag13 1 5 (getVertWT) (edgeWT) (calcCharWeight)




################################################################################

##############################TEST CODE String##################################

let dag1 = add_vertex empty      "AB" 
let dag2 = add_vertex (fst dag1) "AC"  
let dag3 = add_vertex (fst dag2) "AD"  
let dag4 = add_vertex (fst dag3) "AE" 
let dag5 = add_vertex (fst dag4) "AF"  
let dag6 = add_vertex (fst dag5) "AG" 

let dag7  =  add_edge (fst dag6)  (snd dag1) (snd dag2) "AH" 
let dag8  =  add_edge      dag7   (snd dag2) (snd dag4) "AI" 
let dag9  =  add_edge      dag8   (snd dag4) (snd dag5) "AJ" 
let dag10 =  add_edge      dag9   (snd dag3) (snd dag6) "AH" 
let dag11 =  add_edge      dag10  (snd dag1) (snd dag4) "AL" 
let dag12 =  add_edge      dag11  (snd dag5) (snd dag3) "AM" 
let dag13 =  add_edge      dag12  (snd dag2) (snd dag5) "AN" 

weight_of_longest_path dag13 1 5 (getVertWT) (edgeWT) (calcStringWeight)




################################################################################




https://wiki.haskell.org/Abstract_data_type
-}
