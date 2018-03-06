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

{-


##############################TEST CODE NUM ####################################

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

let x = weight_of_longest_path dag13 1 5 (vertWT) (edgeWT)

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

let x = weight_of_longest_path dag13 1 5 (vertWT) (edgeWT)



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

let x = weight_of_longest_path dag13 1 5 (vertWT) (edgeWT)




################################################################################




https://wiki.haskell.org/Abstract_data_type
-}
