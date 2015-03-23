module GraphOps where

import GraphType (..)
import Dict as D
import List ((::))
import List
import Debug
import Set as S



--Basic Functions for manipulating Graphs----------------------------------------
--Some of these functions is hella inefficent
-- my algorithmist street cred decreases with every line of code

--'insert' inserts a node, along with any possible edges it may have, into a graph
insert : (comparable, Node) -> List (Edge comparable) -> Graph comparable -> Graph comparable
insert (k,n) es graph = case graph of
    E -> G (D.singleton k n) [] --E
    G ns' es' -> let
                    filtered = List.filter (\x -> verifyEdge x graph) es --filter out inappropriate edges
                  in
                    case filtered of
                      [] -> G (D.insert k n ns') (es')
                      xs -> G (D.insert k n ns') (xs ++ es')
-- we best be careful about key collisions

{-function to get a possible node in a graph based on a key-}
get : comparable -> Graph comparable -> Maybe Node
get k graph = case graph of
    E -> Nothing
    G ns es -> D.get k ns


inducedSubgraph : List comparable -> Graph comparable -> Graph comparable
inducedSubgraph keys (G ns es) = let keyset = S.fromList keys
                                     edges = List.filter (\x -> adamLevine x keyset) es
                                     nodes = List.filter (\x -> S.member (fst x) keyset) (D.toList ns)
                                 in
                                     G (D.fromList nodes) edges


-- is an edge incident to k?
incident : comparable -> Edge comparable -> Bool
incident k edge =
  if | edge.source == k || edge.sink == k -> True
     | otherwise -> False

--remove an edge, debug crash should never have to go off
removeEdge : Edge comparable -> Graph comparable -> Graph comparable
removeEdge e g = case g of
  E -> E
  G ns es -> if (verifyEdge e g)
                then G ns (List.filter (\s -> s /= e) es)
                else Debug.crash "One or more of the nodes in given edge are not within the graph"

--neighboring edges
neighbors : comparable -> Graph comparable -> List (Edge comparable)
neighbors k graph = case graph of
  E -> []
  G ns es -> List.filter (\x -> incident k x) es

--neighboring nodes
neighborNodes : comparable -> Graph comparable -> List comparable
neighborNodes k graph = let edges = neighbors k graph
                            mcride edge = if k == edge.source then edge.sink else edge.source 
                        in
                            List.map mcride edges

--embarrassing, but eh, why not?
fromJust : Maybe a -> a
fromJust mx = case mx of Just x -> x

--see above
justGet i g = fromJust (get i g)

--connect two nodes in a graph denoted by their keys
connect : Edge comparable -> Graph comparable -> Graph comparable
connect e g = case g of
    E -> E
    G ns es -> if (verifyEdge e g) -- check if an edge is appropriate
                  then G ns (e::es)
                  else Debug.crash "One or more of the nodes in given edge are not within the graph"


--verify by comparable key that a particular node is located within a graph
verify : comparable -> Graph comparable -> Bool
verify k g = case g of
    E -> False
    G ns es -> D.member k ns


--verify if a particular edge is allowed
verifyEdge : Edge comparable -> Graph comparable -> Bool
verifyEdge e g = case g of
  E -> False
  _ -> (verify e.source g) && (verify e.sink g)

{-checks if edge is incedent to some set-}
adamLevine edge set = (S.member edge.source set) && (S.member edge.sink set)

--delete a node from a graph based on its key, and remove any of its associated edges
deleteNode : comparable -> Graph comparable -> Graph comparable
deleteNode k graph = case graph of
  G ns es -> let ns' = D.remove k ns
                 incedent = (\x -> x.source == k || x.sink == k)
                 es' = List.filter (\x -> not <| incedent x) es 
             in
                 G ns' es'

-- reverse the source and sink of an edge
edgeReverse e = {source = e.sink, sink = e.source}

-- checks if e
queryGraphHelp k k' es = case es of
  [] -> False
  e'::es' -> let e = {source = k, sink = k'} in e == e' || e == edgeReverse e' || queryGraphHelp k k' es'
 
--querying a graph for connections between two nodes
--queryGraph : Edge comparable -> Graph comparable -> Bool
queryGraph k k' g = case g of
  E      -> False
  G _ es -> queryGraphHelp k k' es

degree k g = List.length (neighbors k g)

degreeseq : Graph comparable -> List Int
degreeseq g = case g of
  E -> []
  G ns es -> List.reverse <| List.sort <| (List.map (\x -> degree (fst x) g)) <| D.toList <| ns


-- returns selected keys
selectedNodes : Graph comparable -> List comparable
selectedNodes g = case g of
  E       -> []
  G ns es -> let foo (key, node) = node.glow in List.map fst (List.filter foo (D.toList ns))

getcolor node = node.colour

colorsinGraph : Graph comparable -> List Int
colorsinGraph g = case g of
  E -> []
  G ns es -> (S.toList (S.fromList (List.map (\x -> getcolor (snd x)) (D.toList ns))))

macklemore k k' g = let knode = justGet k g
                        knode' = justGet k' g in knode.colour == knode'.colour

amISpecialSnowFlake key g = let mrRogers = neighborNodes key g in not (List.foldl (||) False (List.map (\x -> macklemore key x g) mrRogers))


isGoodColoring (G ns es) = List.foldl (&&) True (List.map (\x -> amISpecialSnowFlake x (G ns es)) (D.keys ns))

pairs xs = let pairHelp ys = case ys of
                              [] -> []
                              [y] -> []
                              x::(z::zs) -> (x,z) :: (pairHelp (x::zs))
           in
               case xs of
                [] -> []
                [x] -> []
                x::xs' -> List.append (pairHelp xs) (pairs xs')

kanyeWest keys g = List.map (\(x,y) -> queryGraph x y g) (pairs keys)

indepSet keys g = not (List.foldl (||) False (kanyeWest keys g))

isClique keys g = List.foldl (&&) True (kanyeWest keys g)

subset set1 set2 = List.foldl (&&) True (List.map (\x -> S.member x set2) (S.toList set1))

reachables key g alreadyexplored = let lilWayne = S.fromList (neighborNodes key g)
                                       freshNodes = S.filter (\x -> not (S.member x alreadyexplored)) lilWayne in 
                                       if freshNodes == S.empty 
                                        then 
                                          S.insert key alreadyexplored 
                                        else 
                                          let 
                                            birdMan = S.toList freshNodes 
                                          in
                                            List.foldl (\x y -> S.union x y) S.empty (List.map (\x -> reachables x g (S.insert key alreadyexplored)) birdMan)


isConnected : Graph comparable -> Bool
isConnected g = case g of 
  E -> True --yay!
  G ns es -> let recordspins = D.toList <| ns
                 size = List.length recordspins
                 needleDrop = fst <| List.head <| recordspins 
             in
                 size == (List.length (S.toList (reachables needleDrop g S.empty)))

isOddCycle : Graph comparable -> Bool
isOddCycle g = let degseq = degreeseq g
                   len = List.length degseq
                   regular = List.isEmpty (List.filter (\x -> not (x ==2)) degseq) 
               in
                   regular && (isConnected g) && ((len % 2) == 1)

isK5 : Graph comparable -> Bool
isK5 g = case g of 
  E -> False
  G ns es -> let keys = D.keys ns 
                 len = List.length keys 
             in 
                 (len == 5) && (isClique keys g)

subsequences : List a -> List (List a)
subsequences xs = case xs of
  []    -> [[]]
  x::xs -> List.append (List.map (\ys -> x::ys) (subsequences xs)) (subsequences xs)

-- checks if graph is 3-regular, bipartite, and has 6 vertices
isK33 : Graph comparable -> Bool
isK33 g = case g of
  E -> False
  G ns es -> let keys = D.keys ns in 
                 if | (List.length keys) /= 6 -> False
                    | [3,3,3,3,3,3] /= (degreeseq g) -> False
                    | otherwise -> 
                            let 
                              cyclecheck = List.filter (\x -> (5 == List.length x) || (3 == List.length x)) (subsequences <| keys)
                              cyclecheck2 = List.filter (\x -> isOddCycle x) (List.map (\x -> (inducedSubgraph x g)) cyclecheck) 
                            in 
                              List.isEmpty cyclecheck2
