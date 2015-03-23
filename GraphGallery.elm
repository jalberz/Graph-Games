module GraphGallery where

import List ((::))
import List
import Dict as D
import GraphType (..)
import GraphOps (..)

-- Helpers


pairs2 xs = case xs of
  [] -> []
  [x] -> []
  x::(y::xs') -> (x,y) :: (pairs2 (y::xs'))

-- Makes a clique on n vertices palced evenly around a circumfrence
clique : Int -> Graph comparable
clique n = let getPos = (\x -> fromPolar <| (0.85, (pi/2) + ((toFloat x)* (2.0 * pi) / (toFloat n))))
               nodeList = List.map (\z -> (z, {colour = 4, glow = False, x = fst <| getPos <| z, y = snd <| getPos <| z})) [60 .. (n+59)]
               nodes = D.fromList nodeList
               edges = List.map (\(x,y) -> {source = x, sink = y}) (pairs [60 .. (n+59)])
           in
               G nodes edges


--another version
clique' : Int -> Graph comparable
clique' n = let getPos = (\x -> fromPolar <| (0.45, (pi/2) + ((toFloat x)* (2.0 * pi) / (toFloat n))))
                nodeList = List.map (\z -> (z, {colour = 4, glow = False, x = 0.62 + (fst <| getPos <| z), y = snd <| getPos <| z})) [80 .. (n+79)]
                nodes = D.fromList nodeList
                edges = List.map (\(x,y) -> {source = x, sink = y}) (pairs [80 .. (n+79)])
            in
                G nodes edges

--another version
clique'' : Int -> Graph comparable
clique'' n = let getPos = (\x -> fromPolar <| (0.35, (pi/2) + ((toFloat x)* (2.0 * pi) / (toFloat n))))
                 nodeList = List.map (\z -> (z, {colour = 4, glow = False, x = -0.65 + (fst <| getPos <| z), y = 0.45+(snd <| getPos <| z)})) [100 .. (n+99)]
                 nodes = D.fromList nodeList
                 edges = List.map (\(x,y) -> {source = x, sink = y}) (pairs [100 .. (n+99)])
             in
                 G nodes edges

clique''' : Int -> Graph comparable
clique''' n = let getPos = (\x -> fromPolar <| (0.4, (pi/2) + ((toFloat x)* (2.0 * pi) / (toFloat n))))
                  nodeList = List.map (\z -> (z, {colour = 4, glow = False, x = -0.45+(fst <| getPos <| z), y = -0.45+(snd <| getPos <| z)})) [120 .. (n+119)]
                  nodes = D.fromList nodeList
                  edges = List.map (\(x,y) -> {source = x, sink = y}) (pairs [120 .. (n+119)])
              in
                  G nodes edges


-- Makes a cycle on n vertices placed evenly around a circumfrence
cycle : Int -> Graph comparable
cycle n = let getPos = (\x -> fromPolar <| (0.4, (pi/2) + ((toFloat x)* (2.0 * pi) / (toFloat n))))
              nodeList = List.map (\z -> (z, {colour = 5, glow = False, x = fst <| getPos <| z, y = snd <| getPos <| z})) [20 .. (n+19)]
              nodes = D.fromList nodeList
              edges = List.map (\(x,y) -> {source = x, sink = y}) ((20, n+19) :: (pairs2 [20 .. n+19]))
          in
              G nodes edges



cycle' : Int -> Graph comparable
cycle' n = let getPos = (\x -> fromPolar <| (0.8, (pi/2) + ((toFloat x)* (2.0 * pi) / (toFloat n))))
               nodeList = List.map (\z -> (z, {colour = 5, glow = False, x = fst <| getPos <| z, y = snd <| getPos <| z})) [20 .. (n+19)]
               nodes = D.fromList nodeList
               edges = List.map (\(x,y) -> {source = x, sink = y}) ((20, n+19) :: (pairs2 [20 .. n+19]))
           in
               G nodes edges


-- Makes a K_{n,m} complete bipartite graph, with the nodes in two parallel rows
completeBipartite : Int -> Int -> Graph comparable
completeBipartite n m = let nodes1 = List.map (\z -> (z, {colour = 8, glow = False, x = ((toFloat z)*(2.0 / (toFloat (n+1)))) - 1.0, y = 0.5})) [1 .. n]
                            nodes2 = List.map (\z -> (n + z, {colour = 8, glow = False, x = ((toFloat z)*(2.0 / (toFloat (m+1)))) - 1.0, y = -0.5})) [1 .. m]
                            nodes = D.fromList (List.append nodes1 nodes2)
                            fun fns ys = case fns of
                                          [] -> []
                                          x :: xs -> List.append (List.map x ys) (fun xs ys)
                            edges = List.map (\(x,y) -> {source = x, sink = y}) (fun (List.map (\x -> (\y -> (x,y))) [1 .. n]) [n+1 .. n+m])
                        in
                            G nodes edges

-- generalized Peterson Graph GP(n,k)
genPeterson : Int -> Int -> Graph comparable
genPeterson n k = let getPos1 = (\x -> fromPolar <| (0.84, (pi/2) + ((toFloat x)* (2.0 * pi) / (toFloat n))))
                      getPos2 = (\x -> fromPolar <| (0.35, (pi/2) + (((toFloat x)+1.0)* (2.0 * pi) / (toFloat n))))
                      nodes1 = List.map (\z -> (z, {colour = 1, glow = False, x = fst <| getPos1 <| z, y = snd <| getPos1 <| z})) [1 .. n]
                      nodes2 = List.map (\z -> (z, {colour = 1, glow = False, x = fst <| getPos2 <| -z, y = snd <| getPos2 <| -z})) [-(n-1) .. 0]
                      edges1 = {source = n, sink = 1} :: (List.map (\x -> {source = x, sink = (x+1)}) [1 .. n-1])
                      edges2 = List.map (\x -> {source = x, sink = -(x-1)}) [1 .. n]
                      edges3 = List.map (\x -> {source = -x, sink = -((x+k) % n)}) [0 .. n-1]
                      edges = List.append edges3 (List.append edges1 edges2)
                  in
                      if k > n // 2 
                        then
                          E 
                        else 
                          G (D.fromList (List.append nodes1 nodes2)) edges


claw : Int -> Graph comparable
claw k = let  getPos = (\x -> fromPolar <| (0.6, (pi/2) + ((toFloat x)* (2.0 * pi) / (toFloat k))))
              nodeList = List.map (\z -> (z, {colour = 2, glow = False, x = fst <| getPos <| z, y = snd <| getPos <| z})) [0 .. k-1]
              nodes = D.fromList ((k, {colour = 2, glow = False, x = 0, y= 0})::nodeList)
              edges = List.map (\x -> {source = k, sink = x}) [0 .. k-1]
         in
              G nodes edges

szekeresSnarkFifth : Int -> Float -> (Float, Float) -> Graph comparable
szekeresSnarkFifth initKey angle (x'',y'') = 
  let getPos = (\x -> let k = x-initKey in (x, fromPolar <| (0.25, angle+((toFloat k)* (2.0 * pi) / 9.0))))
      list = List.map getPos [initKey+1 .. initKey+9]
      nodeList = (initKey, {colour = 3, glow = False, x = x'', y =y''}) :: (List.map (\(z,(x',y'))-> (z, {colour = 7, glow = False, x = x'' + x', y = y''+y'})) list)
      nodes = D.fromList nodeList
      es1 = [(initKey, initKey+3), (initKey, initKey+6), (initKey, initKey+9), (initKey+1, initKey+5), (initKey+8,initKey+4)]
      es2 = pairs2 (List.append [initKey+5 .. initKey+9] [initKey+1 .. initKey+4])
      edges = List.map (\(x,y) -> {sink=x, source = y}) (List.append es1 es2)
  in
      G nodes edges

szekeresSnark = let (G ns1 es1) = szekeresSnarkFifth 0 (pi/2) (fromPolar (0.6, pi/2))
                    (G ns2 es2) = szekeresSnarkFifth 10 ((pi/2)-((2.0*pi)/(5.0))) (fromPolar (0.6, (pi/2) - ((2.0*pi)/(5.0))))
                    (G ns3 es3) = szekeresSnarkFifth 20 ((pi/2)-((4.0*pi)/(5.0))) (fromPolar (0.6, (pi/2) - ((4.0*pi)/(5.0))))
                    (G ns4 es4) = szekeresSnarkFifth 30 ((pi/2)-((6.0*pi)/(5.0))) (fromPolar (0.6, (pi/2) - ((6.0*pi)/(5.0))))
                    (G ns5 es5) = szekeresSnarkFifth 40 ((pi/2)-((8.0*pi)/(5.0))) (fromPolar (0.6, (pi/2) - ((8.0*pi)/(5.0))))
                    ns = D.union ns1 (D.union ns2 (D.union ns3 (D.union ns4 ns5)))
                    es = List.append (List.map (\(x,y) -> {source = x, sink =y}) [(4, 15), (14, 25), (24, 35), (34, 45), (44, 5), (27,42) ,(32,17), (12,47), (2, 37), (7, 22)]) (List.concat [es1, es2, es3, es4, es5]) 
                in
                    G ns es



--levelOneSceneOneGraph : Graph (Int)
levelOneSceneOneGraph = let 
                          (G ns es) = claw 8
                          nodetransform = D.fromList (List.map (\(k,n) -> (k,{colour =2, glow = False, x=(n.x)-(0.3), y =(n.y)  })) (D.toList ns))
                          ns' = D.insert 9 {colour = 2, glow = False, x = 0.8, y = 0.2992640687119283} nodetransform
                          ns'' = D.insert 10 {colour = 2, glow = False, x = 0.8, y = -0.2992640687119283} ns'
                          es' = List.append [{source = 9, sink = 10}, {source = 6, sink = 10}, {source = 6, sink = 9}] es 
                        in 
                          G ns'' es'

levelOneSceneThreeGraph = let ns = D.fromList [(5,{ colour = 4, glow = False, x = -0.9975648790363599, y = 0.07473115943931682 }),
                                               (0,{ colour = 4, glow = False, x = -0.6750000000000002, y = 0.19999999999999948 }),
                                               (2,{ colour = 4, glow = False, x = -0.20243512096364075, y = 0.12473115943931712 }),
                                               (1,{ colour = 4, glow = False, x = 0.14635166577877362, y = 0.21114082167286002 }),
                                               (3,{ colour = 4, glow = False, x = 0.3595046347941977, y = -0.1858719811121767 }),
                                               (4,{ colour = 4, glow = False, x = 0.8904953652058026, y = -0.08587198111217695 }),
                                               (6,{ colour = 4, glow = False, x = 0.9036483342212271, y = 0.3611408216728599 })]
                              es = List.map (\(x,y) -> {source = x, sink = y}) [(0,6), (2,3), (5,0), (0,2), (2,1), (1,3), (1,6), (3,4), (4,6), (1,4), (3,6), (5,3), (0,1)] 
                          in
                            G ns es                 
levelOneSceneFourGraph = let ns = D.fromList [(5,{ colour = 3, glow = False, x = 0.5, y = 0.07 }),
                                               (0,{ colour = 3, glow = False, x = -0.6750000000000002, y = -0.4299999999999948 }),
                                               (2,{ colour = 3, glow = False, x = 0.570243512096364075, y = 0.7473115943931712 }),
                                               (7,{ colour = 3, glow = False, x = -0.364075, y = 0.1247311712 }),
                                               (9,{ colour = 3, glow = False, x = -0.20243512096364075, y = -0.612473115943931712 }),
                                               (8,{ colour = 3, glow = False, x = -0.96364075, y = 0.612473115943931712 }),                                               
                                               (1,{ colour = 3, glow = False, x = 0.14635166577877362, y = 0.21114082167286002 }),
                                               (3,{ colour = 3, glow = False, x = 0.046347941977, y = -0.1858719811121767 }),
                                               (4,{ colour = 3, glow = False, x = 0.3953652058026, y = -0.858111217695 }),
                                               (6,{ colour = 3, glow = False, x = 0.993342212271, y = 0.3146728599 })]
                             es = List.map (\(x,y) -> {source = x, sink = y}) [(1,7), (1,5), (1,2), (1,3), (7,0), (7,8), (3,9), (3,4), (5,6)]
                         in
                            G ns es                 
--toolazytowritethisneatly

levelTwoSceneOneGraph = let (G ns1 es1) = cycle 8
                            (G ns3 es4) = claw 5
                            ns1' = D.fromList (List.map (\(k,n) -> (k,{colour =4, glow = False, x=(n.x)-(0.55), y =(n.y)+0.45})) (D.toList ns1))
                            ns3' = D.fromList (List.map (\(k,n) -> (k,{colour =4, glow = False, x=(n.x)+(0.35), y =(n.y)-0.25})) (D.toList ns3))
                            nodes = (D.union ns1' ns3') 
                        in 
                            G nodes (List.concat [es1, es4, [{source =1, sink =0}, {source = 25, sink = 21}, {source =1,sink =20}, {source =22, sink =0}]])

levelTwoSceneTwoGraph = let (G ns1 es1) = cycle 5
                            --(G ns2 es2) = clique 5
                            (G ns3 es4) = claw 5
                            cliqueNodes = D.fromList [(60,{ colour = 4, glow = False, x = 7.080036899695002e-15, y = 0.85 }),
                                                      (61,{ colour = 4, glow = False, x = 0.8083980388508802, y = 0.20759866713122377 }),
                                                      (64,{ colour = 4, glow = False, x = 0.7083980388508827, y = -0.41233555478130174 }),
                                                      (63,{ colour = 4, glow = False, x = -0.9246174644486016, y = -0.3625986671312236 }),
                                                      (62,{ colour = 4, glow = False, x = -0.1496174644486077, y = 0.18733555478129915 })]
                            cliqueEdges = List.map (\(x,y) -> {source = x, sink = y}) (List.append [(1,2), (2,3), (1,3), (60,20), (22,63), (21,63), (60,0)] (pairs [60 .. 64]))                        
                            --ns2' = D.fromList (List.map (\(k,n) -> if (k%2 == 1) then  (k,{colour =4, glow = False, x=-(n.x), y =0.6*(n.y)+0.05}) else (k,n)) (D.toList ns2))
                            ns1' = D.fromList (List.map (\(k,n) -> if (k == 23) || (k == 24) then (k,{colour =4, glow = False, x=(n.x)-(0.87), y =(n.y)+0.45}) else (k,{colour =2, glow = False, x=(n.x)-(0.55), y =(n.y)+0.45})) (D.toList ns1))
                            ns3' = D.fromList (List.map (\(k,n) -> if (k == 5) || (k == 1) then (k,{colour =4, glow = False, x=(n.x)+(0.58), y =(n.y)-0.61}) else (k,{colour =2, glow = False, x=(n.x)+(0.35), y =(n.y)-0.25})) (D.toList ns3))
                            nodes = D.union cliqueNodes (D.union ns1' ns3') in G nodes (List.concat [es1, es4, cliqueEdges])

levelTwoSceneThreeGraph = let (G ns1 es1) = clique' 6
                              (G ns2 es2) = clique'' 5
                              (G ns3' es3) = clique''' 4 
                              ns3 = D.insert 83 { colour = 4, glow = False, x = 1.0097114317029976, y = 0.0249999999999998 } (D.insert 85 { colour = 4, glow = False, x = 0.23028856829700328, y = 0.02500000000000114 } ns3')
                              es4 = List.map (\(x,y) -> {source=x, sink=y}) (pairs [84,85,103,80,120]) 
                          in 
                              G (D.union ns3 (D.union ns1 ns2)) (List.concat [es1,es2,es3,es4])


levelThreeSceneOneGraph = peterson
levelThreeSceneTwoGraph = let (G ns es) = claw 7
                              es' = List.map (\(x,y) -> {source=x, sink=y}) [(0,1), (1,2), (2,3), (3,4), (4,5), (5,6), (6,0)] 
                          in 
                            G ns (List.append es es') 

levelThreeSceneThreeGraph = levelTwoSceneThreeGraph

levelThreeSceneFourGraph = let (G ns es) = cycle' 16
                               es' = List.map (\(x,y) -> {source=x, sink=y}) [(21, 30),(23, 28),(22, 27),(20, 29),(32, 35), (24, 35)] in G ns (List.append es es')                                


levelOneSceneOneWinCondition g = [6] == (selectedNodes g)    

--graph->bool
levelOneSceneTwoWinCondition g = [8] == (selectedNodes g)


levelOneSceneThreeWinCondition g = [3,2,2,2,2,2,1] == (degreeseq g)


levelOneSceneFourWinCondition g = [6,1,1,1,1,1,1] == (degreeseq g)


levelTwoSceneOneWinCondition g = (isClique (selectedNodes g) g) && ((List.length <| selectedNodes g) == 3)


levelTwoSceneTwoWinCondition g = isK5 (inducedSubgraph (selectedNodes g) g)


levelTwoSceneThreeWinCondition g = let (G ns es) = g
                                       list = colorsinGraph g
                                       len = List.length list
                                       foo color = isClique (List.map (\(k,n) -> k) (D.toList (D.filter (\key node -> node.colour == color) ns))) g 
                                   in
                                       (len == 4) && (List.foldl (&&) True (List.map foo list))

levelThreeSceneOneWinCondition g = isGoodColoring g


levelThreeSceneTwoWinCondition g = isGoodColoring g


levelThreeSceneThreeWinCondition g = (isClique (selectedNodes g) g) && ((List.length (selectedNodes g)) == 6)


levelThreeSceneFourWinCondition g = ((List.length (colorsinGraph g)) == 2) && (isGoodColoring g)

--graphs
------------
levelFourSceneThreeGraph = let (G ns es) = cycle' 6
                               fuck = D.insert 0 {glow=False, colour=5,x=0.0, y=0.0} ns
                               es' = List.map (\(x,y) -> {sink=x, source = y}) [(0, 21),(0, 24),(21,25),(20,24),(20,22),(25,23)] 
                           in
                              G fuck (List.append es' es)


levelFourSceneFourGraph = let (G ns es) = completeBipartite 4 3
                              ns' = D.insert 8 {glow=False, colour=8, x=0, y=0.85} ns 
                              es' = List.map (\(x,y) -> {sink=x, source = y}) [(5,6),(6,7),(3,2),(2,8),(3,8)]
                          in 
                              G ns' (List.append es' es)

--checks
--------------------
imgonnaslitmywrists1 g = isK5 g

imgonnaslitmywrists2 g = isK33 g

k_5 = clique 5

k_3_3 = completeBipartite 3 3

peterson = genPeterson 5 2