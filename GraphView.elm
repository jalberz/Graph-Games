module GraphView where

import GraphType (..)
import GraphOps (..)
import GraphGallery (..)
import List ((::))
import List
import Dict as D
import Graphics.Collage as C
import Graphics.Element as E
import Signal (Signal, (<~), (~))
import Signal
import Html as H
import Html.Attributes as A
import Html.Events as V
import Time (Time, every, second, millisecond)
import Mouse as M
import Keyboard
import Graphics.Input (..)
import Color
import Text as T
import Easing


type alias State comparable = { graph : Graph comparable, biggie : comparable, level : Int, runState : RunState comparable }



type RunState comparable    = Steady
                              | Activating (comparable, Node) Time Time Time
                              | NodeDelete (comparable, Node) Time Time Time
                              | Moving (comparable, Node) 
                              | ColorChanging (comparable, Node) Time Time Time
                              | EdgeCollapsing comparable comparable Time Time Time
                              | EdgeDelete (comparable, Node) (comparable, Node) Time Time Time
                              | Leveling (Int) Time Time Time
                              | Collapsing (comparable, Node) (comparable, Node)



type TickOr msg  = Tick | M msg
type alias Event comparable = (Time, { x : Int, y : Int }, TickOr (Msg comparable))


type Msg comparable = NodeClick (comparable, Node) 
                    | NodeDoubleClick (comparable, Node)
                    | EdgeClick (comparable, Node) (comparable, Node) 
                    | EdgeDoubleClick (comparable, Node) (comparable, Node)
                    | NodeHover (comparable, Node)
                    | Move (Int, Int)
                    | Level (Int)
                    | Noop


--The Upstate Function------------------------------------------------------------------------------------------------------


{-
Allows for possible future creation of different upstate functions
-}
upstate : Event comparable -> State comparable -> State comparable
upstate (now, keys, tm) state = case state.level of
  1 -> upstateDefault (now, keys, tm) state
  _ -> upstateDefault (now, keys, tm) state


upstateDefault : Event comparable -> State comparable -> State comparable
upstateDefault (now,keys,tm) state = 
  if (keys.x /= 0 || keys.y /= 0)
    then keyMove keys state
    else
      case (tm, state.runState) of
          (M Noop, _)                                    -> state
          (Tick, Steady)                                 -> state
          (M (NodeDoubleClick (k,n)), Steady)            -> { state | runState <- NodeDelete (k,n) now now (now + delay) }
          (Tick, NodeDelete (k,n) _ start end)           -> if
            | now > end -> removeNode state (k,n)
            | otherwise -> { state | runState <- NodeDelete (k,n) now start end }
          (M (NodeHover (k,n)), Steady)                  -> { state | runState <- Activating (k,n) now now (now + delay) }
          (Tick, Activating (k,n) _ start end)           -> if
            | now > end -> activateNode state (k,n)
            | otherwise -> { state | runState <- Activating (k,n) now start end }
          (M (NodeClick (k,n)), Steady)                  -> { state | runState <- ColorChanging (k,n) now now (now + delay)}
          (Tick, ColorChanging (k,n) _ start end)        -> if
            | now > end -> changeNodeColor state (k,n)
            | otherwise -> { state | runState <- ColorChanging (k,n) now start end }
          (M (EdgeDoubleClick (k,n) (j,m)), Steady)            -> { state | runState <- EdgeCollapsing k j now now (now + delay) }
          (Tick, EdgeCollapsing k j cur start end)       -> edgeContract k j state cur now start end 
          (M (EdgeClick (k,n) (j,m)), Steady)      -> { state | runState <- EdgeDelete (k,n) (j,m) now now (now + delay) }
          (Tick, EdgeDelete (k,n) (j,m) _ start end)     -> if
            | now > end -> deleteEdge (k,n) (j,m) state
            | otherwise -> { state | runState <- EdgeDelete (k,n) (j,m) now start end }
          (M (Level i), Steady)                          -> { state | runState <- Leveling (i) now now (now + delay)}
          (Tick, Leveling i _ start end)                 -> if
            | now > end -> upLevel i state
            | otherwise -> { state | runState <- Leveling i now start end }
          (_,_)                                          -> { state | runState <- Steady }




--a simple time delay
delay = 0.5 * second



--will move level up depending on what you want
upLevel : Int -> State comparable -> State comparable
upLevel i state = case state.graph of
  E -> state
  G ns es -> { state | level <- i, graph <- (determineInit i state), runState <- Steady }


--Contract an edge with easing
edgeContract : comparable -> comparable -> State comparable -> Time -> Time -> Time -> Time -> State comparable
edgeContract k k' state cur now start to = case state.graph of
  E -> state
  G ns es -> let node1 = justGet k state.graph
                 node2 = justGet k' state.graph
                 midPoint = { x = (0.5 * ((node1.x)+(node2.x))), y = (0.5 * ((node1.y)+(node2.y)))}
                 newnode = {colour = node1.colour, glow = node1.glow, x=(0.5 * ((node1.x)+(node2.x))), y=(0.5 * ((node1.y)+(node2.y)))} 
                 neighborsSet = D.remove k' (D.remove k (D.fromList (List.map (\x -> (x, True)) (List.append (neighborNodes k state.graph) (neighborNodes k' state.graph)))))
                 newEdges = List.map (\z -> {source = k, sink = (fst z)}) (D.toList neighborsSet)
                 oldEdges = List.filter (\z' -> not ((incident k z')||(incident k' z'))) es
                 nodes = D.insert k newnode (D.remove k' ns) 
              in
              let 
                firstpoint = Easing.ease Easing.easeInOutQuad Easing.point2d {x = node1.x, y = node1.y} midPoint cur to
                secondpoint = Easing.ease Easing.easeInOutQuad Easing.point2d {x = node2.x, y = node2.y} midPoint cur to
              in
              if (now > to)
                then
                  { state | graph <- (G nodes (List.append newEdges oldEdges)), runState <- Steady }
                else
                  let
                    newnodes = D.update k (\v -> posChange v firstpoint) ns
                    newnodes2 = D.update k' (\w -> posChange w secondpoint) newnodes
                  in
                    { state | graph <- (G newnodes es), runState <- EdgeCollapsing k k' now start to }


--helper function for edgeContract
posChange : Maybe Node -> { x : Float, y : Float } -> Maybe Node
posChange m rec = case m of
  Nothing -> Nothing
  Just n  -> Just { n | x <- rec.x, y <- rec.y }



--move dependingon key control
keyMove : { x : Int, y : Int } -> State comparable -> State comparable
keyMove keys state = case state.graph of 
  E -> state
  G ns es -> let
                filtered = D.filter (isGlowing) ns
                moved = D.map (\k n -> relocate k n keys) filtered
             in
                { state | graph <- (G (D.union moved ns) es) }

--check if a node is glowing
isGlowing : comparable -> Node -> Bool
isGlowing k n = n.glow


--change location of a node
relocate : comparable -> Node -> { x : Int, y : Int } -> Node
relocate k n keys = 
  let
    a = n.x
    b = n.y
    c = (toFloat keys.x)/40
    d = (toFloat keys.y)/40
  in
    { n | x <- (a + c), y <- (b + d) }

--delete a node
removeNode : State comparable -> (comparable, Node) -> State comparable
removeNode state (k,n) = case state.graph of
  G ns es -> { state | graph <- deleteNode k state.graph, runState <- Steady }


--change glow upon selection of a node
activateNode : State comparable -> (comparable, Node) -> State comparable
activateNode state (k,n) = case state.graph of
  G ns es -> let
                newnodes = D.update k (turnOn) ns
             in 
                { state | graph <- G newnodes es, runState <- Steady }

--activateNode helper
turnOn : Maybe Node -> Maybe Node
turnOn m = case m of
  Nothing -> Nothing
  Just n  -> case n.glow of
                False -> Just { n | glow <- True }
                True  -> Just { n | glow <- False }

--change node color upon selection of a node
changeNodeColor : State comparable -> (comparable, Node) -> State comparable
changeNodeColor state (k,n) = case state.graph of
  G ns es -> let
                newnodes = D.update k (colourChange) ns 
             in
                { state | graph <- G newnodes es, runState <- Steady }


--change node color helper
colourChange : Maybe Node -> Maybe Node
colourChange m = case m of
  Nothing -> Nothing
  Just n  ->  let
                  c = n.colour
              in
                  Just { n | colour <- c + 1 }



deleteEdge : (comparable, Node) -> (comparable, Node) -> State comparable -> State comparable
deleteEdge (k,n) (j,m) state = case state.graph of
  G ns es -> { state | graph <- removeEdge {source = k, sink = j} (removeEdge {source = k, sink = j} state.graph), runState <- Steady }

strcolors n = case (n % 8) of
                0 -> "black"
                1 -> "red"
                2 -> "lightblue"
                3 -> "lightgreen"
                4 -> "pink"
                5 -> "lightyellow"
                6 -> "orange"
                7 -> "blue"



nodeHtml : (comparable, Node) -> Float -> ((Float,Float), H.Html)
nodeHtml (k,n) dim = 
  ((n.x,n.y),
  H.div [ 
          V.onClick (Signal.send nodeChannel (NodeClick (k,n)))
        , V.onDoubleClick (Signal.send nodeChannel (NodeDoubleClick (k,n)))
        , V.onMouseLeave (Signal.send nodeChannel (NodeHover (k,n)))
        , A.style [("backgroundColor", strcolors n.colour)
                  ,("height", (toString (dim/40)) ++ "px")
                  ,("width", "100%") 
                  ,("borderRadius", (toString (dim/40)) ++ ("px"))
                  ,("border", case n.glow of {False -> "2px solid black"; True -> "5px solid green"})
                  ]
        ]
        []
  )


edgeHtml : (comparable, Node) -> (comparable, Node) -> Float -> (((Float, Float), Float, Float), H.Html)
edgeHtml (k1, source) (k2, sink) dim =
  ((edgeMidPoint dim source sink, edgeAngle dim source sink, edgeLength dim source sink),
  H.div [ 
          V.onClick (Signal.send edgeChannel (EdgeClick (k1, source) (k2, sink)))
        , V.onDoubleClick (Signal.send edgeChannel (EdgeDoubleClick (k1, source) (k2, sink)))
        , A.style [("backgroundColor", case (source.glow, sink.glow) of {(True,True) -> "blue"; (_,_) -> "darkgrey"})
                  ,("height", "4px")--("borderRadius", (toString (dim/20)) ++ ("px"))
                  ,("width", (toString (edgeLength dim source sink)) ++ "px") --("border", "5 px solid black")
                  --,("borderRadius", (toString (dim/40)) ++ ("px"))
                  --,("border", case n.glow of {False -> "2px solid black"; True -> "5px solid yellow"})
                  ]
        ]
        []
  )

makeEdge dim source sink = C.traced C.defaultLine <| C.segment (dim*source.x, dim*source.y) (dim*sink.x, dim*sink.y)

edgeInfo dim source sink = let x1 = dim * source.x
                               x2 = dim * sink.x
                               y1 = dim * source.y
                               y2 = dim * sink.y in (x1, x2, y1, y2)

edgeLength dim source sink = let (x1, x2, y1, y2) = edgeInfo dim source sink in sqrt (((y2-y1)^2)+((x2-x1)^2))
edgeMidPoint dim source sink = let (x1, x2, y1, y2) = edgeInfo dim source sink in (((y2+y1)/2),((x2+x1)/2))
edgeAngle dim source sink = let (x1, x2, y1, y2) = edgeInfo dim source sink in atan2 (y2-y1) (x2-x1)

--basics for button
myButton enabled dim evt s =
  let
    w = wBtn*(dim)
    h = hBtn*(dim)
  in
  let mkBtn c =
    C.collage (floor w) (floor h) [
        C.filled c (C.rect w h)
      , C.outlined lineStyle (C.rect w h)
      , strStyle3 s |> C.toForm
    ]
  in
  let (x,y,z) =
    if | enabled   -> (Color.gray, Color.lightGrey, Color.lightOrange)
       | otherwise -> (Color.lightYellow, Color.lightRed, Color.red)
  in
    customButton evt (mkBtn x) (mkBtn y) (mkBtn z)

--basics for button
myButton2 enabled dim evt s =
  let
    w = wBtn*(dim)
    h = hBtn*(dim)
  in
  let mkBtn c =
    C.collage (floor w) (floor h) [
        C.filled c (C.rect w h)
      , C.outlined lineStyle (C.rect w h)
      , strStyle3 s |> C.toForm
    ]
  in
  let (x,y,z) =
    if | enabled   -> (Color.lightGreen, Color.lightGrey, Color.lightYellow)
       | otherwise -> (Color.lightRed, Color.lightYellow, Color.red)
  in
    customButton evt (mkBtn x) (mkBtn y) (mkBtn z)

--control button characteristics based on state
maybeButton b dim evt s = if
  | b         -> myButton True dim evt s
  | otherwise -> myButton False dim noop s

maybeButton2 b dim evt s = if
  | b         -> myButton2 True dim evt s
  | otherwise -> myButton2 False dim noop s

--Button mechanics------------------------------------------------------------------------------------------------

--various String styles
strStyle : String -> E.Element
strStyle = T.fromString >> T.height 20 >> T.centered


strStyle2 : String -> E.Element
strStyle2 = T.fromString
             >> T.height 14
             >> T.typeface [ "courier new", "courier", "sans-serif", "sans serif", "gothic", "san serif", "helvetica"]
             >>T.leftAligned


strStyle3 : String -> E.Element
strStyle3 = T.fromString
             >> T.height 20
             >> T.typeface [ "courier new", "courier", "sans-serif", "sans serif", "gothic", "san serif", "helvetica"]
             >> T.centered

strStyle4 : String -> E.Element
strStyle4 = T.fromString
             >> T.height 22
             >> T.bold
             >> T.typeface [ "courier new", "courier", "sans-serif", "sans serif", "gothic", "san serif", "helvetica"]
             >> T.centered

lineStyle =
  let ls = C.defaultLine in
    { ls | color <- Color.darkCharcoal,
           width <- 10 }


--Further FRP mechanics--------------------------------------------------------------------------------------------------------------

{-a channel to broadcast node messages on -}
nodeChannel : Signal.Channel (Msg comparable)
nodeChannel = Signal.channel Noop


{-A channel to broadcast edge messages on -}
edgeChannel : Signal.Channel (Msg comparable)
edgeChannel = Signal.channel Noop

--message to level up the game to level i
levelTo i = Signal.send nodeChannel (Level i)

noop = Signal.send nodeChannel Noop


--merging function for time and mouse positions
mergeWithTicker : Time -> Signal msg -> Signal msg -> Signal { x : Int, y : Int } -> Signal (Time, { x : Int, y : Int }, TickOr msg)
mergeWithTicker t sig1 sig2 sig3 =
  let time = every t in
  Signal.mergeMany
    [
      ((\t k  -> (t, k, Tick)) <~ time ~ sig3)
    , ((\t k m -> (t, k, M m))  <~ Signal.sampleOn sig1 time ~ sig3 ~ sig1)
    , ((\t k m -> (t, k, M m))  <~ Signal.sampleOn sig2 time ~ sig3 ~ sig2)
    ]

wBtn = 0.600
hBtn = 0.1
vSep = E.spacer 1 10

--depending on level, change arppropriate prompt
levelSwitch : State comparable -> String
levelSwitch state = case state.level of
  0 -> "Welcome to Graph Life 3
(confirmed)!
Commands:
  highlight node - mouse over node
  dehighlight node - mouse over again
  cycle node color - single click node
  delete node - double click node
  collapse edge - single click edge
  delete edge - double click edge
  move highlighted nodes - arrow keys
  Make a mistake? Just hit Reset Level! 
On the right you will find a number
of topics on Graph Theory. Each
topic is composed of several levels
of increasing difficulty.
Click a topic to begin!"
  1 -> "Your first lesson is the
concept of degree--or the number of
edges a particular node has.
Select a Node with 
Degree 3. 'Move Next Level'
will turn green when you have
the correct answer.
HINT: You can select a node by 
passing your cursor over it"
  2 -> "Now, select the Node with the
the maximal (highest) degree"
  3 -> "Good work! Your next task will be a 
little more difficult:
  Change the following Degree sequence:\n\t" ++
  (toString <| degreeseq state.graph) ++
  "to [3,2,2,2,2,2,1]\nHINT:Delete edges by double-clicking\nthem
HINT 2: Go back to the summer of '96 
and Freak Nasty's classic 'I put my 
hand upon your hip, when I dip, you 
dip, we dip'"
  4 -> "Similarly, change the
following degree sequence:\n\t" ++
  (toString <| degreeseq state.graph) ++
  "to \n[6,1,1,1,1,1,1].
HINT: You may have to use
operations besides delete"
  5 -> "Now let's move on to something
new, Cliques. A Clique is defined as
a subset of vertices where any two
unique vertices are adjacent.
Select the nodes that make up
the clique in this graph.
HINT: there is exactly 1 clique
here."
  6 -> "Next, select the nodes of
the 5-clique that is hidden
within this Graph"
  7 -> "Now color the maximal
cliques in this graph specific
colors. You can color nodes by
single clicking on them.
Repeated single clicks will
cycle through available colors.
HINT: The maximal number of
cliques in this graph is 4."
  8 -> "Time to learn about colorings!
First things first, a colored
Graph is one where no two
adjacent nodes have the same 
coloring. Your task?
Correctly color this graph!"
  9 -> "Color this one too"
  10 -> "Give a proof as to why
this Graph needs atleast six
colors...
HINT: If the squad wants to wear
different shirts, how many shirts
do you need?
HINT: Try highlighting the special
set of nodes"
  11 -> "Continue Coloring!"
  12 -> "Use edge contractions
to produce a K5 minor."
  13 -> "Use edge contractions
and deletionsto produce a K33 minor."
  otherwise -> "Sandbox mode!
Commands:
  highlight node - mouse over node
  dehighlight node - mouse over again
  cycle node color - single click node
  delete node - double click node
  collapse edge - single click edge
  delete edge - double click edge
  move highlighted nodes - arrow keys
  Make a mistake? Just hit Reset Level!"


--Change initial state of the graph to a new one upon a tick of the level
determineInit : Int -> State comparable -> Graph comparable
determineInit i state = case i of
  1 -> levelOneSceneOneGraph
  2 -> levelOneSceneOneGraph
  3 -> levelOneSceneThreeGraph
  4 -> levelOneSceneFourGraph
  5 -> levelTwoSceneOneGraph
  6 -> levelTwoSceneTwoGraph
  7 -> levelTwoSceneThreeGraph
  8 -> levelThreeSceneOneGraph
  9 -> levelThreeSceneTwoGraph
  10 -> levelThreeSceneThreeGraph
  11 -> levelThreeSceneFourGraph
  12 -> levelFourSceneThreeGraph
  13 -> levelFourSceneFourGraph
  20 -> clique 16
  21 -> szekeresSnark
  otherwise -> state.graph


--determines which level and checks win condition
checkWinCondition : State comparable -> Bool
checkWinCondition state = case state.level of
  0 -> True
  1 -> levelOneSceneOneWinCondition state.graph
  2 -> levelOneSceneTwoWinCondition state.graph
  3 -> levelOneSceneThreeWinCondition state.graph
  4 -> levelOneSceneFourWinCondition state.graph
  5 -> levelTwoSceneOneWinCondition state.graph
  6 -> levelTwoSceneTwoWinCondition state.graph
  7 -> levelTwoSceneThreeWinCondition state.graph
  8 -> levelThreeSceneOneWinCondition state.graph
  9 -> levelThreeSceneTwoWinCondition state.graph
  10 -> levelThreeSceneThreeWinCondition state.graph
  11 -> levelThreeSceneFourWinCondition state.graph
  12 -> imgonnaslitmywrists1 state.graph
  13 -> imgonnaslitmywrists2 state.graph
  20 -> True
  otherwise -> False


--VIEW FUNCTION----------------------------------------------------------------------------------

viewGraph : (Int, Int) -> State comparable -> E.Element
viewGraph (w,h) state = case state.graph of
  E       -> E.empty
  G ns es -> let
                dim = (toFloat (min w h)) / 2
             in
             let console =
                E.opacity 0.75 <| E.flow E.down
                <| List.intersperse vSep
                     [ E.image (floor <| dim*0.664) (floor <| dim*0.374) "http://screenshots.en.sftcdn.net/blog/en/2013/10/HL3HEADER-664x374.png"
                     , strStyle4 "Graph Life 3 (confirmed)"
                     , maybeButton (state.runState == Steady) dim (levelTo 1) "Degrees"
                     , maybeButton (state.runState == Steady) dim (levelTo 5) "Cliques"
                     , maybeButton (state.runState == Steady) dim (levelTo 8) "Colorings"
                     , maybeButton (state.runState == Steady) dim (levelTo 12) "Forbidden Subgraphs"
                     , maybeButton (state.runState == Steady) dim (levelTo 20) "Sandbox Mode"
                     , maybeButton2 (state.runState == Steady && (checkWinCondition state)) dim (levelTo (state.level + 1)) "Move to Next Level"
                     , maybeButton2 (state.runState == Steady) dim (levelTo state.level) "Reset Level"
                     , strStyle2 <| levelSwitch state
                     ]
             in
             let 
                 endpoints = (\x -> ((x.source, justGet x.source state.graph), (x.sink, justGet x.sink state.graph)))
                 endpoints2 = List.map (\z -> endpoints z) es
                 edges1 = List.map (\(x,y) -> edgeHtml x y dim) endpoints2
                 edges = List.map (\(((x,y), theta, l), div) -> C.rotate theta <| C.move (y,x) <| C.toForm <| (H.toElement (floor l) 4 div)) edges1
                 nodes1 = D.toList <| ns
                 nodes2 = List.map (\x -> nodeHtml x dim) nodes1
                 intdim = floor (dim/40) 
                 nodes3 = List.map (\x -> (fst x, H.toElement intdim intdim <| snd x)) nodes2
                 nodes4 = List.map (\x -> C.move (dim*(fst <| fst x), dim*(snd <| fst x)) <| C.toForm <| snd x) nodes3
             in
                E.color Color.lightGrey <| E.container (w) (h) E.middle (E.flow E.right <| [(C.collage (w-300) (h-20) (List.append edges nodes4)), console])
