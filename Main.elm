module MainTest
    (main) 
        where


import List ((::))
import List
import Signal (Signal, (<~), (~))
import Signal
import Graphics.Element as E
import GraphType (..)
import GraphOps (..)
import GraphGallery (..)
import GraphView (..)
import Dict as D
import Time (Time, every, second, millisecond)
import Text as T
import Window
import Mouse as M
import Keyboard


--Some basic test nodes & edges
node1 = (1, {colour = 3, glow = False, x = 0.0, y = 0.30})
node2 = (2, {colour = 2, glow = False, x = 0.2, y = 0.1})
node3 = (3, {colour = 6, glow = False, x = -0.2, y = 0.0})
edge1 = {source = 1, sink = 2}
edge2 = {source = 3, sink = 1}
nodes = D.fromList [node1, node2, node3]


--A sample initstate
initState = { graph = genPeterson 16 3, biggie = 11, level = 0, runState = Steady }

main : Signal E.Element
main = 
    let
        v = viewGraph <~ Window.dimensions
                       ~ Signal.foldp upstate initState
                         (mergeWithTicker (100 * millisecond) (Signal.subscribe nodeChannel) (Signal.subscribe edgeChannel) (Keyboard.arrows))
    in
        v

