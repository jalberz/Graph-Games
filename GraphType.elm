module GraphType where

import List ((::))
import List
import Dict as D

--Data Types for Graphs--------------------------------------------------------

type alias Colour = Int
--different ints will lead to different colors longterm TODO: find better way to describe this

--For Highlighting (may change to ternary or quarternary)
type alias Glow = Bool


{-The node record, currently contains fields for Colour (for vertex coloring shenanigans)
and glow (whether a node is highlighted or not, for the purposes of FRP & gaming)
longterm TODO: add more/broaden fields for portability to other applications-}
type alias Node = { colour : Colour, glow : Glow, x : Float, y : Float }
-- I'm requiring explicit position info, since we're drawing graphs in R^2

{-The edge record, which is addressable by two keys, each of which corresponds to one
of the two nodes they connect -}
type alias Edge comparable = { source : comparable, sink : comparable }
--Ridwan: renamed to sink and source in case we do digraphs

--Was originally considering edge as a tuple: type Edge comparable = (comparable , comparable)



{-The fundamental graph datatype, either empty or non-empty. Non-empty contains a
constructor G as well as a dictionary of (keys,nodes) and a list of edge records.
-}
type Graph comparable = E | G (D.Dict comparable Node) (List (Edge comparable))
