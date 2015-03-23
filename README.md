# Graph-Games

Above is a simple game for learning some basics of Graph Theory, made by Ridwan
Syed and I.

Here are some additional notes:

  We created a multi-level game that teaches a user basic graph theory by allowing
them to directly manipulate the nodes and edges of a graph. On the technical
side of things, our project consisted of several primary segments: firstly, is
the underlying Graph data structure and and operations. We chose to design the
structure as a construction of a dictionary of nodes and a list of edges --
where an 'edge' is a tuple of keys corresponding to two nodes. Operation-wise,
we implemented standard functions such as insertion, deletion, get, connect as 
well as additional more specific functions such as isClique and isOddCycle.
	
Secondly, we built up infrastructure to visualize the graphs and have them
be interactive. This involved utilizing the elm-html library to create divs that
would describe the visual attributes of the nodes and edges, at which point we 
added functionality from the Event sub-library of elm-html. This allowed nodes
and edges to be clickable and hoverable, and send messages that could update the
state of the graph.
	
Thirdly, we created a gallery of functions which can be used to construct 
more complex graphs -- n-sized cliques, peterson graphs, the szekeres snark, as
well as the specific graphs for the levels that we have implemented.

Fourthly, the interactions that we have implemented for the user include the
highlighting of nodes and edges, the deletion of nodes and edges, the movement
of nodes, and the collapsing of edges.
	
Finally, we designed a levelling system that involves 4 specific topics of
basic graph theory (degrees, cliques, colorings, and forbidden subgraphs) with
each containing several levels of varying difficulties. We also provide with
each level a short prompt with some hints, as well as the capability of
resetting the level. We also provide a couple sandboxes with larger graphs in
them so as to allow for more freeform graph interactions.

