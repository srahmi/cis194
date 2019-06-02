module NetworkDistance
    (
    ) where

import qualified Data.Map as M
import Control.Arrow (second)

{--
This problem was asked by Twitter.

A network consists of nodes labeled 0 to N.
You are given a list of edges (a, b, t), describing the time t it takes
for a message to be sent from node a to node b. Whenever a node receives
a message, it immediately passes the message on to a neighboring node, if possible.

Assuming all nodes are connected, determine how long it will take for every node
to receive a message that begins at node 0.

For example, given N = 5, and the following edges:

edgesWeight = [
    ("0", "1", 5),
    ("0", "2", 3),
    ("0", "5", 4),
    ("1", "3", 8),
    ("2", "3", 1),
    ("3", "5", 10),
    ("3", "4", 5)
]
You should return 9, because propagating the message from 0 -> 2 -> 3 -> 4 will take that much time.

[
("0",[Edge "5" (Just 4), Edge "2" (Just 3), Edge "1" (Just 5)]),
("1",[Edge "3" (Just 8)]),
("2",[Edge "3" (Just 1)]),
("3",[Edge "4" (Just 5), Edge "5" (Just 10)])
]
--}
data Edge = Edge Node (Maybe Int) deriving Show
type Node = String
type Vertex = String
type Graph = [(Node, [Edge])]

buildGraph    :: [(Node, Node, Int)] -> Graph
buildGraph  = graph
              .M.fromListWith (++)
              . map(\(s, d, w) -> (s, [(d, w)]))

graph :: M.Map Node [(Node, Int)] -> Graph
graph = map(second toEdges) . M.toList

toEdges :: [(Node, Int)] -> [Edge]
toEdges = map (\(e, w) -> Edge e (Just w))

vertices  :: [(Node, Node, Int)] -> [Vertex]
vertices  =  map(\(x, y, _) -> x ++ y)
