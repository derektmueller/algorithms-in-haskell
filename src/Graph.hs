module Graph where

import qualified AdjacencyList as AL
import qualified Data.Set as Set
import Data.Tree
import Data.Tuple.Utils
import Data.Tuple
import Data.List.Index
import Debug.Trace

data Graph = Graph AL.AdjacencyList deriving (Eq, Show)

mkGraph :: Int -> [(Int, Int)] -> Graph
mkGraph n edges = foldr (\edge acc -> insert acc edge) (Graph (AL.initAdjacencyList n)) edges

neighbors :: Graph -> Int -> [Int]
neighbors (Graph al) u = AL.neighbors al u

vertices :: Graph -> [Int]
vertices (Graph al) = AL.vertices al

insert :: Graph -> (Int, Int) -> Graph
insert (Graph al) e = Graph $ AL.insert (AL.insert al e) (swap e)

bfs :: Graph -> Int -> Graph
bfs g s = exploreNextLayer g queue discovered tree
  where
    tree = (Graph $ AL.initAdjacencyList n)
    n = length $ vertices g
    discovered = [s]
    queue = [s]

exploreNextLayer :: Graph -> [Int] -> [Int] -> Graph -> Graph
exploreNextLayer _ [] _ tree = tree
exploreNextLayer g queue discovered tree = exploreNextLayer g queue' discovered' tree'
  where 
    (queue', discovered', tree') =
      foldr (\u (queue, discovered, graph) -> 
        let undiscoveredNeighbors = filter (not . (flip elem) discovered) (neighbors g u) in
            (undiscoveredNeighbors ++ queue, 
             Set.toList . Set.fromList $ undiscoveredNeighbors ++ discovered, 
             foldr (\v acc -> insert acc (u, v)) graph undiscoveredNeighbors)
        ) ([], discovered, tree) queue

dfs :: Graph -> Int -> Graph
dfs g s = exploreNode g s stack discovered parents tree
  where
    tree = (Graph $ AL.initAdjacencyList n)
    n = length $ vertices g
    discovered = []
    stack = [s]
    parents = take n $ repeat (-1)

exploreNode :: Graph -> Int -> [Int] -> [Int] -> [Int] -> Graph -> Graph
exploreNode _ _ [] _ _ tree = tree
exploreNode g s stack discovered parents tree =
  if not $ elem u discovered
  then
    exploreNode g s
      (neighbors g u ++ (tail stack))
      (u : discovered)
      (foldr (\v acc -> modifyAt v (\_ -> u) acc) parents (neighbors g u))
      (if u == s then tree else insert tree (u, parents !! u))
  else
    exploreNode g s (tail stack) discovered parents tree
  where
    u = head stack
