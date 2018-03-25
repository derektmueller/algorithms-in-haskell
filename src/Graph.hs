module Graph where

import qualified AdjacencyList as AL
import qualified Data.Set as Set
import Data.Tree
import Data.Tuple.Utils
import Data.Tuple

data Graph = Graph AL.AdjacencyList deriving (Eq, Show)

mkGraph :: [(Int, Int)] -> Graph
mkGraph [] = Graph $ AL.AdjacencyList []
mkGraph edges = Graph $ AL.mkAdjacencyList edges

neighbors :: Graph -> Int -> [Int]
neighbors (Graph al) u = AL.neighbors al u

vertices :: Graph -> [Int]
vertices (Graph al) = AL.vertices al

insert :: Graph -> (Int, Int) -> Graph
insert (Graph al) e = Graph $ AL.insert (AL.insert al e) (swap e)

bfs :: Graph -> Int -> Graph
bfs g s = bfs' g queue discovered tree
  where
    tree = (Graph $ AL.initAdjacencyList n)
    n = length $ vertices g
    discovered = [s]
    queue = [s]

bfs' :: Graph -> [Int] -> [Int] -> Graph -> Graph
bfs' _ [] _ tree = tree
bfs' g queue discovered tree = 
  bfs' g queue' discovered' tree'
  where
    (queue', discovered', tree') = exploreNextLayer
    exploreNextLayer :: ([Int], [Int], Graph)
    exploreNextLayer = foldr (\u (queue, discovered, graph) -> 
      let undiscoveredNeighbors = filter (not . (flip elem) discovered) (neighbors g u) in
          (undiscoveredNeighbors ++ queue, 
           Set.toList . Set.fromList $ undiscoveredNeighbors ++ discovered, 
           foldr (\v acc -> insert acc (u, v)) graph undiscoveredNeighbors)
      ) ([], discovered, tree) queue
      

dfs :: Graph -> Int -> Graph
dfs g s = dfs' g queue discovered tree
  where
    tree = (Graph $ AL.initAdjacencyList n)
    n = length $ vertices g
    discovered = [s]
    queue = [s]

dfs' :: Graph -> [Int] -> [Int] -> Graph -> Graph
dfs' _ [] _ tree = tree
dfs' g queue discovered tree = 
  dfs' g queue' discovered' tree'
  where
    (queue', discovered', tree') = exploreNextLayer
    exploreNextLayer :: ([Int], [Int], Graph)
    exploreNextLayer = foldr (\u (queue, discovered, graph) -> 
      let undiscoveredNeighbors = filter (not . (flip elem) discovered) (neighbors g u) in
          (undiscoveredNeighbors ++ queue, 
           Set.toList . Set.fromList $ undiscoveredNeighbors ++ discovered, 
           foldr (\v acc -> insert acc (u, v)) graph undiscoveredNeighbors)
      ) ([], discovered, tree) queue

