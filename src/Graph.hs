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
    u = head queue
    undiscoveredNeighbors = filter (not . (flip elem) discovered) (neighbors g u) 
    queue' = (tail queue) ++ undiscoveredNeighbors
    discovered' = Set.toList . Set.fromList $ undiscoveredNeighbors ++ discovered
    tree' = foldr (\v acc -> insert acc (u, v)) tree undiscoveredNeighbors

dfs :: Graph -> Int -> Graph
dfs g s = tree'
  where
    tree = (Graph $ AL.initAdjacencyList n)
    n = length $ vertices g
    discovered = []
    (_, tree') = exploreNode g s discovered tree

exploreNode :: Graph -> Int -> [Int] -> Graph -> ([Int], Graph)
exploreNode g s discovered tree = foldr (\u (discovered', tree') -> 
    if (not $ elem u discovered')
    then 
      exploreNode g u discovered' (insert tree' (s, u))
    else 
      (discovered', tree')
  ) (s : discovered, tree) (neighbors g s)
