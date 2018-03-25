module BFS where

import qualified Data.Set as Set
import Graph

bfs :: Graph -> Int -> Graph
bfs g s = exploreNextLayer g queue discovered tree
  where
    tree = (mkGraph n [])
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
