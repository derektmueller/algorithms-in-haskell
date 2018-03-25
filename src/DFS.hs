module DFS where

import Graph

dfs :: Graph -> Int -> Graph
dfs g s = tree'
  where
    tree = mkGraph n []
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

