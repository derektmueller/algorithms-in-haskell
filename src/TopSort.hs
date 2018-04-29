module TopSort where

import Digraph
import qualified AdjacencyList as AL
import Data.List

topSort :: Digraph -> Maybe [Int]
topSort = topSort' []

topSort' :: [Int] -> Digraph -> Maybe [Int]
topSort' deleted d@(Digraph _ al1@(AL.AdjacencyList lists)) =
  if (length deleted) == (length $ vertices d)
  then
    Just deleted
  else do
    (v, _) <- 
      find (isNonTerminalNode deleted) (zip (AL.vertices al1) lists)
    topSort' (deleted ++ [v]) (Digraph.delete d v)

isNonTerminalNode :: [Int] -> (Int, [Int]) -> Bool
isNonTerminalNode deleted (u, vs) = 
  not (u `elem` deleted) && vs == []
