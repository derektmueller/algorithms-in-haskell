module AdjacencyList where

import qualified Data.Set as Set
import Data.Tuple

newtype AdjacencyList = AdjacencyList [[Int]] deriving (Eq, Show)

initAdjacencyList :: Int -> AdjacencyList
initAdjacencyList n = AdjacencyList $ take n $ repeat []

mkAdjacencyList :: [(Int, Int)] -> AdjacencyList
mkAdjacencyList edges = foldr (\edge acc -> 
  insert (insert acc edge) (swap edge)) (initAdjacencyList n) edges
  where
    n = length $ Set.toList . Set.fromList $ 
      foldr (\edge acc -> [fst edge, snd edge] ++ acc) [] edges

insert :: AdjacencyList -> (Int, Int) -> AdjacencyList
insert (AdjacencyList lists) (u, v) = 
  AdjacencyList $ 
    (take u lists) ++ [(Set.toList . Set.fromList $ v : (lists !! u))] ++ (drop (u + 1) lists)

neighbors :: AdjacencyList -> Int -> [Int]
neighbors (AdjacencyList lists) u = lists !! u

vertices :: AdjacencyList -> [Int]
vertices (AdjacencyList lists) = [0..(length lists - 1)]
