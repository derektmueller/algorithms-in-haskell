module Graph where

import qualified AdjacencyList as AL
import Data.Tuple

data Graph = Graph AL.AdjacencyList deriving (Eq, Show)

mkGraph :: Int -> [(Int, Int)] -> Graph
mkGraph n edges = 
  foldr (\edge acc -> insert acc edge) 
    (Graph (AL.initAdjacencyList n)) edges

neighbors :: Graph -> Int -> [Int]
neighbors (Graph al) u = AL.neighbors al u

vertices :: Graph -> [Int]
vertices (Graph al) = AL.vertices al

insert :: Graph -> (Int, Int) -> Graph
insert (Graph al) e = Graph $ AL.insert (AL.insert al e) (swap e)
