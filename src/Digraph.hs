module Digraph where

import qualified AdjacencyList as AL
import Data.Tuple

data Digraph = 
  Digraph AL.AdjacencyList AL.AdjacencyList deriving (Eq, Show)

mkDigraph :: Int -> [(Int, Int)] -> Digraph
mkDigraph n edges = 
  foldr (\edge acc -> insert acc edge) 
    (Digraph (AL.initAdjacencyList n) (AL.initAdjacencyList n)) edges

insert :: Digraph -> (Int, Int) -> Digraph
insert (Digraph al0 al1) e = 
  Digraph (AL.insert al0 e) (AL.insert al1 (swap e))

vertices :: Digraph -> [Int]
vertices (Digraph al0 _) = AL.vertices al0

delete :: Digraph -> Int -> Digraph
delete d@(Digraph al0 al1) u =
  foldr (\e acc -> deleteEdge acc e) d toDelete
  where
    toDelete = 
      [(u', v') | 
        (u', v') <- (AL.edges al0) ++ (AL.edges al1), u' == u]

deleteEdge :: Digraph -> (Int, Int) -> Digraph
deleteEdge d@(Digraph al0 al1) e = 
  Digraph (AL.delete al0 e) (AL.delete al1 (swap e))
