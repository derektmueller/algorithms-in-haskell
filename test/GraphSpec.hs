module GraphSpec where

import Test.QuickCheck
import Test.Hspec
import Graph
import AdjacencyList
import Data.Tree

describe_graph = 
  describe "Graph" $ do
    describe "mkGraph" $ do
      describe "when the edges specified are empty" $ do
        it "returns a graph with the specified edges" $ do
          mkGraph 0 [] `shouldBe` Graph (AdjacencyList [])
      describe "when the edges specified are non-empty" $ do
        it "returns a graph with the specified edges" $ do
          mkGraph 4 [(0, 1), (1, 2), (2, 3)] `shouldBe` 
            Graph (AdjacencyList [[1], [0, 2], [1, 3], [2]])
    describe "bfs" $ do 
     it "returns the BFS search tree rooted at s" $ do
       bfs (mkGraph 3 [(0, 1), (0, 2), (1, 2)]) 0 `shouldBe` 
          Graph (AdjacencyList [[1, 2], [0], [0]])
    describe "dfs" $ do 
     it "returns the DFS search tree rooted at s" $ do
       dfs (mkGraph 3 [(0, 1), (0, 2), (1, 2)]) 0 `shouldBe` 
          Graph (AdjacencyList [[2], [2], [0, 1]])
      

