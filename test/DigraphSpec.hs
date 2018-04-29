module DigraphSpec where

import Test.QuickCheck
import Test.Hspec
import Digraph
import qualified AdjacencyList as AL
import TopSort

describe_digraph = 
  describe "Digraph" $ do
    describe "mkDigraph" $ do
      describe "when the edges specified are empty" $ do
        it "returns a graph with the specified edges" $ do
          mkDigraph 0 [] `shouldBe` 
            Digraph (AL.AdjacencyList []) (AL.AdjacencyList [])
      describe "when the edges specified are non-empty" $ do
        it "returns a graph with the specified edges" $ do
          mkDigraph 4 [(0, 1), (1, 2), (2, 3)] `shouldBe` 
            Digraph 
              (AL.AdjacencyList [[1], [2], [3], []])
              (AL.AdjacencyList [[], [0], [1], [2]])
    describe "deleteEdge" $ do
      it "returns a graph with the specified edge removed" $ do
        deleteEdge (mkDigraph 2 [(0, 1)]) (0, 1) `shouldBe` 
          mkDigraph 2 []
    describe "delete" $ do
      it "returns a graph with the specified vertex removed" $ do
        delete (mkDigraph 2 [(0, 1)]) 0 `shouldBe` mkDigraph 2 []
    describe "topological sort" $ do 
      it "returns an array sorted topologically" $ do
        topSort (mkDigraph 3 [(0, 1), (0, 2), (1, 2)]) `shouldBe` 
          Just [0, 1, 2]
      it "returns Nothing if there's a cycle" $ do
        topSort (mkDigraph 3 [(0, 1), (1, 2), (2, 0)]) `shouldBe` 
          Nothing
      

