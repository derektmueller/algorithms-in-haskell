module AdjacencyListSpec where

import Test.QuickCheck
import Test.Hspec
import AdjacencyList

describe_adjacency_list = 
  describe "AdjacencyList" $ do
    describe "initAdjacencyList" $ do
      it "returns an adjacency list with n vertices and 0 edges" $ do
        initAdjacencyList 3  `shouldBe` AdjacencyList [[], [], []]
    describe "mkAdjacencyList" $ do
      describe "when the edges specified are non-empty" $ do
        it "returns an adjacency list with the specified edges" $ do
          mkAdjacencyList [(0, 1), (1, 2), (2, 3)] `shouldBe` 
            AdjacencyList [[1], [0, 2], [1, 3], [2]]
    describe "insert" $ do
      it "inserts the directed edge into the adjacency list" $ do
        insert (initAdjacencyList 5) (0, 1) `shouldBe` AdjacencyList [[1], [], [], [], []]
    describe "neighbors" $ do
      it "returns the neighbors of specified node" $ do
        neighbors (mkAdjacencyList [(0, 1), (1, 2), (2, 3)]) 1 `shouldBe` [0, 2]
    describe "vertices" $ do
      it "returns the vertices" $ do
        vertices (mkAdjacencyList [(0, 1), (1, 2), (2, 3)]) `shouldBe` [0, 1, 2, 3]
        
