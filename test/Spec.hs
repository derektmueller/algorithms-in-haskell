module Main where

import Test.QuickCheck
import Test.Hspec
import HeapSpec
import PriorityQueueSpec
import AdjacencyListSpec
import GraphSpec

main :: IO ()
main = hspec $ do
  describe_heap
  describe_priority_queue
  describe_adjacency_list
  describe_graph

