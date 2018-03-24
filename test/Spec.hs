module Main where

import Test.QuickCheck
import Test.Hspec
import HeapSpec
import PriorityQueueSpec

main :: IO ()
main = hspec $ do
  describe_heap
  describe_priority_queue

