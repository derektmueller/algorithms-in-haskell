module PriorityQueueSpec where

import Test.QuickCheck
import Test.Hspec
import Heap
import qualified PriorityQueue as PQ
import Data.List (unfoldr)

sorted :: Ord a => [a] -> Bool
sorted [] = True
sorted [x] = True
sorted (x:y:xs) = x <= y && sorted xs

describe_priority_queue = 
  describe "PriorityQueue" $ do
    describe "mkPriorityQueue" $ do
      it "returns an empty priority queue" $ do
        (PQ.mkPriorityQueue :: PQ.PriorityQueue Int) `shouldBe`
          PQ.PriorityQueue (Heap [])
    describe "empty" $ do
      it "returns True if the priority queue is empty" $ do
        PQ.empty (PQ.PriorityQueue (Heap ([0, 1, 1]))) == False
      it "returns False if the priority queue isn't empty" $ do
        PQ.empty (PQ.PriorityQueue (Heap ([]:: [Int]))) == True
    describe "insert" $ do
      it "inserts a value into the priority queue" $ do
        PQ.insert (PQ.PriorityQueue (Heap ([]))) 1 `shouldBe`
          PQ.PriorityQueue (Heap ([1]))
        PQ.insert (PQ.PriorityQueue (Heap ([2]))) 1 `shouldBe`
          PQ.PriorityQueue (Heap ([1, 2]))
    describe "remove" $ do
      it "removes the min value in a priority queue" $ do
        PQ.remove (PQ.PriorityQueue (Heap (([] :: [Int])))) `shouldBe`
          PQ.PriorityQueue (Heap ([]))
        PQ.remove (PQ.PriorityQueue (Heap ([1, 2]))) `shouldBe`
          PQ.PriorityQueue (Heap ([2]))
    describe "peek" $ do
      it "returns the min value in a priority queue" $ do
        PQ.peek (PQ.PriorityQueue (Heap (([] :: [Int])))) `shouldBe`
          Nothing
        PQ.peek (PQ.PriorityQueue (Heap ([1, 2]))) `shouldBe`
          Just 1
    specify "the client sorts an array with a priority queue" $ do
      property $ (\xs ->
        let pq = 
              foldr (\x pq -> PQ.insert pq x) 
              PQ.mkPriorityQueue (xs :: [Int])
            sortedArray = 
              unfoldr (\pq -> 
                if PQ.empty pq 
                then Nothing 
                else Just (PQ.peek pq, PQ.remove pq)) pq in
        sorted sortedArray)
