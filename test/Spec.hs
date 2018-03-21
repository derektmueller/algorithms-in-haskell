module Main where

import Test.QuickCheck
import Test.QuickCheck.Monadic
import Test.Hspec
import Heap
import qualified PriorityQueue as PQ
import Data.List (unfoldr)

sorted :: Ord a => [a] -> Bool
sorted [] = True
sorted [x] = True
sorted (x:y:xs) = x <= y && sorted xs

main :: IO ()
main = hspec $ do
  describe "Heap" $ do
    describe "size" $ do
      it "returns the size of the heap" $ do
        size (Heap [1, 2, 3]) `shouldBe` 3
    describe "depth" $ do
      it "returns depth of the node at a given index" $ do
        depth 2 `shouldBe` 1
    describe "key" $ do
      it "returns the value in a heap node at a given index" $ do
        key (Heap ['a', 'b', 'c']) 2 `shouldBe` 'c' 
    describe "parent" $ do
      it "returns index of the parent of the node at a given \
         \index" $ do
        parent 2 `shouldBe` 0
    describe "left" $ do
      it "returns index of the left child of the node at a given \
         \index" $ do
        left 0 `shouldBe` 1
    describe "right" $ do
      it "returns index of the right child of the node at a given \
         \index" $ do
        right 0 `shouldBe` 2
    describe "swap" $ do
      it "swaps nodes at specified indexes" $ do
        swap (Heap ['a', 'b', 'c']) 0 2 `shouldBe` 
          Heap ['c', 'b', 'a']
    describe "heapifyUp" $ do
      it "restore heap property by performing successive swaps \
         \with parent node" $ do
        heapifyUp (Heap [1, 0]) 1 `shouldBe` 
          Heap [0, 1]
        heapifyUp (Heap [0, 1, 1, 2, 2, 2, 0]) 6 `shouldBe` 
          Heap [0, 1, 0, 2, 2, 2, 1]
    describe "heapifyDown" $ do
      it "restore heap property by performing successive swaps \
         \with child node" $ do
        heapifyDown (Heap [1, 0]) 0 `shouldBe` 
          Heap [0, 1]
        heapifyDown (Heap [3, 1, 1, 2, 2, 2, 2]) 0 `shouldBe` 
          Heap [1, 2, 1, 3, 2, 2, 2]
    describe "insert" $ do
      it "insert value into heap" $ do
        insert (Heap [1]) 0 `shouldBe` Heap [0, 1]
        insert (Heap [1, 2, 2]) 0 `shouldBe` Heap [0, 1, 2, 2]
    describe "findMin" $ do
      it "returns just the min value, or nothing, if the heap is \
         \empty" $ do
        findMin (Heap [1]) `shouldBe` Just 1
        findMin (Heap ([] :: [Int])) `shouldBe` Nothing
    describe "delete" $ do
      it "delete the node at a given index" $ do
        delete (Heap [1]) 0 `shouldBe` Heap []
        delete (Heap ([] :: [Int])) 0 `shouldBe` Heap []
        delete (Heap [1]) 1 `shouldBe` Heap [1]
        delete (Heap [0, 1, 1, 2, 2, 2, 2]) 3 `shouldBe` 
          Heap [0, 1, 1, 2, 2, 2]
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

