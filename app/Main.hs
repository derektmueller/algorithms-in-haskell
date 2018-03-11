module Main where

import Heap
import PriorityQueue
import Data.List
import Data.Maybe

main :: IO ()
main = do
  let heap = Heap [1, 2, 2, 3, 3, 3, 3, 0] 
  putStrLn $ show $ key heap $ parent 1
  putStrLn $ show $ parent 2
  putStrLn $ show $ parent 3
  putStrLn $ show $ parent 4
  putStrLn $ show heap
  putStrLn $ show $ heapifyUp heap 7
  let heap2 = Heap [4, 2, 2, 3, 3, 3, 3, 3] 
  putStrLn $ show $ heapifyDown heap2 0
  putStrLn $ show $ Heap.insert (heapifyDown heap2 0) 5
  putStrLn $ show $ Heap.insert (heapifyDown heap2 0) 1
  putStrLn $ show $ 
    Heap.delete (Heap.insert (heapifyDown heap2 0) 1) 0
  let heap3 = Heap.delete (Heap.insert (heapifyDown heap2 0) 1) 0
  putStrLn $ show heap3
  putStrLn $ show $ key heap3 ((size heap3) - 1)
  putStrLn $ show $ Heap.delete heap3 ((size heap3) - 1)

  let heap4 = Heap [1, 2, 3] 
  putStrLn $ show $ heap4
  putStrLn $ show $ Heap.swap heap4 0 2

  let heap5 = Heap [3, 2] 
  putStrLn $ show $ heap5
  putStrLn $ show $ Heap.heapifyDown heap5 0

  -- heapsort:
  let pq = 
        foldr (\x pq -> PriorityQueue.insert pq x) 
          mkPriorityQueue (reverse [1..10])
  let (PriorityQueue h) = pq
  putStrLn $ show h
  let sortedList = 
        unfoldr (\pq -> 
          if empty pq 
          then Nothing 
          else Just (peek pq, remove pq)) pq
  putStrLn $ show $ length sortedList
  putStrLn $ concat $ map show $ catMaybes sortedList
  return ()

