module Main where

import Heap

main :: IO ()
main = do
  let heap = Heap [1, 2, 2, 3, 3, 3, 3, 0] 
  --putStrLn $ show $ key heap $ parent 1
  --putStrLn $ show $ parent 2
  --putStrLn $ show $ parent 3
  --putStrLn $ show $ parent 4
  putStrLn $ show heap
  putStrLn $ show $ heapifyUp heap 7
  let heap2 = Heap [4, 2, 2, 3, 3, 3, 3, 3] 
  putStrLn $ show $ heapifyDown heap2 0
  putStrLn $ show $ insert (heapifyDown heap2 0) 5
  putStrLn $ show $ insert (heapifyDown heap2 0) 1
  putStrLn $ show $ delete (insert (heapifyDown heap2 0) 1) 0

