module PriorityQueue where

import Heap

newtype PriorityQueue a = PriorityQueue (Heap a)

mkPriorityQueue :: PriorityQueue a
mkPriorityQueue = PriorityQueue (Heap [])

empty :: PriorityQueue a -> Bool
empty (PriorityQueue (Heap [])) = True
empty _ = False

insert :: Ord a => PriorityQueue a -> a -> PriorityQueue a
insert (PriorityQueue h) a = PriorityQueue (Heap.insert h a)

remove :: Ord a => PriorityQueue a -> PriorityQueue a
remove (PriorityQueue h) = PriorityQueue (delete h 0)

peek :: PriorityQueue a -> Maybe a
peek (PriorityQueue h) = findMin h
