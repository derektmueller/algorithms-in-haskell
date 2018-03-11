module Heap where

newtype Heap a = Heap [a]

instance (Show a) => Show (Heap a) where
  show h = show' h 0
    where
      show' h@(Heap a) i = 
        if i < length a
        then
          (take (depth i) $ repeat ' ') ++ 
          (show $ key h i) ++ 
          "\n" ++ 
          show' h (left i) ++
          show' h (right i)
        else []

size :: Heap a -> Int
size (Heap a) = length a

depth :: Integral a => Int -> a
depth i = floor (logBase 2 (fromIntegral (i + 1)))

key :: Heap a -> Int -> a
key (Heap a) i = a !! i

parent :: Int -> Int
parent i = floor ((fromIntegral (i + 1)) / 2) - 1

left :: Int -> Int
left i = 2 * i + 1

right :: Int -> Int
right i = 2 * i + 2

swap :: Heap a -> Int -> Int -> Heap a
swap h@(Heap a) parentIndex childIndex = 
  let j = parentIndex
      i = childIndex in
      Heap (
        (take j a) ++ 
        [key h i] ++ 
        (take (i - j - 1) (drop (j + 1) a)) ++
        [key h j] ++ 
        (drop (i + 1) a))

heapifyUp :: Ord a => Heap a -> Int -> Heap a
heapifyUp h@(Heap a) i = 
  if i > 0
  then let j = parent i in
    if key h i < key h j
    then heapifyUp (swap h j i) j
    else h
  else h

heapifyDown :: Ord a => Heap a -> Int -> Heap a
heapifyDown h@(Heap a) i =
  let n = size h in
  if left i > n - 1
  then h
  else 
    let j = if left i < n - 1
        then 
          let minKey = minimum [
                key h (left i), key h (right i)] in
            if minKey == key h (left i)
            then left i
            else right i
        else left i
    in
      if key h j < key h i
      then heapifyDown (swap h i j) j
      else h

insert :: Ord a => Heap a -> a -> Heap a
insert h@(Heap a) v = heapifyUp (Heap (a ++ [v])) (length a)

findMin :: Heap a -> Maybe a
findMin (Heap []) = Nothing
findMin (Heap (x:xs)) = Just x

delete :: Ord a => Heap a -> Int -> Heap a
delete h@(Heap a) i
  | i < (length a - 1) = 
    heapifyDown (delete (swap h i (length a - 1)) (length a - 1)) i
  | i == (length a - 1) = Heap (take i a)
  | otherwise = h
     
