{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeApplications #-}

module Year2013.Exam where

-- > This test took me quite some months not because of difficulty but for I was
-- > kind of overloaded by life : /
-- >
-- > In terms of the content, it's mostly straightforward. My only doubt is the
-- > "binarySum" in Part 3 - the speç asks for not using "reverse", which I did
-- > not, however, I feel the use of "length" is already not elegant and it is
-- > effectively a second pass. However, I could not fathom a way to truly pass
-- > the lists just once given that the lists may differ in length.

import Data.Foldable (minimumBy)
import Data.Function (on)
import Test

type BinHeap a = [BinTree a]

data BinTree a = Node a Int (BinHeap a)
               deriving (Eq, Ord, Show)

--------------------------------------------------------------
-- PART I

-- > In the speç it's called "value", but here it's called "key"
key :: BinTree a -> a
key (Node a _ _) = a

rank :: BinTree a -> Int
rank (Node _ r _) = r

children :: BinTree a -> [BinTree a]
children (Node _ _ h) = h

combineTrees :: Ord a => BinTree a -> BinTree a -> BinTree a
combineTrees t1@(Node a1 r1 h1) t2@(Node a2 r2 h2)
  | a1 < a2   = Node a1 r (t2 : h1)
  | otherwise = Node a2 r (t1 : h2)
  where
    r = 1 + max r1 r2

--------------------------------------------------------------
-- PART II

extractMin :: Ord a => BinHeap a -> a
extractMin = minimum . map key

mergeHeaps :: Ord a => BinHeap a -> BinHeap a -> BinHeap a
mergeHeaps h1@(t1:ts1) h2@(t2:ts2)
  | r1 < r2   = t1 : mergeHeaps ts1 h2
  | r1 > r2   = t2 : mergeHeaps h1 ts2
  | otherwise = mergeHeaps [combineTrees t1 t2] (mergeHeaps ts1 ts2)
  where
    (r1, r2) = (rank t1, rank t2)
mergeHeaps h1 h2 = h1 <> h2 -- > To reach here, one of the heaps must be empty

insert :: Ord a => a -> BinHeap a -> BinHeap a
insert e = mergeHeaps [Node e 0 []]

deleteMin :: Ord a => BinHeap a -> BinHeap a
deleteMin [] = []
deleteMin h  = mergeHeaps ts (reverse ts')
  where
    (Node _ _ ts', ts) = removeMin h

-- > this function is mentioned in the speç, and we don't use it
remove :: Eq a => a -> BinHeap a -> BinHeap a
remove
  = undefined

removeMin :: Ord a => BinHeap a -> (BinTree a, BinHeap a)
removeMin (t:ts) = go t [] ts
  where
    go t acc [] = (t, reverse acc)
    go t acc (t':ts')
      | key t <= key t' = go t (t':acc) ts'
      | otherwise       = go t' (t:acc) ts'

binSort :: Ord a => [a] -> [a]
binSort = go . foldr insert []
  where
    go [] = []
    go h  = extractMin h : go (deleteMin h)

--------------------------------------------------------------
-- PART III

toBinary :: BinHeap a -> [Int]
toBinary h = go 0 (map rank h)
  where
    go _ []     = []
    go d (r:rs) = go (r + 1) rs ++ 1 : replicate (r - d) 0

binarySum :: [Int] -> [Int] -> [Int]
binarySum xs ys
  | lenXs < lenYs = binarySum (replicate (lenYs - lenXs) 0 ++ xs) ys
  | lenXs > lenYs = binarySum xs (replicate (lenXs - lenYs) 0 ++ ys)
  | otherwise     = fromEnum carry : map fromEnum res
  where
    (res, carry)       = go (toEnum <$> xs) (toEnum <$> ys) False
    (lenXs, lenYs)     = (length xs, length ys)
    go [] [] c         = ([], c)
    go (x:xs) (y:ys) c = let (s, c')  = go xs ys c
                             (r, c'') = fullAdder x y c'
                          in (r : s, c'')
    fullAdder x y c    = let xor = x /= y in (xor /= c, x && y || c && xor)

------------------------------------------------------
-- Some sample trees...

t1, t2, t3, t4, t5, t6, t7, t8 :: BinTree Int
-- Note: t7 is the result of merging t5 and t6

-- t1 to t4 appear in Figure 1...
t1 = Node 4 0 []
t2 = Node 1 1 [Node 5 0 []]
t3 = Node 2 2 [Node 8 1 [Node 9 0 []], 
               Node 7 0 []]
t4 = Node 2 3 [Node 3 2 [Node 6 1 [Node 8 0 []], 
                         Node 10 0 []],
               Node 8 1 [Node 9 0 []],
               Node 7 0 []]

-- t5 and t6 are on the left of Figure 2; t7 is on the
-- right
t5 = Node 4 2 [Node 6 1 [Node 8 0 []], 
                         Node 10 0 []]
t6 = Node 2 2 [Node 8 1 [Node 9 0 []], Node 7 0 []]
t7 = Node 2 3 [Node 4 2 [Node 6 1 [Node 8 0 []], Node 10 0 []],
               Node 8 1 [Node 9 0 []], 
               Node 7 0 []]

-- An additional tree...
t8 = Node 12 1 [Node 16 0 []]

------------------------------------------------------
-- Some sample heaps...

h1, h2, h3, h4, h5, h6, h7 :: BinHeap Int
-- Two arbitrary heaps for testing...
h1 = [t2, t7]
h2 = [Node 1 2 [Node 12 1 [Node 16 0 []],
                Node 5 0 []],
      Node 2 3 [Node 4 2 [Node 6 1 [Node 8 0 []],
                          Node 10 0 []],
                Node 8 1 [Node 9 0 []],
                Node 7 0 []]]

-- h3 is shown in Figure 3...
h3 = [t1, t2, t4]

-- Two additional heaps, used below. They are shown
-- in Figure 4(a)...

h4 = [t2, t5]
h5 = [t1, t8]

-- h6 is the result of merging h4 and h5, shown in Figure 4(b)...
h6 = [Node 4 0 [],
      Node 1 3 [Node 4 2 [Node 6 1 [Node 8 0 []],
                          Node 10 0 []],
                Node 12 1 [Node 16 0 []],
                Node 5 0 []]]

-- h7 is shown in Figure 5...
h7 = [Node 4 3 [Node 4 2 [Node 12 1 [Node 16 0 []],
                          Node 5 0 []],
                Node 6 1 [Node 8 0 []],
                Node 10 0 []]]

---------------------------------------------------------
-- Tests

tester :: IO ()
tester = runTest do
  label "Test 'key'" do
    key t4 ==. 2
  label "Test 'rank'" do
    rank t7 ==. 3
  label "Test 'children'" do
    children t2 ==. [Node 5 0 []]
  label "Test 'combineTrees'" do
    combineTrees t5 t6 ==. t7
  label "Test 'extractMin'" do
    extractMin h3 ==. 1
  label "Test 'mergeHeaps'" do
    mergeHeaps h4 h5 ==. h6
  label "Test 'insert'" do
    insert 7 [t1] ==. [Node 4 1 [Node 7 0 []]]
  label "Test 'deleteMin'" do
    deleteMin h6 ==. h7
  label "Test 'binSort'" do
    binSort @Int [] ==. []
    binSort [1, 4, 2, 8, 5, 7, 6] ==. [1, 2, 4, 5, 6, 7, 8]
  label "Test 'toBinary'" do
    toBinary h2 ==. [1, 1, 0, 0]
  label "Test 'binarySum'" do
    binarySum (toBinary h4) (toBinary h5) ==. [1, 0, 0, 1]
