module Year2020.Tries where

import Data.List hiding (insert)
import Data.Bits

import Year2020.Types
import Year2020.HashFunctions
import Year2020.Examples

--------------------------------------------------------------------
-- Part I

-- Use this if you're counting the number of 1s in every
-- four-bit block...
bitTable :: [Int]
bitTable
  = [0,1,1,2,1,2,2,3,1,2,2,3,2,3,3,4]

countOnes :: Int -> Int
countOnes n
  | n < 16    = bitTable !! n
  | otherwise = let (q, r) = n `divMod` 16
                in bitTable !! r + countOnes q

countOnesFrom :: Int -> Int -> Int
countOnesFrom k n = countOnes (n .&. (2 ^ k - 1))

getIndex :: Int -> Int -> Int -> Int
getIndex n l b = (n `shiftR` (l * b)) .&. (2 ^ b - 1)

-- Pre: the index is less than the length of the list
replace :: Int -> [a] -> a -> [a]
replace n xs _ | n < 0 = xs
replace 0 (_ : xs) x'  = x' : xs
replace n (x : xs) x'  = x : replace (n - 1) xs x'

-- Pre: the index is less than or equal to the length of the list
insertAt :: Int -> a -> [a] -> [a]
insertAt n x' xs | n <= 0 = x' : xs
insertAt n x' (x : xs)    = x : insertAt (n - 1) x' xs

--------------------------------------------------------------------
-- Part II

sumTrie :: (Int -> Int) -> ([Int] -> Int) -> Trie -> Int
sumTrie _ r (Leaf vs)   = r vs
sumTrie f r (Node _ ns) = sum (map sumSubNode ns)
  where
    sumSubNode (Term v)   = f v
    sumSubNode (SubTrie t) = sumTrie f r t

--
-- If you get the sumTrie function above working you can uncomment
-- these three functions; they may be useful in testing.
--
trieSize :: Trie -> Int
trieSize t
  = sumTrie (const 1) length t

binCount :: Trie -> Int
binCount t
  = sumTrie (const 1) (const 1) t

meanBinSize :: Trie -> Double
meanBinSize t
  = fromIntegral (trieSize t) / fromIntegral (binCount t)

member :: Int -> Hash -> Trie -> Int -> Bool
member v h t b = go 0 t
  where
    go _ (Leaf vs)    = v `elem` vs
    go l (Node bv ns)
      | not (testBit bv ix)  = False
      | Term v' <- n         = v' == v
      | SubTrie t' <- n      = go (l + 1) t'
      where
        ix = getIndex h l b
        n  = ns !! countOnesFrom ix bv

--------------------------------------------------------------------
-- Part III

insert :: HashFun -> Int -> Int -> Int -> Trie -> Trie
insert f d b v t = go 0 v t
  where
    go' l v t             = SubTrie $ go l v t
    go _ v (Leaf vs)
      | v `elem` vs = Leaf $ sort vs
      | otherwise   = Leaf $ sort (v : vs)
    go l v _ | l >= d - 1 = Leaf [v]
    go l v (Node bv ns)
      | not (testBit bv ix) = Node (setBit bv ix) (insertAt ix' (Term v) ns)
      | SubTrie t' <- n     = Node bv (replace ix' ns $ go' (l + 1) v t')
      | Term v' <- n        = if v' == v
        then Node bv ns
        else Node bv (replace ix' ns $ go' (l + 1) v $ go (l + 1) v' empty)
      where
        ix  = getIndex (f v) l b
        ix' = countOnesFrom ix bv
        n   = ns !! countOnesFrom ix bv

buildTrie :: HashFun -> Int -> Int -> [Int] -> Trie
buildTrie f d b = foldl' (flip $ insert f d b) empty
