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
countOnes
  = undefined

countOnesFrom :: Int -> Int -> Int
countOnesFrom
  = undefined

getIndex :: Int -> Int -> Int -> Int
getIndex
  = undefined

-- Pre: the index is less than the length of the list
replace :: Int -> [a] -> a -> [a]
replace
  = undefined

-- Pre: the index is less than or equal to the length of the list
insertAt :: Int -> a -> [a] -> [a]
insertAt
  = undefined

--------------------------------------------------------------------
-- Part II

sumTrie :: (Int -> Int) -> ([Int] -> Int) -> Trie -> Int
sumTrie
  = undefined

{-
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
-}

member :: Int -> Hash -> Trie -> Int -> Bool
member
  = undefined

--------------------------------------------------------------------
-- Part III

insert :: HashFun -> Int -> Int -> Int -> Trie -> Trie
insert
  = undefined

buildTrie :: HashFun -> Int -> Int -> [Int] -> Trie
buildTrie
  = undefined