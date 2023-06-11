module Year2019.SOL where

import Data.List
import Data.Maybe

import Year2019.Types
import Year2019.TestData

printF :: Formula -> IO()
printF
  = putStrLn . showF
  where
    showF (Var v)
      = v
    showF (Not f)
      = '!' : showF f
    showF (And f f')
      = "(" ++ showF f ++ " & " ++ showF f' ++ ")"
    showF (Or f f')
      = "(" ++ showF f ++ " | " ++ showF f' ++ ")"

--------------------------------------------------------------------------
-- Part I

-- 1 mark
lookUp :: Eq a => a -> [(a, b)] -> b
-- Pre: The item being looked up has a unique binding in the list
lookUp 
  = undefined

-- 3 marks
vars :: Formula -> [Id]
vars 
  = undefined

-- 1 mark
idMap :: Formula -> IdMap
idMap 
  = undefined

--------------------------------------------------------------------------
-- Part II

-- An encoding of the Or distribution rules.
-- Both arguments are assumed to be in CNF, so the
-- arguments of all And nodes will also be in CNF.
distribute :: CNF -> CNF -> CNF
distribute a (And b c)
  = And (distribute a b) (distribute a c)
distribute (And a b) c
  = And (distribute a c) (distribute b c)
distribute a b
  = Or a b

-- 4 marks
toNNF :: Formula -> NNF
toNNF 
  = undefined

-- 3 marks
toCNF :: Formula -> CNF
toCNF 
  = undefined

-- 4 marks
flatten :: CNF -> CNFRep
flatten 
  = undefined

--------------------------------------------------------------------------
-- Part III

-- 5 marks
propUnits :: CNFRep -> (CNFRep, [Int])
propUnits 
  = undefined

-- 4 marks
dp :: CNFRep -> [[Int]]
dp 
  = undefined

--------------------------------------------------------------------------
-- Part IV

-- Bonus 2 marks
allSat :: Formula -> [[(Id, Bool)]]
allSat
  = undefined
