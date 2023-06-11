module Year2019.SOL where

import Control.Monad
import Control.Monad.Trans.Reader
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
lookUp a = snd . head . filter ((== a) . fst)

-- 3 marks
vars :: Formula -> [Id]
vars (Var v)    = [v]
vars (Not f)    = vars f
vars (And f f') = sort . nub $ vars f ++ vars f'
vars (Or f f')  = sort . nub $ vars f ++ vars f'

-- 1 mark
idMap :: Formula -> IdMap
idMap f = zip (vars f) [1..]

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
toNNF (And f f')       = And (toNNF f) (toNNF f')
toNNF (Or f f')        = Or (toNNF f) (toNNF f')
toNNF (Not (And f f')) = Or (toNNF (Not f)) (toNNF (Not f'))
toNNF (Not (Or f f'))  = And (toNNF (Not f)) (toNNF (Not f'))
toNNF (Not (Not f))    = toNNF f
toNNF f                = f

-- 3 marks
toCNF :: Formula -> CNF
toCNF = toCNF' . toNNF
  where
    toCNF' (And f f') = And (toCNF' f) (toCNF' f')
    toCNF' (Or f f')  = distribute (toCNF' f) (toCNF' f')
    toCNF' f          = f

-- 4 marks
flatten :: CNF -> CNFRep
flatten f = (`runReader` idMap f) (flattenAnd f)
  where
    flattenVar (Var v)    = lookUp v <$> ask
    flattenVar (Not f)    = negate <$> flattenVar f
    flattenOr (Or f f')   = liftM2 (++) (flattenOr f) (flattenOr f')
    flattenOr f           = pure <$> flattenVar f
    flattenAnd (And f f') = liftM2 (++) (flattenAnd f) (flattenAnd f')
    flattenAnd f          = pure <$> flattenOr f



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
