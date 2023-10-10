{-# LANGUAGE BlockArguments #-}

module Year2019.SOL where

-- > This year's test is overall easy. The interesting part is that I have
-- > studied the DP(LL) algorithm in the second year module of 50009, and
-- > a (simplified) implementation would be even more straightforward for me.
-- >
-- > SAT problem is NP-hard, thus we do not know any "efficient" algorithm for
-- > it. This algorithm has a worst case of O(2^n), but in practice the worst
-- > case is often avoided, especially if you have a good heuristic for choosing
-- > which literal to guess next.
-- >
-- > Note that in practice, the DP algorithm itself is not used for SAT solving
-- > (although the foundational idea remains), and we are usually just
-- > interested in one solution rather than all (which is an even harder
-- > problem). Furthermore, instead of translating the original formula into
-- > CNF (which is by itself exponential), we could use the Tseitin
-- > transformation to create a different CNF formula that is equisatisfiable to
-- > the original one. This is what modern SAT solvers do.

import Control.Monad
import Control.Monad.Trans.State
import Data.List
import Data.Tuple

import Year2019.Types
import Year2019.TestData

import Test

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
-- > The idea is very similar to parser combinators :)
flatten f = flattenAnd f
  where
    flattenAnd (And f f') = flattenAnd f ++ flattenAnd f'
    flattenAnd f          = [flattenOr f]
    flattenOr (Or f f')   = flattenOr f ++ flattenOr f'
    flattenOr f           = [flattenVar f]
    flattenVar (Var v)    = lookUp v (idMap f)
    flattenVar (Not f)    = negate (flattenVar f)


--------------------------------------------------------------------------
-- Part III

-- 5 marks
propUnits :: CNFRep -> (CNFRep, [Int])
-- > The "State" here arguably complicates things :/
propUnits = swap . runState worker
  where
    findSingleton []         = Nothing
    findSingleton ([x] : xs) = Just x
    findSingleton (_ : xs)   = findSingleton xs
    worker = do
      cnf <- get
      case findSingleton cnf of
        Nothing -> pure []
        Just x  -> do
          modify (map (\\ [-x]) . filter (notElem x))
          (x :) <$> worker

-- 4 marks
dp :: CNFRep -> [[Int]]
dp cnfRep
  | any null cnfRep' = []
  | otherwise        = case cnfRep' of
    []            -> [asgns]
    ((x : _) : _) -> map ((x : asgns) ++) (dp (set x))
                  ++ map ((-x : asgns) ++) (dp (set (-x)))
  where
    (cnfRep', asgns) = propUnits cnfRep
    set x            = map (\\ [-x]) $ filter (notElem x) cnfRep'


--------------------------------------------------------------------------
-- Part IV

-- Bonus 2 marks
allSat :: Formula -> [[(Id, Bool)]]
allSat f = sort . map sort
         $ concatMap (complete . map toAsgn) (dp . flatten $ toCNF f)
  where
    toAsgn x
      | x > 0     = (lookUp x idToV, True)
      | otherwise = (lookUp (-x) idToV, False)
    idToV          = swap <$> idMap f
    complete asgns = foldM (\a v -> [(v, True) : a, (v, False) : a])
                           asgns (vars f \\ map fst asgns)

---------------------------------------------------------
-- Test & Helpers

tester :: IO ()
tester = runTest do
  label "Test 'lookUp'" do
    lookUp 3 [(2, 1), (3, 8)] ==. 8
  label "Test 'vars'" do
    vars f1 ==. ["a"]
    vars cnf3 ==. ["a", "b", "c", "d", "e", "f", "g"]
  label "Test 'idMap'" do
    idMap f1 ==. [("a", 1)]
    idMap cnf3 ==. [("a", 1), ("b", 2), ("c", 3), ("d", 4), ("e", 5), ("f", 6), ("g", 7)]
  label "Test 'toNNF'" do
    toNNF f1 ==. Var "a"
    toNNF f6 ==. And (Not (Var "b")) (Or (Not (Var "a")) (Var "c"))
    toNNF f8 ==. Or (And (Var "a") (And (Not (Var "b")) (Not (Var "c")))) (Not (Var "d"))
  label "Test 'toCNF'" do
    toCNF f4 ==. Or (Var "a") (Var "b")
    toCNF f6 ==. And (Not (Var "b")) (Or (Not (Var "a")) (Var "c"))
    toCNF f8 ==. And (Or (Var "a") (Not (Var "d"))) (And (Or (Not (Var "b")) (Not (Var "d"))) (Or (Not (Var "c")) (Not (Var "d"))))
  label "Test 'flatten'" do
    flatten c3 ==. [[1], [2]]
    flatten c4 ==. [[1, 2]]
    flatten cnf3 ==. [[-3, -7], [2, 3, 5], [1, -2], [1, -5], [-1, -4], [3, 4, 6], [-1, -6], [7]]
  label "Test 'propUnits'" do
    propUnits [] ==. ([], [])
    propUnits cnf1Rep ==. ([[1, 2, 3], [1, 2, -3], [1, -2, 3], [1, -2, -3], [-1, 2, 3], [-1, 2, -3], [-1, -2, 3]], [])
    propUnits cnf2Rep ==. ([[2, 5], [3, -2], [3, -5], [-3, -7], [7, 6], [-3, 6]], [4, -1])
  label "Test 'dp'" do
    sortOn abs <$> dp cnf1Rep ==. [[1, 2, 3]]
    sortOn abs <$> dp cnf2Rep ==. [[-1, 2, 3, 4, 6, -7], [-1, -2, 3, 4, 5, 6, -7]]
  label "Test 'allSat'" do
    allSat cnf1 ==. [[("a", True), ("b", True), ("c", True)]]
    allSat cnf2 ==. [ [("a", False), ("b", False), ("c", True), ("d", True), ("e", True), ("f", True), ("g", False)]
                    , [("a", False), ("b", True), ("c", True), ("d", True), ("e", False), ("f", True), ("g", False)]
                    , [("a", False), ("b", True), ("c", True), ("d", True), ("e", True), ("f", True), ("g", False)] ]
    allSat cnf3 ==. []
    allSat cnf4 ==. [ [("a", False), ("b", True), ("c", True), ("d", False)]
                    , [("a", True), ("b", False), ("c", False), ("d", True)] ]
