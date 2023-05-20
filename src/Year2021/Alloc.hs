module Year2021.Alloc where

import Data.Maybe
import Data.List

import Year2021.Types
import Year2021.Examples
import Data.Ord
import Data.Bifunctor

------------------------------------------------------
--
-- Part I
--
count :: Eq a => a -> [a] -> Int
count = (length .) . filter . (==)

degrees :: Eq a => Graph a -> [(a, Int)]
degrees (ns, es) = [(n, count n $ concatMap (\(x, y) -> [x, y]) es) | n <- ns]

neighbours :: Eq a => a -> Graph a -> [a]
neighbours n (ns, es) = [y | (x, y) <- es, n == x] ++ [x | (x, y) <- es, n == y]

removeNode :: Eq a => a -> Graph a -> Graph a
removeNode n (ns, es) = (ns \\ [n], [(x, y) | (x, y) <- es, x /= n, y /= n])

------------------------------------------------------
--
-- Part II
--
colourGraph :: (Ord a, Show a) => Int -> Graph a -> Colouring a
colourGraph _ ([], _) = []
colourGraph k g       = (n, c) : cMap'
  where
    (n, d) = minimumBy (comparing snd) (degrees g)
    ns     = neighbours n g
    g'     = removeNode n g
    cMap'  = colourGraph k g'
    ncs    = map (`lookUp` cMap') ns
    c      = fromMaybe 0 $ listToMaybe [c | c <- [1..k], c `notElem` ncs]

------------------------------------------------------
--
-- Part III
--
buildIdMap :: Colouring Id -> IdMap
buildIdMap = (("return", "return") :) . map (second (('R' :) . show))

buildArgAssignments :: [Id] -> IdMap -> [Statement]
buildArgAssignments xs idMap = map (\x -> Assign (lookUp x idMap) (Var x)) xs

renameExp :: Exp -> IdMap -> Exp
-- Pre: A precondition is that every variable referenced in 
-- the expression is in the idMap. 
renameExp (Const i) _   = Const i
renameExp (Var x) idMap = Var (lookUp x idMap)
renameExp (Apply op e1 e2) idMap
  = Apply op (renameExp e1 idMap) (renameExp e2 idMap)

renameBlock :: Block -> IdMap -> Block
-- Pre: A precondition is that every variable referenced in 
-- the block is in the idMap. 
renameBlock b idMap = map renameStatement b
  where
    renameStatement (Assign x e)
      = Assign (lookUp x idMap) (renameExp e idMap)
    renameStatement (If e b1 b2)
      = If (renameExp e idMap) (renameBlock b1 idMap) (renameBlock b2 idMap)
    renameStatement (While e b)
      = While (renameExp e idMap) (renameBlock b idMap)

renameFun :: Function -> IdMap -> Function
renameFun (f, as, b) idMap
  = (f, as, buildArgAssignments as idMap ++ renameBlock b idMap)

-----------------------------------------------------
--
-- Part IV
--
buildIG :: [[Id]] -> IG
buildIG liveVars = (ns, es)
  where
    ns = nub $ concat liveVars
    es = nub $ concatMap (\xs -> [(x, y) | x <- xs, y <- xs, x < y]) liveVars

-----------------------------------------------------
--
-- Part V
--
liveVars :: CFG -> [[Id]]
liveVars 
  = undefined

buildCFG :: Function -> CFG
buildCFG 
  = undefined