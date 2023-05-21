module Year2021.Alloc where

-- > This is the test I took when I was in my first year. I remember that I
-- > spent way too long on Part V, trying to find a way up and over, but I was
-- > not able to finish it, and it wasted me precious time that I could have
-- > spent on reviewing other parts.
-- >
-- > In my second year, I implemented a similar graph colouring algorithm for
-- > the WACC compiler project. Therefore, this is my third time visiting this
-- > test.
-- >
-- > While the assessed sections are generally straighforward, Part V is
-- > arguably the most difficult part across all tests. I spent a lot of time
-- > trying to find a neat one-pass solution for "buildCFG", and while I did
-- > manage to come up with one, I believe there must be solutions more elegant.

import Data.Maybe
import Data.List

import Year2021.Types
import Year2021.Examples

import Control.Monad.Trans.State
import Data.Bifunctor
import Data.Ord

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
-- > Verbatim translation of the algorithm. Note that the the union is performed
-- > AFTER the difference, which confused me more than once.
liveVars cfg = buildLV $ replicate (length cfg) []
  where
    buildLV curLVs
      | nextLVs == curLVs = curLVs
      | otherwise         = buildLV nextLVs
      where
        nextLVs = zipWith buildLine [0..] cfg
        buildLine ix ((d, u), s)
          = u `union` (foldl union [] [curLVs !! next | next <- s] \\ [d])

buildCFG :: Function -> CFG
-- > This implementation is admittedly convoluted :/
-- >
-- > Extracting "def" and "use" from each @Statement@ is trivial, however, to
-- get the successor nodes, we need to know how long each @Block@ is. This is
-- > accomplished with a @State@ monad that keeps track of the current index.
-- >
-- > The real problem occurs when we deal with branching. When the "then" branch
-- > ends, its successor not the next node (which would be the starting point
-- > of the "else" branch), but (usually) the node after the "else" branch.
-- > Things become even trickier when we have nested flow controls. One solution
-- > to this problem is to always "revisit" the last statement in the "then"
-- > branch and modify its successor nodes once we have the information.
-- > However, this is not only inefficient (we need to visit the same node at
-- > least twice), but also requires taking note of many edge cases, e.g. when
-- one (or both) of the branches are empty, or if there is nested branchings, or
-- if the last statement in the branch body is a return statment, etc.
-- >
-- > The solution here took a different approach. Instead of returning the list
-- > of CFG entries for each block, we return a function that takes an index and
-- > returns a list of entries. When we have successive blocks, we take the
-- > current index and pass it to the function of the previous block. In this
-- > way, we can easily modify the successor nodes of the previous block.
-- >
-- > The approach is arguably harder to understand, but once you get it, it
-- > handles all the edge cases automatically.
buildCFG (_, _, stmts) = flip evalState 0 $ do
  builder <- build stmts
  builder <$> get
  where
    getVars                  = nub . getVars'
    getVars' (Const _)       = []
    getVars' (Var x)         = [x]
    getVars' (Apply _ e1 e2) = getVars e1 ++ getVars e2
    build []                 = pure $ const []
    build [stmt]             = buildOne stmt
    build (stmt : stmts)     = do
      cur  <- buildOne stmt
      ix   <- get
      rest <- build stmts
      pure $ \ix' -> cur ix ++ rest ix'
    buildOne stmt           = do
      ix <- get
      let next = ix + 1
      put next
      case stmt of
        Assign v exp -> pure $ case v of
          "return" -> const [((v, getVars exp), [])]
          _        -> \ix' -> [((v, getVars exp), [ix'])]
        If exp b b'  -> do
          builder  <- build b
          middle   <- get
          builder' <- build b'
          let builderExp ix' = (if null b then ix' else next)
                             : [if null b' then ix' else middle]
          pure $ \ix' -> (("_", getVars exp), nub $ builderExp ix')
                       : builder ix' ++ builder' ix'
        While exp b  -> do
          builder <- build b
          pure $ \ix' -> (("_", getVars exp), nub $ ix' : [next | not $ null b])
                       : builder ix
