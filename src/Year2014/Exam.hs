{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TupleSections #-}

module Year2014.Exam where

import Data.Maybe
import Data.List

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import qualified Control.Monad.Trans.State as S
import qualified Data.IntMap as IM
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Test

data RE = Null   |
          Term Char |
          Seq RE RE |
          Alt RE RE |
          Rep RE    |
          Plus RE   |
          Opt RE
        deriving (Eq, Show)

type State = Int

data Label = C Char | Eps
           deriving (Eq, Ord, Show)

type Transition = (State, State, Label)

type Automaton = (State, [State], [Transition])

--------------------------------------------------------
-- showRE - this may be useful for testing

showRE :: RE -> String
showRE (Seq re re')
  = showRE re ++ showRE re'
showRE (Alt re re')
  = "(" ++ showRE re ++ "|" ++ showRE re' ++ ")"
showRE (Rep re)
  = showRE' re ++ "*"
showRE (Plus re)
  = showRE' re ++ "+"
showRE (Opt re)
  =  showRE' re ++ "?"
showRE re
  = showRE' re

showRE' Null
  = ""
showRE' (Term c)
  = [c]
showRE' (Alt re re')
  = showRE (Alt re re')
showRE' re
  = "(" ++ showRE re ++ ")"

--------------------------------------------------------
-- Part I

lookUp :: Eq a => a -> [(a, b)] -> b
--Pre: There is exactly one occurrence of the item being looked up.
lookUp x ((x',y):xys)
  | x == x'   = y
  | otherwise = lookUp x xys

simplify :: RE -> RE
simplify (Seq re re')
  = Seq (simplify re) (simplify re')
simplify (Alt re re')
  = Alt (simplify re) (simplify re')
simplify (Rep re)
  = Rep (simplify re)
simplify (Plus re)
  = Seq (simplify re) (Rep (simplify re))
simplify (Opt re)
  = Alt (simplify re) Null
simplify re
  = re

-- A custom helper
transStarts :: [Transition] -> [State]
transStarts = map (\(s, _, _) -> s)

--------------------------------------------------------
-- Part II

startState :: Automaton -> State
startState (s, _, _) = s

terminalStates :: Automaton -> [State]
terminalStates (_, ts, _) = ts

transitions :: Automaton -> [Transition]
transitions (_, _, ts) = ts

isTerminal :: State -> Automaton -> Bool
isTerminal s au = s `elem` terminalStates au

transitionsFrom :: State -> Automaton -> [Transition]
transitionsFrom s au = filter (\(s', _, _) -> s' == s) (transitions au)

labels :: [Transition] -> [Label]
labels ts = nub (map (\(_, _, l) -> l) ts) \\ [Eps]

accepts :: Automaton -> String -> Bool
accepts au = accepts' (startState au)
  where
    try str (_, s, Eps) = accepts' s str
    try (c : cs) (_, s, C c')
      | c == c' = accepts' s cs
    try _ _             = False
    accepts' s str
      | isTerminal s au = null str
      | otherwise       = any (try str) trans
      where
        trans = transitionsFrom s au

--------------------------------------------------------
-- Part III

makeNDA :: RE -> Automaton
makeNDA re
  = (1, [2], sort transitions)
  where
    (transitions, k) = make (simplify re) 1 2 3

make :: RE -> Int -> Int -> Int -> ([Transition], Int)
make Null m n k         = ([(m, n, Eps)], k)
make (Term c) m n k     = ([(m, n, C c)], k)
make (Seq re re') m n k = ((k, k + 1, Eps) : trans ++ trans', k'')
  where
    (trans, k')   = make re m k (k + 2)
    (trans', k'') = make re' (k + 1) n k'
make (Alt re re') m n k = (bases ++ trans ++ trans', k'')
  where
    bases         = [ (m, k, Eps), (k + 1, n, Eps)
                    , (m, k + 2, Eps), (k + 3, n, Eps) ]
    (trans, k')   = make re k (k + 1) (k + 4)
    (trans', k'') = make re' (k + 2) (k + 3) k'
make (Rep re) m n k     = (bases ++ trans, k')
  where
    bases      = [(m, k, Eps), (k + 1, n, Eps), (k + 1, k, Eps), (m, n, Eps)]
    (trans, k') = make re k (k + 1) (k + 2)

--------------------------------------------------------
-- Part IV

type MetaState = [State]

type MetaTransition = (MetaState, MetaState, Label)

getFrontier :: State -> Automaton -> [Transition]
getFrontier = (nub .) . worker
  where
    worker s au
      | s `elem` terminalStates au = [(s, s, Eps)]
      | otherwise                  = nes ++ frontiers
      where
        (es, nes) = partition (\(_, _, l) -> l == Eps) $ transitionsFrom s au
        frontiers = concatMap (\(_, s', _) -> worker s' au) es

groupTransitions :: [Transition] -> [(Label, [State])]
groupTransitions trans = M.assocs $ foldl worker initGroups trans
  where
    worker groups (_, _, Eps) = groups
    worker groups (_, s', l)  = M.adjust (s' :) l groups
    initGroups                = M.fromList $ map (, []) (labels trans)

makeDA :: Automaton -> Automaton
-- Pre: Any cycle in the NDA must include at least one non-Eps transition
makeDA au@(s, ts, _) = ( states M.! transStarts initTrans
                       , terminals, S.evalState (worker initTrans) M.empty )
  where
    -- A meta-state is terminal if and only if it contains a terminal state.
    -- Here, $M.assocs states$ is a list of (meta-state, index) pairs.
    terminals       = snd <$> filter (\(ms, _) -> and [t `elem` ms | t <- ts])
                                     (M.assocs states)
    initTrans       = getFrontier s au
    (trans, states) = S.runState (worker initTrans) M.empty
    -- The workhorse to compute the transitions as well as the state indices.
    worker trs      = do
      let metaState = transStarts trs
      metaStates <- S.get -- Seen meta states
      if metaState `M.member` metaStates
        then pure [] -- Already seen, no need to compute
        else do
          S.put $ M.insert metaState (length metaStates + 1) metaStates
          let groups = groupTransitions trs
          curIndex <- S.gets (M.! metaState)
          -- Collect all subsequent meta-transitions.
          join <$> forM groups \(l, states) -> do
            let newTrs = nub $ concatMap (`getFrontier` au) states
            result   <- worker newTrs
            newIndex <- S.gets (M.! transStarts newTrs)
            pure $ (curIndex, newIndex, l) : result

--------------------------------------------------------
-- Test cases

reFigure, re1, re2, re3, re4, re5 :: RE
reFigure
  = Seq (Rep (Alt (Term 'a') (Term 'b'))) (Term 'c')
re1
  = Seq (Alt (Term 'x') (Term 'y')) (Alt (Term '1') (Term '2'))
re2
  = Seq (Term 'x') (Rep (Term '\''))
re3
  = Rep (Alt (Seq (Term 'a') (Term 'b')) (Term 'c'))
re4
  = Seq (Alt (Term 'a') Null) (Term 'a')
re5
  = Seq (Opt (Seq (Term 'a') (Term 'b'))) (Plus (Term 'd'))

nd, nd' :: Automaton
nd = (1,[4],[(1,2,C 'a'),(1,3,C 'b'),(2,3,Eps),(2,4,C 'c')])

nd' = (1,[4],[(1,2,Eps),(1,3,C 'a'),(2,4,C 'a'),(2,4,C 'b'),
              (3,4,C 'b'),(3,4,Eps)])

da :: Automaton
da = (0,[3],[(0,1,C 'a'),(0,2,C 'a'),(0,2,C 'b'),(1,2,C 'a'),
             (1,3,C 'b'),(2,2,C 'a'),(2,1,C 'a'),(2,3,C 'b')])

re :: RE
re = Seq (Alt (Term 'a') (Term 'b')) (Seq (Rep (Term 'a')) (Term 'b'))

ndaFigure, nda1, nda2, nda3, nda4, nda5 :: Automaton
daFigure, da1, da2, da3, da4, da5 :: Automaton
ndaFigure
  = (1,[2],[(1,3,Eps),(1,5,Eps),(3,4,Eps),(4,2,C 'c'),(5,7,Eps),
            (5,9,Eps),(6,3,Eps),(6,5,Eps),(7,8,C 'a'),(8,6,Eps),
            (9,10,C 'b'),(10,6,Eps)])
daFigure
  = (1,[2],
     [(1,1,C 'a'),(1,1,C 'b'),(1,2,C 'c')])

nda1
  = (1,[2],[(1,5,Eps),(1,7,Eps),(3,4,Eps),(4,9,Eps),(4,11,Eps),
            (5,6,C 'x'),(6,3,Eps),(7,8,C 'y'),(8,3,Eps),(9,10,C '1'),
            (10,2,Eps),(11,12,C '2'),(12,2,Eps)])
da1
  = (1,[3],
     [(1,2,C 'x'),(1,2,C 'y'),(2,3,C '1'),(2,3,C '2')])

nda2
  = (1,[2],[(1,3,C 'x'),(3,4,Eps),(4,2,Eps),(4,5,Eps),(5,6,C '\''),
            (6,2,Eps),(6,5,Eps)])
da2
  = (1,[2],
     [(1,2,C 'x'),(2,2,C '\'')])

nda3
  = (1,[2],[(1,2,Eps),(1,3,Eps),(3,5,Eps),(3,7,Eps),(4,2,Eps),
            (4,3,Eps), (5,9,C 'a'),(6,4,Eps),(7,8,C 'c'),(8,4,Eps),
            (9,10,Eps),(10,6,C 'b')])
da3
  = (1,[1],
     [(1,1,C 'c'),(1,2,C 'a'),(2,1,C 'b')])

nda4
  = (1,[2],[(1,5,Eps),(1,7,Eps),(3,4,Eps),(4,2,C 'a'),(5,6,C 'a'),
            (6,3,Eps),(7,8,Eps),(8,3,Eps)])
da4
  = (1,[2,3],[(1,2,C 'a'),(2,3,C 'a')])

nda5
  = (1,[2],[(1,5,Eps),(1,7,Eps),(3,4,Eps),(4,11,C 'd'),(5,9,C 'a'),
            (6,3,Eps),(7,8,Eps),(8,3,Eps),(9,10,Eps),(10,6,C 'b'),
            (11,12,Eps),(12,2,Eps),(12,13,Eps),(13,14,C 'd'),
            (14,2,Eps),(14,13,Eps)])
da5
  = (1,[2],[(1,2,C 'd'),(1,3,C 'a'),(2,2,C 'd'),(3,4,C 'b'),
            (4,2,C 'd')])

---------------------------------------------------------
-- Test & Helpers

tester :: IO ()
tester = runTest do
  let (.==.) = with EqAutomaton (==.)
  label "Test 'terminalStates'" do
    terminalStates nda1 ==. [2]
  label "Test 'isTerminal'" do
    isTerminal 2 nda1 ==. True
    isTerminal 3 da4 ==. True
    isTerminal 5 nda2 ==. False
  label "Test 'transitionsFrom'" do
    transitionsFrom 5 ndaFigure ==. [(5, 7, Eps), (5, 9, Eps)]
    transitionsFrom 10 nda3 ==. [(10, 6, C 'b')]
  label "Test 'labels'" do
    sort (labels [(1, 2, Eps)]) ==. []
    sort (labels (transitions nda3)) ==. [C 'a', C 'b', C 'c']
  label "Test 'accepts'" do
    accepts nda1 "x1" ==. True
    accepts nda1 "y1" ==. True
    accepts nda1 "x2" ==. True
    accepts nda1 "y2" ==. True
    accepts nda1 "x" ==. False
    accepts nda1 "y" ==. False
    accepts nda1 "1" ==. False
    accepts nda1 "2" ==. False
    accepts nda2 "x" ==. True
    accepts nda2 "x'" ==. True
    accepts nda2 "x''" ==. True
    accepts nda2 "x," ==. False
    accepts nda3 "" ==. True
    accepts nda3 "ab" ==. True
    accepts nda3 "c" ==. True
    accepts nda3 "abc" ==. True
    accepts nda3 "abab" ==. True
    accepts nda3 "abcab" ==. True
    accepts nda3 "cab" ==. True
    accepts nda3 "ac" ==. False
    accepts nda3 "bc" ==. False
  label "Test 'makeNDA'" do
    makeNDA re1 ==. nda1
    makeNDA re2 ==. nda2
    makeNDA re3 ==. nda3
    makeNDA re4 ==. nda4
    makeNDA re5 ==. nda5
  label "Test 'makeDA'" do
    makeDA nda1 .==. da1
    makeDA nda2 .==. da2
    makeDA nda3 .==. da3
    makeDA nda4 .==. da4
    makeDA nda5 .==. da5

newtype EqAutomaton = EqAutomaton { unEQ :: Automaton }

instance Eq EqAutomaton where
  (==) :: EqAutomaton -> EqAutomaton -> Bool
  (==) a1@(EqAutomaton (s1, ts1, trs1)) a2@(EqAutomaton (s2, ts2, trs2))
    = length ts1 == length ts2 && length trs1 == length trs2
   && S.evalState (worker trs1 trs2) (IM.singleton s1 s2)
    where
      worker [] _                     = do
        mapping <- S.get
        pure $ case mapM (mapping IM.!?) ts1 of
          Nothing -> False
          Just ts -> sort ts == sort ts2
      worker ((s1, s2, l) : trs) trs' = do
        mapping <- S.get
        or <$> forMContinue trs' \tr'@(s1', s2', l') -> do
          guard $ l == l'
          lift $ S.put mapping
          case mapping IM.!? s1 of
            Just img1 -> guard $ img1 == s1'
            Nothing   -> lift $ S.modify (IM.insert s1 s1')
          case mapping IM.!? s2 of
            Just img2 -> guard $ img2 == s2'
            Nothing   -> lift $ S.modify (IM.insert s2 s2')
          lift $ worker trs (trs' \\ [tr'])

instance Show EqAutomaton where
  show :: EqAutomaton -> String
  show = show . unEQ

forMContinue :: Monad m => [a] -> (a -> MaybeT m b) -> m [b]
forMContinue [] _ = pure []
forMContinue (x : xs) f = do
  r <- runMaybeT $ f x
  case r of
    Nothing -> forMContinue xs f
    Just y  -> (y :) <$> forMContinue xs f
