{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

-- > Just as Tony commented, This year's test is indeed one of the hardest
-- > tests in the history of the course, and I would imagine not doing it too
-- > well had I taken it. Fortunately, I have been able to learn a lot since my
-- > first year, and I was quite familiar with the ideas presented in this test.
-- >
-- > Some of functions (especially the ones in Part III) are quite tricky, but
-- > the formulation suits well with RWS monads. In fact, those monads make the
-- > solution easier as long as one understands how to use them.

module Year2022.SC where

import Control.Monad
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Writer
import Data.Bifunctor
import Data.List
import Data.Maybe

import Test
import Year2022.Examples
import Year2022.Types

---------------------------------------------------------

prims :: [Id]
prims
  = ["+", "-", "*", "<=", "ite"]

lookUp :: Id -> [(Id, a)] -> a
lookUp v env
  = fromMaybe (error ("lookUp failed with search key " ++ v))
              (lookup v env)

---------------------------------------------------------
-- Part I

isFun :: Exp -> Bool
isFun Fun {} = True
isFun _      = False

splitDefs :: [Binding] -> ([Binding], [Binding])
splitDefs = partition (isFun . snd)

topLevelFunctions :: Exp -> Int
topLevelFunctions (Let bs _) = length $ filter (isFun . snd) bs
topLevelFunctions _          = 0

---------------------------------------------------------
-- Part II

unionAll :: Eq a => [[a]] -> [a]
unionAll = foldl' union []

freeVars :: Exp -> [Id]
freeVars (Const _)    = []
freeVars (Var v)
  | v `elem` prims = []
  | otherwise      = [v]
freeVars (App fun es) = unionAll $ map freeVars (fun : es)
freeVars (Fun as e)   = freeVars e \\ as
freeVars (Let bs e)   = unionAll (map freeVars $ e : map snd bs) \\ map fst bs

---------------------------------------------------------
-- Part III

-- Given...
lambdaLift :: Exp -> Exp
lambdaLift e
  = lift (modifyFunctions (buildFVMap e) e)

buildFVMap :: Exp -> [(Id, [Id])]
buildFVMap (Const _)    = []
buildFVMap (Var _)      = []
buildFVMap (App fun es) = unionAll $ map buildFVMap (fun : es)
buildFVMap (Fun _ e)    = buildFVMap e
buildFVMap (Let bs e)   = buildFVMap e
                  `union` unionAll (map (buildFVMap . snd) bs)
                  `union` map (second (\\ map fst fDefs)) (iter fFreeVars)
  where
    (fDefs, _) = splitDefs bs
    fDefCount  = length fDefs
    fFreeVars  = second freeVars <$> fDefs
    -- > The union of free variables of recursively referenced functions.
    -- > The sorting is optional. Here I sorted so that it would match exactly
    -- > with the test examples.
    iter ffvs  = second sort <$> iterate (map (second expand)) ffvs !! fDefCount
    expand fvs = unionAll (fvs : map (fromMaybe [] . (`lookup` fFreeVars)) fvs)

modifyFunctions :: [(Id, [Id])] -> Exp -> Exp
-- Pre: The mapping table contains a binding for every function
-- named in the expression.
-- > It's a straightforward application of the rules defined in the speç.
-- > The reader monad is used to pass the mapping table around.
modifyFunctions = (. worker) . flip runReader
  where
    worker e@(Var v)    = do
      subs <- ask
      pure $ case lookup v subs of
        Nothing  -> e -- > Not function
        Just []  -> Var $ '$' : v -- > No free variables
        Just fvs -> App (Var $ '$' : v) (map Var fvs) -- > Has free variables
    worker (App fun es) = liftM2 App (worker fun) (mapM worker es)
    worker (Let bs e)   = do
      -- > bs' is the traversed bindings.
      bs' <- forM bs \case
        (fun, Fun as e') -> do
          -- > For each function binding, traverse the function definition
          -- > and collect the free variables.
          subs <- ask
          e''  <- worker e'
          pure ('$' : fun, Fun (lookUp fun subs ++ as) e'')
        (var, e')        -> (var ,) <$> worker e' -- > Traverse the definition
      Let bs' <$> worker e
    worker e            = pure e

-- The default definition here is id.
-- If you implement the above two functions but not this one
-- then lambdaLift above will remove all the free variables
-- in functions; it just won't do any lifting.
lift :: Exp -> Exp
lift e = case scs of
  [] -> e'
  _  -> Let scs e'
  where
    (e', scs) = lift' e

-- You may wish to use this...
lift' :: Exp -> (Exp, [Supercombinator])
-- > Again, the sortings are not necessary and are included only for the tests.
lift' = second (sortOn fst) . runWriter . worker
  where
    worker (Let bs e)   = do
      -- > Recursively traverse the binding definitions.
      bs' <- forM bs $ \(v, e') -> (v, ) <$> worker e'
      -- > Split into supercombinators and others (non-function bindings).
      let (scs, others) = partition (isFun . snd) bs'
      tell scs -- > Add the supercombinators to the writer
      case others of
        [] -> worker e -- > No non-function bindings
        _  -> Let (sortOn fst others) <$> worker e -- > retain other bindings
    worker (App fun es) = liftM2 App (worker fun) (mapM worker es)
    worker (Fun as e)   = Fun as <$> worker e
    worker e            = pure e

---------------------------------------------------------
-- Test & Helpers

tester :: IO ()
tester = runTest do
  let (.==.) = with ShowExp (==.)
  label "Test 'isFun'" do
    isFun e1 ==. False
    isFun e2 ==. False
    isFun e3 ==. False
    isFun e4 ==. False
    isFun e5 ==. True
    isFun e6 ==. False
    isFun e7 ==. False
    isFun e8 ==. True
    isFun e9 ==. False
    isFun e10 ==. False
    isFun e11 ==. False
    isFun e12 ==. False
    isFun e13 ==. False
    isFun e14 ==. False
    isFun e15 ==. False
    isFun e16 ==. False
    isFun e18 ==. False
    isFun e19 ==. False
    isFun e20 ==. False
    isFun e21 ==. False
  label "Test 'modifyFunctions'" $ modifyFunctions (buildFVMap e1) e1 ==. e1''
  label "Test 'lambdaLift'" do
    lambdaLift e1 .==. e1'
    lambdaLift e2 .==. e2'
    lambdaLift e4 .==. e4'
    lambdaLift e12 .==. e12'
    lambdaLift e13 .==. e13'
    lambdaLift e14 .==. e14'
    lambdaLift e15 .==. e15'
    lambdaLift e16 .==. e16'
    lambdaLift e18 .==. e18'
    lambdaLift e19 .==. e19'

newtype ShowExp = ShowExp { unShow :: Exp }
  deriving Eq

instance Show ShowExp where
  show = showE . unShow
