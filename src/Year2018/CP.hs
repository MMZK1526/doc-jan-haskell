{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TupleSections #-}

module Year2018.CP where

-- > Despite been a 2-star test, I feel that this test deserves a 3-star if you
-- > consider the bonus part (Part 4). It is quite tricky to get SSA generation
-- > right and handle all the edge cases correctly and gracefully, something
-- > that would be a miracle if you can get it right in the exam (I for one
-- > didn't manage to do it in time).
-- >
-- > Assuming you are in Year 1, this test is definitely helpful for your WACC
-- > project next year as you could be dealing with SSA form and constant
-- > propagation. As far as I remember, our group were using live variable
-- > analysis for constant propagation, which incidentally corresponds to the
-- > test in 2021 -- the one of my year.
-- >
-- > A final note on Part 4: there is a reason that the speç does not provide
-- > any test cases for SSA generation. This is because there are more than one
-- > correct answers for the same input. For instance, you may name the temp
-- > variables differently, and you might have different orderings when breaking
-- > down a nested expression. As a matter of fact, the SSA examples provided in
-- > this module also does a little bit of static analysis on unreachable code,
-- > something that I did not do in my implementation.

import Data.Maybe
import Data.List

import Control.Monad
import qualified Control.Monad.Trans.State as S
import Data.Tuple
import Test

type Id = String

type Function = (Id, [Id], Block)

type Block = [Statement]

data Statement = Assign Id Exp |
                 If Exp Block Block |
                 DoWhile Block Exp
               deriving (Eq, Show)

data Exp = Const Int | Var Id | Apply Op Exp Exp | Phi Exp Exp
         deriving (Eq, Show)

data Op = Add | Mul | Eq | Gtr
        deriving (Eq, Show)

------------------------------------------------------------------------
-- Given functions to support the interpreter...

lookUp :: (Eq a, Show a) => a -> [(a, b)] -> b
lookUp i table
  = fromMaybe (error ("lookup failed on identifier: " ++ show i))
              (lookup i table)

execFun :: Function -> [Int] -> State
execFun (v', args, p) vs
  = execBlock p (zip args vs)

------------------------------------------------------------------------
-- Part I

type State = [(Id, Int)]

update :: (Id, Int) -> State -> State
update (id, v) [] = [(id, v)]
update (id, v) ((id', v') : s)
  | id == id' = (id, v) : s
  | otherwise = (id', v') : update (id, v) s

apply :: Op -> Int -> Int -> Int
apply Add = (+)
apply Mul = (*)
apply Eq  = (fromEnum .) . (==)
apply Gtr = (fromEnum .) . (>)

eval :: Exp -> State -> Int
-- Pre: the variables in the expression will all be bound in the given state
-- Pre: expressions do not contain phi instructions
eval (Const i) _       = i
eval (Var id) s        = lookUp id s
eval (Apply op e e') s = apply op (eval e s) (eval e' s)

execStatement :: Statement -> State -> State
execStatement (Assign v e) s = update (v, eval e s) s
execStatement (If e b b') s
  | eval e s /= 0 = execBlock b s
  | otherwise     = execBlock b' s
execStatement (DoWhile b e) s
  | eval e s' /= 0 = execStatement (DoWhile b e) s'
  | otherwise      = s'
  where
    s' = execBlock b s

execBlock :: Block -> State -> State
execBlock b s = foldl (flip execStatement) s b

------------------------------------------------------------------------
-- Given function for testing propagateConstants...

-- Converts a function in SSA form into its optimised SSA form...
applyPropagate :: Function -> Function
applyPropagate (v', args, body)
  = (v', args, propagateConstants body)

------------------------------------------------------------------------
-- PART II

foldConst :: Exp -> Exp
-- Pre: the expression is in SSA form
foldConst (Phi e e')      = case (foldConst e, foldConst e') of
  (Const i, Const i') -> if i == i' then Const i else Phi e e'
  (e, e')             -> Phi e e'
foldConst (Apply op e e') = case (op, foldConst e, foldConst e') of
  (Add, e, Const 0)      -> e
  (Add, Const 0, e)      -> e
  (_, Const i, Const i') -> Const (apply op i i')
  (_, e, e')             -> Apply op e e'
foldConst e               = e

sub :: Id -> Int -> Exp -> Exp
-- Pre: the expression is in SSA form
sub v i e = foldConst $ sub' e
  where
    sub' (Var v') | v == v' = Const i
    sub' (Apply op e e')    = Apply op (sub' e) (sub' e')
    sub' (Phi e e')         = Phi (sub' e) (sub' e')
    sub' e                  = e

-- Use (by uncommenting) any of the following, as you see fit...
-- type Worklist = [(Id, Int)]
-- scan :: Id -> Int -> Block -> (Worklist, Block)
scan :: (Exp -> Exp) -> Block -> (Exp -> Exp, Block)
-- > I used the second approach, namely keeping track of a "transformation
-- > function" and use composition to update it.
-- >
-- > At each scan, we first apply the transformation function to the block,
-- > then we compute the new transformation by visiting each statement in the
-- > block. If the statement is an assignment, we update the transformation
-- > via composition. Other cases are handled recursively.
-- >
-- > It is probably not the most efficient solution since at every invocation
-- > of "scan" we sould have to use the whole transformation function all over
-- > again. However it is in my opinion a more elegant solution than the first
-- > one, which is not only more imperative in nature but also requires using
-- > a dummy invalid variable for the first pass.
scan subFun b = swap $ S.runState (modifyBlock $ subBlock subFun b) subFun
  where
    subBlock                = map . subStmt
    subStmt f (Assign v e)  = Assign v (f e)
    subStmt f (If e b b')   = If (f e) (subBlock f b) (subBlock f b')
    subStmt f (DoWhile b e) = DoWhile (subBlock f b) (f e)
    modifyBlock             = fmap concat . mapM modifyStmt
    modifyStmt b            = case b of
      Assign v e  -> case e of
        -- The constant ssignment is retained if it's a return statement.
        Const i -> [Assign v (Const i) | v == "$return"] <$ S.modify (sub v i .)
        _       -> pure [Assign v e]
      If e b b'   -> pure <$> liftM2 (If e) (modifyBlock b) (modifyBlock b')
      DoWhile b e -> pure <$> liftM2 DoWhile (modifyBlock b) (pure e)

propagateConstants :: Block -> Block
-- Pre: the block is in SSA form
-- > Keep calling "scan" until we reach a fixed point.
propagateConstants b = iter (scan foldConst b)
  where
    iter (f, b)
      | b' == b   = b
      | otherwise = iter (scan f' b')
      where
        (f', b') = scan f b

------------------------------------------------------------------------
-- Given functions for testing unPhi...

-- Applies unPhi to a given function...
applyUnPhi :: Function -> Function
applyUnPhi (name, args, body)
  = (name, args, unPhi body)

-- Combines propagation/folding and unPhi to convert a function from its
-- unoptimised SSA form to a final non-SSA form...
optimise :: Function -> Function
optimise (v', args, body)
  = (v', args, unPhi (propagateConstants body))

------------------------------------------------------------------------
-- PART III

unPhi :: Block -> Block
-- Pre: the block is in SSA form
unPhi = unPhi' . map unPhiStmt
  where
    unPhiStmt (If e b b')   = If e (unPhi b) (unPhi b')
    unPhiStmt (DoWhile b e) = DoWhile (unPhi b) e
    unPhiStmt e             = e
    unPhi' (If e b b' : Assign v (Phi e1 e2) : b'')
      = unPhi' $ If e (b ++ [Assign v e1]) (b' ++ [Assign v e2]) : b''
    unPhi' (DoWhile (Assign v (Phi e1 e2) : b) e : bs)
      = Assign v e1 : unPhi' (DoWhile (b ++ [Assign v e2]) e : bs)
    unPhi' (b : bs)         = b : unPhi' bs
    unPhi' []               = []

------------------------------------------------------------------------
-- Part IV

-- > We assume that all original variables does not contain digits as we are
-- > going to use digital suffixes to represent different versions of the
-- > variables.
-- >
-- > When splitting nested expressions, we use the variable "$temp" to store
-- > the intermediate results.
makeSSA :: Function -> Function
makeSSA (v', args, b)
  = (v', args, S.evalState (makeSSABlock b) ((, -1) <$> "$temp" : getLV b))

-- > Grab all the variables in a block that has appeared on the left-hand side
-- of an assignment statement.
getLV :: Block -> [Id]
getLV = nub . concatMap getLVStmt
  where
    getLVStmt (Assign v _)  = [v]
    getLVStmt (If _ b b')   = getLV b ++ getLV b'
    getLVStmt (DoWhile b _) = getLV b

-- > Helper function that converts a block to SSA form.
makeSSABlock :: Block -> S.State State Block
makeSSABlock = fmap concat . mapM makeSSAStmt

-- > Helper function that converts a statement to SSA form.
makeSSAStmt :: Statement -> S.State State [Statement]
makeSSAStmt (Assign v e) = makeSSAExp (v /= "$return") v e
makeSSAStmt (If e b b')  = do
  let lv = getLV b ++ getLV b'
  -- > If the conditional expression is split into multiple statements, we keep
  -- > the last statement as the conditional expression and the previous ones
  -- > as assignment statements before the conditional.
  (e', es) <- makeSSAExp' e
  st       <- S.get
  b1       <- makeSSABlock b
  st1      <- S.get
  b2       <- makeSSABlock b'
  st2      <- S.get
  -- > Generate a list of phi statements by traversing all the left values in
  -- > the branches and see if they are updated in at least one of the branches.
  phis <- fmap concat <$> forM lv
    $ \v -> genPhi v (lookUp v st) (lookUp v st1) (lookUp v st2)
  pure $ es ++ If e' b1 b2 : phis
  where
    genPhi v ix ix1 ix2
      -- > When the two branches generates different versions of the variable.
      | ix1 /= ix2 = do
        v' <- getName v
        pure [Assign v' (Phi (Var (v ++ show ix1)) (Var (v ++ show ix2)))]
      -- > When the two branches generates a new of the variable.
      -- > This only happens when the variable is used only in the then-branch,
      -- > In this case, the first PHI value corresponds to the then-branch and
      -- > the second corresponds to the variable before the if-statement.
      | ix1 /= ix  = do
        v' <- getName v
        pure [Assign v' (Phi (Var (v ++ show ix1)) (Var (v ++ show ix)))]
      -- > When no new version of the variable is generated.
      | otherwise  = pure []
makeSSAStmt (DoWhile b e) = do
  let lv = getLV b
  st       <- S.get
  phiVs    <- mapM getName lv
  b'       <- makeSSABlock b
  st'      <- S.get
  -- > Similar to the if-case, we need to split down the conditional expression
  -- > into multiple statements. They will be appended to the loop body.
  (e', es) <- makeSSAExp' e
  let genPhi v v' = Assign v' ( Phi (Var (v ++ show (lookUp v st)))
                                    (Var (v ++ show (lookUp v st'))) )
  -- > Generate the phi statements for the loop which will be prepended to the
  -- > loop body.
  let phis = zipWith genPhi lv phiVs
  pure [DoWhile (phis ++ b' ++ es) e']

-- > Helper function that converts a nested expression to SSA form. It takes
-- > a boolean field indicate if we want to change the name of the left value.
-- >
-- > For example, if the assignment corresponds to a return statement, then we
-- > would not want to change the name of "$return".
makeSSAExp :: Bool -> Id -> Exp -> S.State State [Statement]
makeSSAExp isRenaming v expr = do
  (e', b) <- makeSSAExp' expr
  v'      <- if isRenaming then getName v else pure v
  -- > There are more efficient alternatives to the list concatenation here.
  pure $ b ++ [Assign v' e']

-- > Similar to "makeSSAExp", but it retains the unassigned last expression.
makeSSAExp' :: Exp -> S.State State (Exp, [Statement])
makeSSAExp' expr = do
  oldState <- S.get
  -- > Name the variables in the expression with the latest version.
  let tag (Var v')        = Var (v' ++ maybe "" show (lookup v' oldState))
      tag (Apply op e e') = Apply op (tag e) (tag e')
      tag e               = e
  flatternExp (tag expr)
  where
    -- Split nested expressions into multiple small assignments.
    flatternExp e | isExpAtom e = pure (e, []) -- Already simple enough
    flatternExp (Apply op e e') = case (isExpAtom e, isExpAtom e') of
      (True, True)  -> pure (Apply op e e', []) -- Already simple enough
      (False, True) -> do
        -- Create temp variable to store the result of the first expression.
        s1 <- makeSSAExp True "$temp" e
        v1 <- ("$temp" ++) . show <$> S.gets (lookUp "$temp")
        pure (Apply op (Var v1) e', s1)
      (True, False) -> do
        -- Create temp variable to store the result of the second expression.
        s2 <- makeSSAExp True "$temp" e'
        v2 <- ("$temp" ++) . show <$> S.gets (lookUp "$temp")
        pure (Apply op e (Var v2), s2)
      (_, _)        -> do
        -- Create temp variables to store the result of the two expressions.
        s1 <- makeSSAExp True "$temp" e
        v1 <- ("$temp" ++) . show <$> S.gets (lookUp "$temp")
        s2 <- makeSSAExp True "$temp" e'
        v2 <- ("$temp" ++) . show <$> S.gets (lookUp "$temp")
        pure (Apply op (Var v1) (Var v2), s1 ++ s2)

-- > Increment the variable version and return the latest variable v'.
getName :: String -> S.State [(String, Int)] String
getName v         = do
  mIx <- S.gets (lookup v)
  case mIx of
    Nothing -> pure v
    Just ix -> do
      S.modify (update (v, ix + 1))
      pure (v ++ show (ix + 1))

-- > Checks if an expression is a constant or a variable.
isExpAtom :: Exp -> Bool
isExpAtom (Const _) = True
isExpAtom (Var _)   = True
isExpAtom _         = False

------------------------------------------------------------------------
-- Predefined functions for displaying functions and blocks...

opNames
  = [(Add, "+"), (Mul, "*"), (Eq, "=="), (Gtr, ">")]

precTable
  = [(Add, 1), (Mul, 2), (Eq, 0), (Gtr, 0)]

prec op
  = lookUp op precTable

showArgs []
  = ""
showArgs as
  = foldr1 (\a s -> a ++ (", " ++ s)) as

showExp :: Int -> Exp -> String
showExp _ (Const n)
  = show n
showExp _ (Var id)
  = id
showExp n (Apply op' e e')
  | n > n'    = "(" ++ s ++ ")"
  | otherwise = s
  where
    n' = prec op'
    s = showExp n' e ++ " " ++ fromJust (lookup op' opNames ) ++ " " ++
        showExp n' e'
showExp _ (Phi e e')
  = "PHI(" ++ showArgs (map (showExp 0) [e, e']) ++ ")"

showLine s n k
  =  putStrLn (show n ++ ": " ++ replicate (k + 2 - length (show n)) ' ' ++ s)

showBlock' b n
  = showBlock'' b n 2
  where
    showBlock'' :: Block -> Int -> Int -> IO Int
    showBlock'' [] n k
      = return n
    showBlock'' (s : b) n k
      = do n'  <- showStatement s n k
           n'' <- showBlock'' b n' k
           return n''
    showStatement (Assign id e) n k
      = do showLine (id ++ " = " ++ showExp 0 e) n k
           return (n + 1)
    showStatement (If p q []) n k
      = do showLine ("if " ++ "(" ++ showExp 0 p ++ ") {") n k
           n' <- showBlock'' q (n + 1) (k + 2)
           showLine "}" n' k
           return (n' + 1)
    showStatement (If p q r) n k
      = do showLine ("if " ++ "(" ++ showExp 0 p ++ ") {") n k
           n'  <- showBlock'' q (n + 1) (k + 2)
           showLine "} else {" n' k
           n'' <- showBlock'' r (n' + 1) (k + 2)
           showLine "}" n'' k
           return (n'' + 1)
    showStatement (DoWhile b p) n k
      = do showLine "do {" n k
           n' <- showBlock'' b (n + 1) (k + 2)
           showLine ("} while " ++ showExp 9 p) n' k
           return (n' + 1)

showFun :: Function -> IO()
showFun (v', args, body)
  = do putStrLn ("1:  " ++ v' ++ "(" ++ showArgs args ++ ") {")
       n <- showBlock' body 2
       showLine "}" n 0

showBlock ::  Block -> IO()
showBlock b
  = do n <- showBlock' b 1
       return ()

------------------------------------------------------------------------
-- Example state and expressions for testing...

s1 :: State
s1 = [("x", 7), ("y", 8)]

e1, e2, e3, e4, e5 :: Exp
e1 = Var "x"
e2 = Apply Mul (Apply Add (Var "x") (Const 1)) (Var "y")
e3 = Phi (Const 2) (Const 2)
e4 = Apply Add (Const 0) (Var "x")
e5 = Apply Add (Var "a") (Var "x")

------------------------------------------------------------------------
-- Example functions...

-- Figure 1...
example :: Function
example
  = ("example",["x"],[Assign "a" (Const 1),Assign "b" (Apply Add (Var "x")
    (Const 2)),Assign "c" (Const 3),If (Apply Eq (Var "x") (Const 10))
    [Assign "a" (Const 1),Assign "c" (Const 5)] [],Assign "d"
    (Apply Add (Var "a") (Const 3)),Assign "e" (Apply Add (Var "d") (Var "b")),
    Assign "$return" (Apply Add (Var "e") (Var "c"))])

exampleSSA :: Function
exampleSSA
  = ("example",["x"],[Assign "a0" (Const 1),Assign "b0" (Apply Add (Var "x")
    (Const 2)),Assign "c0" (Const 3),If (Apply Eq (Var "x") (Const 10)) [Assign
    "a1" (Const 1),Assign "c1" (Const 5)] [],Assign "a2" (Phi (Var "a1") (Var
    "a0")),Assign "c2" (Phi (Var "c1") (Var "c0")),Assign "d0" (Apply Add (Var
    "a2") (Const 3)),Assign "e0" (Apply Add (Var "d0") (Var "b0")),
    Assign "$return" (Apply Add (Var "e0") (Var "c2"))])

exampleSSAPropagated :: Function
exampleSSAPropagated
  = ("example",["x"],[Assign "b0" (Apply Add (Var "x") (Const 2)),If (Apply Eq
    (Var "x") (Const 10)) [] [],Assign "c2" (Phi (Const 5) (Const 3)),
    Assign "e0" (Apply Add (Const 4) (Var "b0")),Assign "$return"
    (Apply Add (Var "e0") (Var "c2"))])

exampleOptimised :: Function
exampleOptimised
  = ("example",["x"],[Assign "b0" (Apply Add (Var "x") (Const 2)),If (Apply Eq
    (Var "x") (Const 10)) [Assign "c2" (Const 5)] [Assign "c2" (Const 3)],Assign
    "e0" (Apply Add (Const 4) (Var "b0")),Assign "$return" (Apply Add (Var "e0")
    (Var "c2"))])


-- Figure 2 (there is no SSA version of this)...
fact :: Function
fact
  = ("fact",
     ["n"],
     [If (Apply Eq (Var "n") (Const 0))
        [Assign "$return" (Const 1)]
        [Assign "prod" (Const 1),
         Assign "i" (Var "n"),
         DoWhile
           [Assign "prod" (Apply Mul (Var "prod") (Var "i")),
            Assign "i" (Apply Add (Var "i") (Const (-1)))
           ]
           (Apply Gtr (Var "i") (Const 0)),
         Assign "$return" (Var "prod")
        ]
     ]
    )


-- Summation loop, specialised loop for the case k=0...
loop :: Function
loop
  = ("loop",["n"],[Assign "i" (Var "n"),Assign "k" (Const 0),Assign "sum"
    (Const 0),If (Apply Eq (Var "i") (Const 0)) [Assign "$return" (Const 0)]
    [DoWhile [Assign "sum" (Apply Add (Var "sum") (Apply Mul (Apply Add
    (Var "i") (Apply Mul (Const 2) (Var "k"))) (Apply Add (Apply Add (Var "i")
    (Apply Mul (Const 2) (Var "k"))) (Const 1)))),Assign "i" (Apply Add
    (Var "i") (Const (-1)))] (Apply Gtr (Var "i") (Apply Add (Const 0) (Const 0))),
    Assign "$return" (Var "sum")]])

loopSSA :: Function
loopSSA
  = ("loop",["n"],[Assign "i0" (Var "n"),Assign "k0" (Const 0),Assign "sum0"
    (Const 0),If (Apply Eq (Var "i0") (Const 0)) [Assign "$return" (Const 0)]
    [DoWhile [Assign "sum1" (Phi (Var "sum0") (Var "sum2")),Assign "i1"
    (Phi (Var "i0") (Var "i2")),Assign "k1" (Apply Mul (Var "k0") (Const 2)),
    Assign "a0" (Apply Add (Var "i1") (Var "k1")),Assign "k2" (Apply Mul
    (Var "k0") (Const 2)),Assign "b0" (Apply Add (Var "k2") (Const 1)),
    Assign "b1" (Apply Add (Var "i1") (Var "b0")),Assign "m0" (Apply Mul
    (Var "a0") (Var "b1")),Assign "sum2" (Apply Add (Var "sum1") (Var "m0")),
    Assign "i2" (Apply Add (Var "i1") (Const (-1)))] (Apply Gtr (Var "i2")
    (Const 0)),Assign "$return" (Var "sum2")]])

loopSSAPropagated :: Function
loopSSAPropagated
  = ("loop",["n"],[Assign "i0" (Var "n"),If (Apply Eq (Var "i0") (Const 0))
    [Assign "$return" (Const 0)] [DoWhile [Assign "sum1" (Phi (Const 0) (Var
    "sum2")),Assign "i1" (Phi (Var "i0") (Var "i2")),Assign "a0" (Var "i1"),
    Assign "b1" (Apply Add (Var "i1") (Const 1)),Assign "m0" (Apply Mul
    (Var "a0") (Var "b1")),Assign "sum2" (Apply Add (Var "sum1") (Var "m0")),
    Assign "i2" (Apply Add (Var "i1") (Const (-1)))] (Apply Gtr (Var "i2")
    (Const 0)),Assign "$return" (Var "sum2")]])

loopOptimised :: Function
loopOptimised
  = ("loop",["n"],[Assign "i0" (Var "n"),If (Apply Eq (Var "i0") (Const 0))
    [Assign "$return" (Const 0)] [Assign "sum1" (Const 0),Assign "i1" (Var
    "i0"),DoWhile [Assign "a0" (Var "i1"),Assign "b1" (Apply Add (Var "i1")
    (Const 1)),Assign "m0" (Apply Mul (Var "a0") (Var "b1")),Assign "sum2"
    (Apply Add (Var "sum1") (Var "m0")),Assign "i2" (Apply Add (Var "i1")
    (Const (-1))),Assign "sum1" (Var "sum2"),Assign "i1" (Var "i2")]
    (Apply Gtr (Var "i2") (Const 0)),Assign "$return" (Var "sum2")]])


-- Basic block (no conditionals or loops)...
basicBlock :: Function
basicBlock
  = ("basicBlock",[],[Assign "x" (Const 1),Assign "y" (Const 2),Assign "x"
    (Apply Add (Var "x") (Var "y")),Assign "y" (Apply Mul (Var "x") (Const
    3)),Assign "$return" (Var "y")])

basicBlockSSA :: Function
basicBlockSSA
  = ("basicBlock",[],[Assign "x0" (Const 1),Assign "y0" (Const 2),Assign "x1"
    (Apply Add (Var "x0") (Var "y0")),Assign "y1" (Apply Mul (Var "x1") (Const
    3)),Assign "$return" (Var "y1")])

basicBlockSSAPropagated :: Function
basicBlockSSAPropagated
  = ("basicBlock", [], [Assign "$return" (Const 9)])

-- (This is the same as above, as there were no phi functions.)
basicBlockOptimised :: Function
basicBlockOptimised
  = ("basicBlock", [], [Assign "$return" (Const 9)])

-- Computes the maximum of two integers; useful for testing unPhi...
max2 :: Function
max2
  = ("max2",["x","y"],[If (Apply Gtr (Var "x") (Var "y")) [Assign "m" (Var "x")]
    [Assign "m" (Var "y")],Assign "$return" (Var "m")])

max2SSA :: Function
max2SSA
  = ("max2",["x","y"],[If (Apply Gtr (Var "x") (Var "y")) [Assign "m0" (Var
    "x")] [Assign "m1" (Var "y")],Assign "m2" (Phi (Var "m0") (Var "m1")),Assign
    "$return" (Var "m2")])

max2SSAPropagated :: Function
max2SSAPropagated
  = ("max2",["x","y"],[If (Apply Gtr (Var "x") (Var "y")) [Assign "m0" (Var
    "x")] [Assign "m1" (Var "y")],Assign "m2" (Phi (Var "m0") (Var "m1")),Assign
    "$return" (Var "m2")])

max2Optimised :: Function
max2Optimised
  = ("max2",["x","y"],[If (Apply Gtr (Var "x") (Var "y")) [Assign "m0" (Var
    "x"),Assign "m2" (Var "m0")] [Assign "m1" (Var "y"),Assign "m2" (Var
    "m1")],Assign "$return" (Var "m2")])

---------------------------------------------------------
-- Test & Helpers

tester :: IO ()
tester = runTest do
  label "Test 'update'" do
    sort (update ("x", 3) s1) ==. [("x", 3), ("y", 8)]
    sort (update ("y", 3) s1) ==. [("x", 7), ("y", 3)]
    sort (update ("z", 0) s1) ==. [("x", 7), ("y", 8), ("z", 0)]
  label "Test 'apply" do
    apply Add 7 5 ==. 12
    apply Gtr 4 9 ==. 0
    apply Eq  4 4 ==. 1
  label "Test 'eval'" do
    eval e1 s1 ==. 7
    eval e2 s1 ==. 64
  label "Test 'execStatement' & 'execBlock" do
    sort (execFun fact [5]) ==. [("$return", 120), ("i", 0), ("n", 5), ("prod", 120)]
    sort (execFun loop [4]) ==. [("$return", 40), ("i", 0), ("k", 0), ("n", 4), ("sum", 40)]
    sort (execFun loopOptimised [4]) ==. [("$return", 40), ("a0", 1), ("b1", 2), ("i0", 4), ("i1", 0), ("i2", 0), ("m0", 2), ("n", 4), ("sum1", 40), ("sum2", 40)]
  label "Test 'foldConst'" do
    foldConst e3 ==. Const 2
    foldConst e4 ==. Var "x"
  label "Test 'sub'" do
     sub "a" 0 e5 ==. Var "x"
  label "Test constant propagation" do
    applyPropagate loopSSA ==. loopSSAPropagated
    applyPropagate exampleSSA ==. exampleSSAPropagated
    applyPropagate basicBlockSSA ==. basicBlockSSAPropagated
    applyPropagate max2SSA ==. max2SSAPropagated
  label "Test 'unPhi'" do
    applyUnPhi loopSSAPropagated ==. loopOptimised
    applyUnPhi exampleSSAPropagated ==. exampleOptimised
    applyUnPhi basicBlockSSAPropagated ==. basicBlockOptimised
    applyUnPhi max2SSAPropagated ==. max2Optimised
  label "Test 'optimise'" do
    optimise loopSSA ==. loopOptimised
    optimise exampleSSA ==. exampleOptimised
    optimise basicBlockSSA ==. basicBlockOptimised
    optimise max2SSA ==. max2Optimised
  label "Test 'makeSSA'" do
    isSSA (makeSSA loop) ==. True
    forM_ [[x] | x <- [0..9]] $ \args ->
      eta (optimise $ makeSSA loop) loopOptimised args ==. True
    isSSA (makeSSA example) ==. True
    forM_ [[x] | x <- [0..9]] $ \args ->
      eta (optimise $ makeSSA example) exampleOptimised args ==. True
    isSSA (makeSSA basicBlock) ==. True
    eta (optimise $ makeSSA basicBlock) basicBlockOptimised [] ==. True
    isSSA (makeSSA max2) ==. True
    forM_ [[x, y] | x <- [0..4], y <- [0..4]] $ \args ->
      eta (optimise $ makeSSA max2) max2Optimised args ==. True

-- > Check if the function is in SSA form.
-- >
-- > As explained in the title (TODO), there's more than one correct SSA
-- > representation for a given function. So for the test, we will just check
-- > if the function is in SSA form, but not if it is in the same SSA form as
-- > the examples.
isSSA :: Function -> Bool
isSSA (_, _, b) = S.evalState (ap (==) nub <$> (isSSABlock b >> S.get)) []
  where
    isSSABlock               = fmap and <$> mapM isSSAStmt
    isSSAStmt (Assign v e)   = do
      unless (head v == '$') $ S.modify (v :)
      isSSAExpr e
    isSSAStmt (If e b1 b2)
      = and <$> sequence [isSSAExpr e, isSSABlock b1, isSSABlock b2]
    isSSAStmt (DoWhile b e)  = and <$> sequence [isSSABlock b, isSSAExpr e]
    isSSAExpr (Var _)        = pure True
    isSSAExpr (Const _)      = pure True
    isSSAExpr (Apply _ e e') = pure $ isExpAtom e && isExpAtom e'
    isSSAExpr (Phi e e')     = pure $ isExpAtom e && isExpAtom e'

-- > Check if the two functions behave the same with the given arguments.
-- > Namesake from eta-equivalence.
-- >
-- > Although we could never be sure if two functions are equivalent (to be
-- > certain we would need to test all possible inputs), we can at least check
-- > if they behave the same with a given set of arguments.
eta :: Function -> Function -> [Int] -> Bool
eta f f' args
  = lookUp "$return" (execFun f args) == lookUp "$return" (execFun f' args)
