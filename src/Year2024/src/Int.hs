{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE ViewPatterns #-}

-- > This test can be considered as a continuation of a previous tutorial on
-- > differentiation. Of course, integration in general is not as
-- > straightforward. The test only focuses on one major technique, namely the
-- > reverse chain rule.
-- >
-- > In my opinion, this test is not difficult, but there are many edge cases
-- > and it is very easy to miss out on some of them, espcially under time
-- > pressure.
-- >
-- > In practice, of course, the integration rules covered in the test is only
-- > a tiny fraction, and it is in general a very hard problem to solve.

module Int where

import GHC.Real
import Data.List
import Data.Maybe
import Control.Applicative

import Types
import Utilities
import Examples

import Data.Bifunctor

--
-- Universal assumptions/preconditions:
-- 1. All polynomials are in standard form with decreasing
--    powers of x
-- 2. 0 is represented by P [(0, 0)]; P [] is undefined for
--    the purposes of the exercise.
-- 3. All constants will be polynomials of the form
--    [(c, 0)], e.g. logarithms of constants and constant
--    powers will not appear.
-- 4. All computed integrals omit the constant of integration.
--

-------------------------------------------------
-- Part I (13 marks)

addP :: Polynomial -> Polynomial -> Polynomial
addP p1@((c1,e1):r1) p2@((c2,e2):r2)
  | e1 > e2   = (c1, e1) : addP r1 p2
  | e1 < e2   = (c2, e2) : addP p1 r2
  | otherwise = (c1 + c2, e1) : addP r1 r2
addP p1 p2 = p1 ++ p2 -- > one of them is empty

mulP :: Polynomial -> Polynomial -> Polynomial
mulP p = sumP . map (\(c,e) -> map (bimap (c *) (e +)) p)

sumP :: [Polynomial] -> Polynomial
sumP = foldl' addP [(0, 0)]

prodP :: [Polynomial] -> Polynomial
prodP = foldl' mulP [(1, 0)]

diffT :: Term -> Term
diffT (c, 0) = (0, 0)
diffT (c, e) = (c * (e % 1), e - 1)

-- > The speç should specify the constant term to be zero!
intT :: Term -> Term
intT (0, 0) = (0, 0)
intT (c, e) = (c / (e % 1 + 1), e + 1)

diffP :: Polynomial -> Polynomial
diffP = map diffT

intP :: Polynomial -> Polynomial
intP = map intT

-------------------------------------------------
-- Part II (7 marks)

diffE :: Expr -> Expr
diffE (P p)       = P $ diffP p
diffE (Add e1 e2) = Add (diffE e1) (diffE e2)
diffE (Mul e1 e2) = Add (Mul (diffE e1) e2) (Mul e1 (diffE e2))
diffE (Pow e n)   = Mul (Mul (P [(n, 0)]) (Pow e (n - 1))) (diffE e)
diffE (Log e)     = Mul (Pow e (-1)) (diffE e)

--
-- Given
--
toExpr :: Rational -> Expr
toExpr n = P [(n, 0)]

isConstant :: Expr -> Bool
isConstant (P [(_, 0)]) = True
isConstant _ = False

simplifiedDiff :: Expr -> Expr
simplifiedDiff = simplify . diffE

printDiff :: Expr -> IO ()
printDiff = prettyPrint . simplifiedDiff

-------------------------------------------------
-- Part III (10 marks)

intE :: Expr -> Maybe Expr
intE (P p)       = Just $ P (intP p)
intE (Add e1 e2) = Add <$> intE e1 <*> intE e2
intE (Mul e1 e2)
  | isConstant e1 = Mul e1 <$> intE e2
  | isConstant e2 = Mul e2 <$> intE e1
  | otherwise     = applyICR e1 e2 <|> applyICR e2 e1 -- > try both ways
intE e           = applyICR e (toExpr 1) -- > the multiply-by-one trick

applyICR :: Expr -> Expr -> Maybe Expr
applyICR fg g' = case factorise g' (diffE fg) of -- > Try the case "f(x)f'(x)"
  Just coeff -> Just $ Mul (toExpr $ coeff / 2) (Pow fg 2)
  Nothing    -> case fg of
      Pow g n -> do
        coeff <- factorise g' (diffE g) -- > Try the case "g^n(x)g'(x)"
        pure $ case n of
          -1 -> Mul (toExpr coeff) (Log g)
          _  -> Mul (toExpr $ coeff / (n + 1)) (Pow g (n + 1))
      Log g   -> do
        coeff <- factorise g' (diffE g) -- > Try the case "log(g(x))g'(x)"
        pure $ Mul (toExpr coeff) (Mul g (Add (Log g) (toExpr -1)))
      _       -> Nothing
  where
    -- > split a function into a coefficient times the simplified form.
    splitByCoeff (simplify -> e) = case e of
      P [(c, 0)]          -> (c, toExpr 1) -- > f(x) = c => (c, 1)
      Mul (P [(c, 0)]) e' -> (c, e') -- > f(x) = c * g(x) => (c, g(x))
      _                   -> (1, e) -- > f(x) can't be simplified => (1, f(x))
    -- > attempt to factorise the given functions, which only works if they
    -- > can both be split into a coefficient times the same simplified form.
    factorise (splitByCoeff -> (c1, r1)) (splitByCoeff -> (c2, r2))
      | r1 == r2  = Just $ c1 / c2
      | otherwise = Nothing

--
-- Given...
--
simplifiedInt :: Expr -> Maybe Expr
simplifiedInt = fmap simplify . intE

printInt :: Expr -> IO ()
printInt e = maybe (putStrLn "Fail") prettyPrint (simplifiedInt e)
