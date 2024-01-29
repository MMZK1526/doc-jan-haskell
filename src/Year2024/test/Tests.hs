import IC.TestSuite

import Int
import Utilities
import Examples
import Types

main :: IO ()
main = runTests tests

tests :: [TestGroup]
tests =
  [ testGroup "addP" addPTests
  , testGroup "mulP" mulPTests
  , testGroup "sumP" sumPTests
  , testGroup "prodP" prodPTests
  , testGroup "diffT" diffTTests
  , testGroup "intT" intTTests
  , testGroup "diffP" diffPTests
  , testGroup "intP" intPTests
  -- PART II
  , testGroup "diffE" diffETests
  -- PART III
  , testGroup "intE" intETests
  , testGroup "intESecret" intESecretTests
  ]

addPTests :: [TestCase]
addPTests = [ pretty (addP p2 p2) --> "[2x]"
            , pretty (addP p3 p4) --> "[2x^2 + x - 2]"
            , pretty (addP p5 [(0, 0)]) --> "[2x^3 - 2x + 2]"
            , pretty (addP [(3, 2)] [(1, 1)]) --> "[3x^2 + x]"
            ]

mulPTests :: [TestCase]
mulPTests = [ pretty (mulP p3 p4) --> "[8x^3 - 18x^2 + 13x - 3]"
            ]

sumPTests :: [TestCase]
sumPTests = [ pretty (sumP [[(0, 0)]]) --> "[0]"
            , pretty (sumP [p1, p2, p3, p4, p5]) --> "[2x^3 + 2x^2 + 5]"
            ]

prodPTests :: [TestCase]
prodPTests = [ pretty (prodP [p3, p5]) -->
                 "[4x^5 - 6x^4 - 2x^3 + 10x^2 - 8x + 2]"
             ]

diffTTests :: [TestCase]
diffTTests = [ diffT (1, 0) --> (0, 0)
             , diffT (2, 3) --> (6, 2)
             ]

intTTests :: [TestCase]
intTTests = [ intT (1, 0) --> (1, 1)
            , intT (2, 3) --> (1 / 2, 4)
            ]

diffPTests :: [TestCase]
diffPTests = [ pretty (simplify (P (diffP p3))) -->
                 -- without the `simplify . P`, this needs a `+ 0` on the end
                 "[4x - 3]"
             ]

intPTests :: [TestCase]
intPTests = [ pretty (intP p3) --> "[(2/3)x^3 + (-3/2)x^2 + x]"
            ]

diffETests :: [TestCase]
diffETests = [ pretty (simplifiedDiff e3) --> "[4x - 3]"
             , pretty (simplifiedDiff e10) --> "[24x^2 - 36x + 13]"
             , pretty (simplifiedDiff e11) --> "[-1] . [x]^-2 . log[x] + [x]^-2"
             ]

intETests :: [TestCase]
intETests = [ fmap pretty (simplifiedInt e1) -->
                Just "[5x]"
            , fmap pretty (simplifiedInt e2) -->
                Just "[(1/2)x^2]"
            , fmap pretty (simplifiedInt e3) -->
                Just "[(1/6)] . [4x^3 - 9x^2 + 6x]"
            , fmap pretty (simplifiedInt e4) -->
                Just "[2x^2 - 3x]"
            , fmap pretty (simplifiedInt e5) -->
                Just "[(1/2)] . [x^4 - 2x^2 + 4x]"
            , fmap pretty (simplifiedInt e6) -->
                Just "[(1/6)] . [3x^4 + 4x^3 - 15x^2 + 18x]"
            , fmap pretty (simplifiedInt e7) -->
                Just "[(5/6)] . [3x^4 + 4x^3 - 15x^2 + 18x]"
            , fmap pretty (simplifiedInt e8) -->
                Just "[x] . ([-1] + log[x])"
            , fmap pretty (simplifiedInt e9) -->
                Just "log[x]"
            , fmap pretty (simplifiedInt e10) -->
                Just "[(1/2)] . [4x^4 - 12x^3 + 13x^2 - 6x + 1]"
            , fmap pretty (simplifiedInt e11) -->
                Just "[(1/2)] . (log[x])^2"
            , fmap pretty (simplifiedInt e12) -->
                Just "[2x^2 - 3x + 1] . ([-1] + log[2x^2 - 3x + 1])"
            , fmap pretty (simplifiedInt e13) -->
                Just "[(2/5)] . ([2x^2 - 3x + 1])^(5/2)"
            , fmap pretty (simplifiedInt e14) -->
                Just "[(2/25)] . ([5] . [2x^2 - 3x + 1])^(5/2)"
            , fmap pretty (simplifiedInt e15) -->
                Just "[(1/3)] . [x^3 - 3x] . ([-1] + log[x^3 - 3x])"
            , fmap pretty (simplifiedInt e16) -->
                Nothing
            ]

intESecretTests :: [TestCase]
intESecretTests = [ fmap pretty (simplifiedInt e17) -->
                      Just "[(2/125)] . ([5] . [2x^2 - 3x + 1])^(5/2)"
                  , fmap pretty (simplifiedInt e18) -->
                      Just "[(1/15)] . [x^3 - 3x] . ([-1] + log[x^3 - 3x])"
                  , fmap pretty (simplifiedInt e19) -->
                      Just "[(1/3)] . [x^3 - 3x]"
                  , fmap pretty (simplifiedInt e20) -->
                      Just "[(1/4)] . [x^4 - 6x^2]"
                  , fmap pretty (simplifiedInt e21) -->
                      Just "[(1/3)] . [x^3 - 3x] . ([-1] + log[x^3 - 3x])"
                  , fmap pretty (simplifiedInt e22) -->
                      Just "[(5/3)] . [x^3 - 3x] . ([-1] + log[x^3 - 3x])"
                  ]
