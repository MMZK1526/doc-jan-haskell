module Year2023.Solver where

import Data.List
import Data.Char

import Year2023.Types
import Year2023.WordData
import Year2023.Clues
import Year2023.Examples

------------------------------------------------------
-- Part I

punctuation :: String
punctuation 
  = "';.,-!?"

cleanUp :: String -> String
cleanUp 
  = undefined

split2 :: [a] -> [([a], [a])]
split2 
  = undefined

split3 :: [a] -> [([a], [a], [a])]
split3
  = undefined

uninsert :: [a] -> [([a], [a])]
uninsert
  = undefined

{- Uncomment these functions when you have defined the above.
split2M :: [a] -> [([a], [a])]
split2M xs
  = sxs ++ [(y, x) | (x, y) <- sxs] 
  where
    sxs = split2 xs

split3M :: [a] -> [([a], [a], [a])]
split3M xs
  = sxs ++ [(z, y, x) | (x, y, z) <- sxs]
  where
    sxs = split3 xs
-}

------------------------------------------------------
-- Part II

matches :: String -> ParseTree -> Bool
matches
  = undefined

evaluate :: Parse -> Int -> [String]
evaluate 
  = undefined

------------------------------------------------------
-- Part III

-- Given...
parseWordplay :: [String] -> [ParseTree]
parseWordplay ws
  = concat [parseSynonym ws,
            parseAnagram ws,
            parseReversal ws,
            parseInsertion ws,
            parseCharade ws]
    
parseSynonym :: [String] -> [ParseTree]
parseSynonym 
  = const []

parseAnagram :: [String] -> [ParseTree]
parseAnagram
  = const []

parseReversal :: [String] -> [ParseTree]
parseReversal
  = const []

parseInsertion :: [String] -> [ParseTree]
parseInsertion
  = const []

parseCharade :: [String] -> [ParseTree]
parseCharade 
  = const []

-- Given...
parseClue :: Clue -> [Parse]
parseClue clue@(s, n)
  = parseClueText (words (cleanUp s))

parseClueText :: [String] -> [Parse]
parseClueText
  = undefined

solve :: Clue -> [Solution]
solve 
  = undefined


------------------------------------------------------
-- Some additional test functions

-- Returns the solution(s) to the first k clues.
-- The nub removes duplicate solutions arising from the
-- charade parsing rule.
solveAll :: Int -> [[String]]
solveAll k
  = map (nub . map getSol . solve . (clues !!)) [0..k-1]

getSol :: Solution -> String
getSol (_, _, sol) = sol

showAll
  = mapM_ (showSolutions . solve . (clues !!)) [0..23]

