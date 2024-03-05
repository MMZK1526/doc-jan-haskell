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
cleanUp = map toLower . filter (`notElem` punctuation)

split2 :: [a] -> [([a], [a])]
split2 xs = flip splitAt xs <$> [1..(length xs - 1)]

split3 :: [a] -> [([a], [a], [a])]
split3 = concatMap worker . split2
  where
    worker (xs, zs) = (xs, [], zs) : [(xs', ys', zs) | (xs', ys') <- split2 zs]

uninsert :: [a] -> [([a], [a])]
uninsert = map (\(xs, ys, zs) -> (ys, xs ++ zs)) . split3

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

------------------------------------------------------
-- Part II

matches :: String -> ParseTree -> Bool
matches clue (Synonym str)       = clue `elem` synonyms str
matches clue (Anagram _ str)     = sort clue == sort str
matches clue (Reversal _ tree)   = matches (reverse clue) tree
matches clue (Insertion _ t1 t2) = or [ matches c1 t1 && matches c2 t2
                                      | (c1, c2) <- uninsert clue ]
matches clue (Charade _ t1 t2)   = or [ matches c1 t1 && matches c2 t2
                                      | (c1, c2) <- split2 clue ]

evaluate :: Parse -> Int -> [String]
evaluate (defs, _, tree) size
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

