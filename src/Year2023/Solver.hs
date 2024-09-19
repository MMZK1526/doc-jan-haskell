module Year2023.Solver where

import Data.List
import Data.Char

import Year2023.Types
import Year2023.WordData
import Year2023.Clues
import Year2023.Examples

import Control.Monad

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
    worker (xs, zs) = (xs, [], zs) : [(xs, ys', zs') | (ys', zs') <- split2 zs]

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
  = concatMap (filter isMatch . synonyms) defs
  where
    isMatch syn = length syn >= size && matches syn tree

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
parseSynonym ws = let sentence = unwords ws in case synonyms sentence of
  [] -> []
  _  -> [Synonym sentence]

parseAnagram :: [String] -> [ParseTree]
parseAnagram ws = do
  (ind, args) <- split2M ws
  guard $ unwords ind `elem` anagramIndicators
  pure $ Anagram ind (concat args)

parseReversal :: [String] -> [ParseTree]
parseReversal ws = do
  (ind, args) <- split2M ws
  guard $ unwords ind `elem` reversalIndicators
  clue        <- parseWordplay args
  pure $ Reversal ind clue

parseInsertion :: [String] -> [ParseTree]
parseInsertion ws = standardInsertions <> envelopeInsertions
  where
    standardInsertions = do
      (arg, ind, arg') <- split3 ws
      guard $ unwords ind `elem` insertionIndicators
      clue             <- parseWordplay arg
      clue'            <- parseWordplay arg'
      pure $ Insertion ind clue clue'
    envelopeInsertions = do
      (arg, ind, arg') <- split3 ws
      guard $ unwords ind `elem` envelopeIndicators
      clue             <- parseWordplay arg
      clue'            <- parseWordplay arg'
      pure $ Insertion ind clue' clue

parseCharade :: [String] -> [ParseTree]
parseCharade ws = beforeCharade <> afterCharade
  where
    beforeCharade = do
      (arg, ind, arg') <- split3 ws
      guard $ unwords ind `elem` beforeIndicators
      clue             <- parseWordplay arg
      clue'            <- parseWordplay arg'
      pure $ Charade ind clue clue'
    afterCharade = do
      (arg, ind, arg') <- split3 ws
      guard $ unwords ind `elem` afterIndicators
      clue             <- parseWordplay arg
      clue'            <- parseWordplay arg'
      pure $ Charade ind clue' clue

-- Given...
parseClue :: Clue -> [Parse]
parseClue clue@(s, n)
  = parseClueText (words (cleanUp s))

parseClueText :: [String] -> [Parse]
parseClueText ws = do
  (def, link, wps) <- split3M ws
  guard $ unwords link `elem` linkWords
  guard (not . null . synonyms $ unwords def)
  clue             <- parseWordplay wps
  pure (def, link, clue)

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

