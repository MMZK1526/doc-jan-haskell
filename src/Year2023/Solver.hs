{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Year2023.Solver where

import Control.Monad
import Data.List
import Data.Char
import Test

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
    worker (xs, zs) = (xs, [], zs) : [(xs, ys', zs') | (ys', zs') <- split2 zs]

uninsert :: [a] -> [([a], [a])]
uninsert = map (\(xs, ys, zs) -> (ys, xs ++ zs))
         . filter (\(_, ys, _) -> not (null ys)) . split3

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
matches clue (HiddenWord _ str)  = clue == str

evaluate :: Parse -> Int -> [String]
evaluate (def, _, tree) size
  = filter isMatch $ synonyms (unwords def)
  where
    isMatch syn = length syn == size && matches syn tree

------------------------------------------------------
-- Part III

-- Given...
parseWordplay :: [String] -> [ParseTree]
parseWordplay ws
  = concat [parseSynonym ws,
            parseAnagram ws,
            parseReversal ws,
            parseInsertion ws,
            parseCharade ws,
            parseHiddenWord ws]

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
solve clue@(s, n) = concatMap buildSolution (parseClue clue)
  where
    buildSolution p = (clue, p,) <$> evaluate p n
    parseTrees = parseClue clue

------------------------------------------------------
-- Part IV

parseHiddenWord :: [String] -> [ParseTree]
parseHiddenWord ws = do
  (ind, args) <- split2 ws
  guard $ unwords ind `elem` hiddenWordIndicators
  hiddenWord  <- extractHiddenWords args
  guard (not . null $ synonyms hiddenWord)
  pure $ HiddenWord ind hiddenWord

trueSuffixes :: [a] -> [[a]]
truePrefixes :: [a] -> [[a]]
trueSuffixes []       = []
trueSuffixes [_]      = []
trueSuffixes (x : xs) = xs : trueSuffixes xs
truePrefixes xs       = reverse <$> trueSuffixes (reverse xs)

extractHiddenWords :: [[a]] -> [[a]]
extractHiddenWords []  = []
extractHiddenWords [w] = concatMap trueSuffixes (truePrefixes w)
extractHiddenWords [w, w'] = liftM2 (<>) (trueSuffixes w) (truePrefixes w')
extractHiddenWords (w : ws) = do
  let (ws', [w']) = splitAt (length ws - 1) ws
  start <- trueSuffixes w
  end   <- truePrefixes w'
  pure $ start <> concat ws' <> end


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


---------------------------------------------------------
-- Test & Helpers

tester :: IO ()
tester = runTest do
  let (.==.) :: forall a. (Ord a, Show a) => [a] -> [a] -> Test
      (.==.) = with sort (==.)
  let allSolutions =
        [ ["concern","problem"], ["rotates"], ["redshank"], ["master"], ["edam"]
        , ["repaid"], ["amnesty"], ["remainder"], ["sustain"], ["loire"]
        , ["cabin"],["snappy"],["fremantle"],["nasty"], ["rotate"], ["inapt"]
        , ["kensington"],["defiant"],["speed"], ["ling"], ["edward"], ["hera"]
        , ["large"], ["tofu"] ]
  label "Test 'cleanUp'" do
    cleanUp "half-baked" ==. "halfbaked"
    cleanUp "That's it!" ==. "thats it"
    cleanUp "all ok" ==. "all ok"
  label "Test 'split2'" do
    split2 ([] @Int) .==. []
    split2 [1] .==. []
    split2 [1, 2] .==. [([1], [2])]
    split2 [1, 2, 3, 4] .==. [([1], [2, 3, 4]), ([1, 2], [3, 4]), ([1, 2, 3], [4])]
  label "Test 'split3'" do
    split3 ([] @Int) .==. []
    split3 [1] .==. []
    split3 [1, 2] .==. [([1], [], [2])]
    split3 [1, 2, 3] .==. [([1], [], [2, 3]), ([1], [2], [3]), ([1, 2],[],[3])]
  label "Test 'uninsert'" do
    uninsert ([] @Int) .==. []
    uninsert [1] .==. []
    uninsert [1, 2, 3] .==. [([2], [1, 3])]
    uninsert [1, 2, 3, 4] .==. [([2], [1, 3, 4]), ([2, 3], [1, 4]), ([3], [1, 2, 4])]
  label "Test 'matches'" do
    forM_ (zip [0..] trees) \(i, tree) ->
      forM_ (zip [0..] strings) \(j, string) ->
        matches (fst string) tree ==. i == j
  label "Test 'evaluate'" do
    forM_ (zip3 parses enumerations allSolutions) \(p, e, s) ->
      evaluate p e .==. s
  label "Test 'parseSynonym'" do
    parseSynonym ["great"] .==. [Synonym "great"]
    parseSynonym ["at home"] .==. [Synonym "at home"]
    parseSynonym ["triangular", "mollusc"] .==. []
  label "Test 'parseAnagram'" do
    parseAnagram ["mixed", "bag"] .==. [Anagram ["mixed"] "bag"]
    parseAnagram ["bag", "mixed"] .==. [Anagram ["mixed"] "bag"]
    parseAnagram ["changed", "the", "car"] .==. [Anagram ["changed"] "thecar"]
    parseAnagram ["fruit", "cake"] .==. []
  label "Test 'parseReversal'" do
    parseReversal ["go", "backwards"] .==. [Reversal ["backwards"] (Synonym "go")]
    parseReversal ["backwards", "go"] .==. [Reversal ["backwards"] (Synonym "go")]
    parseReversal ["mixed", "bag"] .==. []
    parseReversal ["back", "at", "home"] .==. [ Reversal ["back"] (Synonym "at home")
                                              , Reversal ["back"] (Charade [] (Synonym "at") (Synonym "home")) ]
  label "Test 'parseInsertion'" do
    parseInsertion ["back", "in", "business"] .==. [Insertion ["in"] (Synonym "back") (Synonym "business")]
    parseInsertion ["work", "around", "town"] .==. [Insertion ["around"] (Synonym "town") (Synonym "work")]
    parseInsertion ["back", "pain"] .==. []
    parseInsertion ["was", "back", "in", "time"] .==. [ Insertion ["in"] (Reversal ["back"] (Synonym "was")) (Synonym "time")
                                                      , Insertion ["in"] (Charade [] (Synonym "was") (Synonym "back")) (Synonym "time") ]
  label "Test 'parseCharade'" do
    parseCharade ["stop", "go"] .==. [Charade [] (Synonym "stop") (Synonym "go")]
    parseCharade ["stop", "and", "go"] .==. [ Charade [] (Synonym "stop") (Charade [] (Synonym "and") (Synonym "go"))
                                            , Charade [] (Charade [] (Synonym "stop") (Synonym "and")) (Synonym "go")
                                            , Charade ["and"] (Synonym "stop") (Synonym "go") ]
    parseCharade ["go", "after", "stop"] .==. [ Charade [] (Synonym "go") (Charade [] (Synonym "after") (Synonym "stop"))
                                              , Charade [] (Charade [] (Synonym "go") (Synonym "after")) (Synonym "stop")
                                              , Charade ["after"] (Synonym "stop") (Synonym "go")]
  label "Test 'parseHiddenWord" do
    parseHiddenWord ["hiding", "in", "fitted", "wardrobe"] .==. [HiddenWord ["hiding", "in"] "edward"]
  label "Test 'solve'" do
    forM_ (zip (solveAll 24) allSolutions) \(exp, act) -> do
      exp .==. act
