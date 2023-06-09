{-# LANGUAGE BlockArguments #-}

module Year2017.DC where

-- > This year's test is pretty straightforward, so I didn't even include any
-- > comment for the functions.
-- >
-- > Just one thing to note: the idea of decision tree will come back in the
-- > third-year module of Introduction to Machine Learning, and the
-- > implementation of decision tree is pretty much the same there (but in
-- > Python of course).

import Data.Maybe
import Data.List
import Data.Ord
import Test

type AttName = String

type AttValue = String

type Attribute = (AttName, [AttValue])

type Header = [Attribute]

type Row = [AttValue]

type DataSet = (Header, [Row])

data DecisionTree = Null |
                    Leaf AttValue | 
                    Node AttName [(AttValue, DecisionTree)]
                  deriving (Eq, Show)

type Partition = [(AttValue, DataSet)]

type AttSelector = DataSet -> Attribute -> Attribute

xlogx :: Double -> Double
xlogx p
  | p <= 1e-100 = 0.0
  | otherwise   = p * log2 p 
  where
    log2 x = log x / log 2

lookUp :: (Eq a, Show a, Show b) => a -> [(a, b)] -> b
lookUp x table
  = fromMaybe (error ("lookUp error - no binding for " ++ show x ++ 
                      " in table: " ++ show table))
              (lookup x table)

--------------------------------------------------------------------
-- PART I
--------------------------------------------------------------------

allSame :: Eq a => [a] -> Bool
allSame []       = True
allSame (x : xs) = all (== x) xs

remove :: Eq a => a -> [(a, b)] -> [(a, b)]
remove a [] = []
remove a ((x, y) : xys)
  | a == x    = remove a xys
  | otherwise = (x, y) : remove a xys

lookUpAtt :: AttName -> Header -> Row -> AttValue
--Pre: The attribute name is present in the given header.
lookUpAtt attName header = lookUp attName . zip (map fst header)

removeAtt :: AttName -> Header -> Row -> Row
removeAtt attName header = map snd . remove attName . zip (map fst header)

addToMapping :: Eq a => (a, b) -> [(a, [b])] -> [(a, [b])]
addToMapping (x, y) [] = [(x, [y])]
addToMapping (x, y) ((x', ys) : es)
  | x == x'   = (x', y : ys) : es
  | otherwise = (x', ys) : addToMapping (x, y) es

buildFrequencyTable :: Attribute -> DataSet -> [(AttValue, Int)]
--Pre: Each row of the data set contains an instance of the attribute
buildFrequencyTable (attName, attVals) (header, rows)
  = map (\attVal -> (attVal, length (filter (== attVal) dataAttVals))) attVals
  where
    dataAttVals = map (lookUpAtt attName header) rows

--------------------------------------------------------------------
-- PART II
--------------------------------------------------------------------

nodes :: DecisionTree -> Int
nodes Null        = 0
nodes (Leaf _)    = 1
nodes (Node _ ns) = 1 + sum (map (nodes . snd) ns)

evalTree :: DecisionTree -> Header -> Row -> AttValue
evalTree Null _ _          = ""
evalTree (Leaf attVal) _ _ = attVal
evalTree (Node attName ns) header row
  = evalTree (lookUp (lookUpAtt attName header row) ns) header row

--------------------------------------------------------------------
-- PART III
--------------------------------------------------------------------

--
-- Given...
-- In this simple case, the attribute selected is the first input attribute 
-- in the header. Note that the classifier attribute may appear in any column,
-- so we must exclude it as a candidate.
--
nextAtt :: AttSelector
--Pre: The header contains at least one input attribute
nextAtt (header, _) (classifierName, _)
  = head (filter ((/= classifierName) . fst) header)

partitionData :: DataSet -> Attribute -> Partition
partitionData (header, rows) (attrName, attVals)
  = map (\attVal -> (attVal, (header', rowsWith attVal))) attVals
  where
    header'         = remove attrName header
    rowsWith attVal = map (removeAtt attrName header)
                    $ filter ((== attVal) . lookUpAtt attrName header) rows

buildTree :: DataSet -> Attribute -> AttSelector -> DecisionTree 
buildTree dataSet@(header, rows) attr@(attrName, _) selector
  | null results    = Null
  | allSame results = Leaf (head results)
  | otherwise       = Node attrName' $ map (uncurry recBuild) partition
  where
    attr'@(attrName', _) = selector dataSet attr
    partition            = partitionData dataSet attr'
    results              = lookUpAtt attrName header <$> rows
    recBuild attVal ds   = (attVal, buildTree ds attr selector)

--------------------------------------------------------------------
-- PART IV
--------------------------------------------------------------------

entropy :: DataSet -> Attribute -> Double
entropy dataSet attr
  | len == 0  = 0.0
  | otherwise = negate . sum $ xlogx <$> freqs
  where
    freqTable = buildFrequencyTable attr dataSet
    len       = genericLength $ snd dataSet
    freqs     = (/ len) . fromIntegral . snd <$> freqTable
gain :: DataSet -> Attribute -> Attribute -> Double
gain dataSet attrP attrC = entWhole - entParts
  where
    dsLength = genericLength . snd
    len      = dsLength dataSet
    entWhole = entropy dataSet attrC
    partSets = snd <$> partitionData dataSet attrP
    entParts = sum $ (\ds -> entropy ds attrC * dsLength ds / len) <$> partSets

bestGainAtt :: AttSelector
bestGainAtt dataSet@(header, _) attr = maximumBy comparator attrs
  where
    attrs      = header \\ [attr]
    comparator = comparing (flip (gain dataSet) attr)

--------------------------------------------------------------------

outlook :: Attribute
outlook 
  = ("outlook", ["sunny", "overcast", "rainy"])

temp :: Attribute 
temp 
  = ("temp", ["hot", "mild", "cool"])

humidity :: Attribute 
humidity 
  = ("humidity", ["high", "normal"])

wind :: Attribute 
wind 
  = ("wind", ["windy", "calm"])

result :: Attribute 
result
  = ("result", ["good", "bad"])

fishingData :: DataSet
fishingData
  = (header, table)

header :: Header
table  :: [Row]
header 
  =  [outlook,    temp,   humidity, wind,    result] 
table 
  = [["sunny",    "hot",  "high",   "calm",  "bad" ],
     ["sunny",    "hot",  "high",   "windy", "bad" ],
     ["overcast", "hot",  "high",   "calm",  "good"],
     ["rainy",    "mild", "high",   "calm",  "good"],
     ["rainy",    "cool", "normal", "calm",  "good"],
     ["rainy",    "cool", "normal", "windy", "bad" ],
     ["overcast", "cool", "normal", "windy", "good"],
     ["sunny",    "mild", "high",   "calm",  "bad" ],
     ["sunny",    "cool", "normal", "calm",  "good"],
     ["rainy",    "mild", "normal", "calm",  "good"],
     ["sunny",    "mild", "normal", "windy", "good"],
     ["overcast", "mild", "high",   "windy", "good"],
     ["overcast", "hot",  "normal", "calm",  "good"],
     ["rainy",    "mild", "high",   "windy", "bad" ]]

--
-- This is the same as the above table, but with the result in the second 
-- column...
--
fishingData' :: DataSet
fishingData'
  = (header', table')

header' :: Header
table'  :: [Row]
header' 
  =  [outlook,    result, temp,   humidity, wind] 
table' 
  = [["sunny",    "bad",  "hot",  "high",   "calm"],
     ["sunny",    "bad",  "hot",  "high",   "windy"],
     ["overcast", "good", "hot",  "high",   "calm"],
     ["rainy",    "good", "mild", "high",   "calm"],
     ["rainy",    "good", "cool", "normal", "calm"],
     ["rainy",    "bad",  "cool", "normal", "windy"],
     ["overcast", "good", "cool", "normal", "windy"],
     ["sunny",    "bad",  "mild", "high",   "calm"],
     ["sunny",    "good", "cool", "normal", "calm"],
     ["rainy",    "good", "mild", "normal", "calm"],
     ["sunny",    "good", "mild", "normal", "windy"],
     ["overcast", "good", "mild", "high",   "windy"],
     ["overcast", "good", "hot",  "normal", "calm"],
     ["rainy",    "bad",  "mild", "high",   "windy"]]

fig1 :: DecisionTree
fig1
  = Node "outlook" 
         [("sunny", Node "temp" 
                         [("hot", Leaf "bad"),
                          ("mild",Node "humidity" 
                                       [("high",   Leaf "bad"),
                                        ("normal", Leaf "good")]),
                          ("cool", Leaf "good")]),
          ("overcast", Leaf "good"),
          ("rainy", Node "temp" 
                         [("hot", Null),
                          ("mild", Node "humidity" 
                                        [("high",Node "wind" 
                                                      [("windy",  Leaf "bad"),
                                                       ("calm", Leaf "good")]),
                                         ("normal", Leaf "good")]),
                          ("cool", Node "humidity" 
                                        [("high", Null),
                                         ("normal", Node "wind" 
                                                         [("windy",  Leaf "bad"),
                                                          ("calm", Leaf "good")])])])]

fig2 :: DecisionTree
fig2
  = Node "outlook" 
         [("sunny", Node "humidity" 
                         [("high", Leaf "bad"),
                          ("normal", Leaf "good")]),
          ("overcast", Leaf "good"),
          ("rainy", Node "wind" 
                         [("windy", Leaf "bad"),
                          ("calm", Leaf "good")])]


outlookPartition :: Partition
outlookPartition
  = [("sunny",   ([("temp",["hot","mild","cool"]),("humidity",["high","normal"]),
                   ("wind",["windy","calm"]),("result",["good","bad"])],
                  [["hot","high","calm","bad"],["hot","high","windy","bad"],
                   ["mild","high","calm","bad"],["cool","normal","calm","good"],
                   ["mild","normal","windy","good"]])),
     ("overcast",([("temp",["hot","mild","cool"]),("humidity",["high","normal"]),
                   ("wind",["windy","calm"]),("result",["good","bad"])],
                  [["hot","high","calm","good"],["cool","normal","windy","good"],
                   ["mild","high","windy","good"],["hot","normal","calm","good"]])),
     ("rainy",   ([("temp",["hot","mild","cool"]),("humidity",["high","normal"]),
                   ("wind",["windy","calm"]),("result",["good","bad"])],
                  [["mild","high","calm","good"],["cool","normal","calm","good"],
                   ["cool","normal","windy","bad"],["mild","normal","calm","good"],
                   ["mild","high","windy","bad"]]))]

---------------------------------------------------------
-- Test & Helpers

tester :: IO ()
tester = runTest do
  label "Test 'allSame" do
    allSame ([] :: [Int]) ==. True
    allSame [9, 9, 9] ==. True
    allSame "abc" ==. False
  label "Test 'remove'" do
    remove 1 [(3, 'a'), (1, 'b'), (7, 'a')] ==. [(3, 'a'), (7, 'a')]
    remove 6 ([] :: [(Int, Char)]) ==. []
  label "Test 'lookUpAtt'" do
    lookUpAtt "temp" header (head table) ==. "hot"
  label "Test 'removeAtt'" do
    removeAtt "temp" header (head table) ==. ["sunny", "high", "calm", "bad"]
  label "Test 'addToMapping'" do
    sort (addToMapping (1, 'a') [(2, "b")]) ==. [(1, "a"), (2, "b")]
    sort (addToMapping (5, 'a') [(2, "b"), (5, "bcd")]) ==. [(2, "b"), (5, "abcd")]
  label "Test 'buildFrequencyTable'" do
     buildFrequencyTable result fishingData ==. [("good", 9), ("bad", 5)]
     buildFrequencyTable outlook fishingData ==. [("sunny", 5), ("overcast", 4), ("rainy", 5)]
     buildFrequencyTable outlook ([], []) ==. [("sunny", 0), ("overcast", 0), ("rainy", 0)]
  label "Test 'nodes'" do
    nodes fig1 ==. 18
    nodes fig2 ==. 8
  label "Test 'evalTree'" do
    evalTree fig1 header (table !! 5) ==. "bad"
    evalTree fig2 header (table !! 4) ==. "good"
  label "Test 'partitionData'" do
    partitionData fishingData outlook ==. outlookPartition
  label "Test 'buildTree'" do
    buildTree fishingData result nextAtt ==. fig1
  label "Test 'entropy'" do
    entropy fishingData result ==~ 0.9402859586706309
    entropy fishingData temp ==~ 1.5566567074628228
    entropy (header, []) result ==~ 0.0
  label "Test 'informationGain'" do
    gain fishingData outlook result ==~ 0.2467498197744391
    gain fishingData temp result ==~ 2.9222565658954647e-2
    gain fishingData humidity result ==~ 0.15183550136234136
    gain fishingData wind result ==~ 0.04812703040826927
  label "Test 'bestGainAtt'" do
     bestGainAtt fishingData result ==. ("outlook", ["sunny", "overcast", "rainy"])
     buildTree fishingData result bestGainAtt ==. fig2
