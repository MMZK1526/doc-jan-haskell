{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE InstanceSigs #-}

module Year2016.Exam where

-- > The XML transformation is a foreign concept for me, and while this test
-- > is not (in my opinion) the hardest of all times, I did spend much longer
-- > than average both reading the specification and debugging my code. It would
-- > be another example where I may not be able to get everything right in the
-- > 3-hour time limit!
-- >
-- > That being said, the logic of the XML transformation is not too difficult
-- > once you understand the specification. The parsing in Part II can be
-- > quite cumbersome, especially for those who used parser combinators before.
-- > Evidently monadic parser wins!

import Data.Char
import Data.Maybe

import Control.Monad.IO.Class
import Control.Monad.Trans.State
import Data.Bifunctor
import Test

type Name = String

type Attributes = [(Name, String)]

-- > The @Null@ constructor is removed from the updated specification but not
-- > from the code. It is used to represent the empty XML element, but now that
-- > is represented by @Text ""@.
data XML = Null | Text String | Element Name Attributes [XML]
         deriving (Eq, Show)

type Stack = [XML]

-----------------------------------------------------------------------
-- Some useful show/print functions

-- The 'show' function for XML objects
showXML :: XML -> String
showXML (Text t)
  = t
showXML (Element n as es)
  = "<" ++ n ++ showAtts as ++ ">" ++ concatMap showXML es ++ "</" ++ n ++ ">"
  where
    showAtts as = concatMap showAtt as
    showAtt (n, v) = " " ++ n ++ "=" ++ "\"" ++ v ++ "\""

-- The 'show' function for lists of XML objects
showXMLs :: [XML] -> String
showXMLs
  = concatMap showXML

-- Prints an XML object to the terminal
printXML :: XML -> IO()
printXML
  = putStrLn . showXML

-- Prints a list of XML objects to the terminal (useful for testing the
-- output from expandXML')
printXMLs :: [XML] -> IO()
printXMLs
  = mapM_ printXML

-----------------------------------------------------------------------
-----------------------------------------------------------------------
-- Part I

skipSpace :: String -> String
skipSpace = dropWhile isSpace

getAttribute :: String -> XML -> String
getAttribute attrName (Element _ attrs _) = fromMaybe "" (lookup attrName attrs)
getAttribute _ _                          = ""

getChildren :: String -> XML -> [XML]
getChildren name (Element _ _ children) = filter nameFilter children
  where
    nameFilter (Element n _ _) = n == name
    nameFilter _               = False
getChildren _ _                         = []

getChild :: String -> XML -> XML
getChild name xml = case getChildren name xml of
  []      -> Text ""
  (x : _) -> x

addChild :: XML -> XML -> XML
-- Pre: the second argument is an Element
addChild xml (Element name attrs children)
  = Element name attrs (children ++ [xml])

getValue :: XML -> XML
getValue = Text . getString
  where
    getString Null                   = ""
    getString (Text s)               = s
    getString (Element _ _ children) = concatMap getString children

-------------------------------------------------------------------------
-- Part II

-- Parses an element/attribute name
parseName :: String -> (Name, String)
parseName []
  = error "Error: attempt to read empty name"
parseName s@(c : cs)
  | isAlpha c = break (not . isNameChar) s
  | otherwise = error ("parseName error: name " ++ show s ++
                      " must begin with a letter")
  where
    isNameChar c = isAlpha c || isDigit c || elem c "-."

sentinel :: XML
sentinel
  = Element "" [] []

addText :: String -> Stack -> Stack
-- Pre: There is at least one Element on the stack
addText str (Element name attrs children : xmls)
  = Element name attrs (children ++ [Text str]) : xmls

popAndAdd :: Stack -> Stack
-- Pre: There are at least two Elements on the stack
popAndAdd (xml : Element name attrs children : xmls)
  = Element name attrs (children ++ [xml]) : xmls

parseAttributes :: String -> (Attributes, String)
-- Pre: The XML attributes string is well-formed
parseAttributes str = runState (modify skipSpace >> worker) str
  where
    worker   = do
      str <- get
      if head str == '>'
        then [] <$ skip1 -- > End of attribute list
        else do -- > Parse an attribute
          let (name, str') = parseName str
          lexeme $ put str'
          lexeme skip1 -- > By well-formed assumption, this must be a '='
          lexeme skip1 -- > By well-formed assumption, this must be a '"'
          str <- get
          let (value, _ : str') = break (== '"') str
          lexeme $ put str'
          ((name, value) :) <$> worker -- > Parse the rest of the attributes
    lexeme p = p <* modify skipSpace -- > Skip spaces after parsing
    skip1    = modify (drop 1) -- > Skip one character

parse :: String -> XML
-- Pre: The XML string is well-formed
parse s
  = parse' (skipSpace s) [sentinel]

-- > We assume that all inputs are well-formed.
-- >
-- > Each case is a direct translation from the algorithm in the specification.
parse' :: String -> Stack -> XML
parse' "" (Element _ _ (xml : _) : _)
 = xml
parse' ('<' : '/' : str) xmls
  = parse' (tail $ dropWhile (/= '>') str) (popAndAdd xmls)
parse' ('<' : str) xmls
  = parse' str' (Element name attrs [] : xmls)
  where
    (name, (attrs, str')) = second parseAttributes (parseName str)
parse' str xmls
  = parse' str' (addText text xmls)
  where
    (text, str') = break (== '<') str

-------------------------------------------------------------------------
-- Part III

type Context = XML

type XSL = XML

-- Parses XSL and XML source documents and transforms the latter using the
-- former. The output is written to the given file (String).
-- Example use:
--   output "out.html" filmsXSL films
-- To render output.html in a browser, type this at the Linux prompt:
--   firefox output.html &
output :: String -> XML -> XML -> IO()
output file xsl source
  = writeFile file (showXMLs (expandXSL xsl source))

expandXSL :: XSL -> XML -> [XML]
expandXSL xsl source
  = expandXSL' root xsl
  where
    root = Element "/" [] [source]

-- > The `context` keeps track of the current position in the XML tree. The only
-- > place where the context is modified is in the `for-each` case, where it is
-- > set to each of the elements in the list of elements selected by the
-- > `select` attribute.
expandXSL' :: Context -> XSL -> [XML]
expandXSL' context xsl = case xsl of
  Element "value-of" [("select", value)] _
    -> [getElem (breakPath value) context]
  Element "for-each" [("select", value)] children
    -> [ xml | context <- getDescendents (breakPath value) context
             , child <- children, xml <- expandXSL' context child ]
  Element name attrs children
    -> [Element name attrs (concatMap (expandXSL' context) children)]
  xml
    -> [xml]
  where
    -- > Split a path into its components; '.' is ignored since it is already
    -- > the current directory.
    breakPath ""                = []
    breakPath path              = case break (== '/') path of
      ("", y)    -> breakPath y
      (".", y)   -> breakPath y
      (x, "")    -> [x]
      (x, _ : y) -> x : breakPath y
    -- > The following are partial functions that assume the path is
    -- > well-formed. `getElem` returns the textual part of the first element
    -- > that matches the path, and `getDescendents` returns all elements that
    -- > match the path.
    getElem [] xml              = getValue xml
    getElem ['@' : attr] xml    = Text $ getAttribute attr xml
    getElem (x : xs) xml        = getElem xs (getChild x xml)
    getDescendents [] xml       = [xml]
    getDescendents (x : xs) xml = concatMap (getDescendents xs)
                                            (getChildren x xml)

-------------------------------------------------------------------------
-- Test data for Parts I and II

-- Simple test cases (no whitespace)
s1, s2, s3 :: String
s1
  = "<a>A</a>"
s2
  = "<a x=\"1\"><b>A</b><b>B</b></a>"
s3
  = "<a>\
      \<b>\
        \<c att=\"att1\">text1</c>\
        \<c att=\"att2\">text2</c>\
      \</b>\
      \<b>\
        \<c att=\"att3\">text3</c>\
        \<d>text4</d>\
      \</b>\
    \</a>"

-- Parsed versions of the above
x1, x2, x3 :: XML
x1
  = Element "a" [] [Text "A"]
x2
  = Element "a"
            [("x","1")]
            [Element "b" [] [Text "A"],
             Element "b" [] [Text "B"]]
x3
  = Element "a"
            []
            [Element "b"
                     []
                     [Element "c"
                              [("att","att1")]
                              [Text "text1"],
                      Element "c"
                              [("att","att2")]
                              [Text "text2"]],
             Element "b"
                     []
                     [Element "c"
                              [("att","att3")]
                              [Text "text3"],
                      Element "d"
                              []
                              [Text "text4"]]]

casablanca :: String
casablanca
  = "<film title=\"Casablanca\">\n  <director>Michael Curtiz</director>\n  <year>1942\
    \</year>\n</film>\n\n\n"

casablancaParsed :: XML
casablancaParsed
  = Element "film"
            [("title","Casablanca")]
            [Text "\n  ",
             Element "director" [] [Text "Michael Curtiz"],
             Text "\n  ",
             Element "year" [] [Text "1942"],
             Text "\n"]

-- Films mark-up of Figure 1
films :: String
films
  = "<filmlist>\n\
    \  <film title = \"Rear Window\">\n\
    \    <director>Alfred Hitchcock</director>\n\
    \    <composer>Franz Waxman</composer>\n\
    \    <year>1954</year>\n\
    \  </film>\n\
    \  <film   title =  \"2001: A Space Odyssey\">\n\
    \    <director>Stanley Kubrick</director>\n\
    \    <composer>Richard Strauss</composer>\n\
    \    <composer>Gyorgy Ligeti</composer>\n\
    \    <composer>Johann Strauss</composer>\n\
    \    <year>1968</year>\n\
    \  </film>\n\
    \  <film title=\"Lawrence of Arabia\"  >\n\
    \    <duration>228</duration>\n\
    \    <director>David Lean</director>\n\
    \    <composer>Maurice Jarre</composer>\n\
    \  </film>\n\
    \</filmlist>\n\n\n"

-- Parsed version of films ('parse films'), suitably formatted
filmsParsed :: XML
filmsParsed
  = Element "filmlist"
            []
            [Text "\n  ",
             Element "film" [("title","Rear Window")]
                            [Text "\n    ",
                             Element "director" [] [Text "Alfred Hitchcock"],
                             Text "\n    ",
                             Element "composer" [] [Text "Franz Waxman"],
                             Text "\n    ",
                             Element "year" [] [Text "1954"],
                             Text "\n  "],
             Text "\n  ",
             Element "film" [("title","2001: A Space Odyssey")]
                            [Text "\n    ",
                             Element "director" [] [Text "Stanley Kubrick"],
                             Text "\n    ",
                             Element "composer" [] [Text "Richard Strauss"],
                             Text "\n    ",
                             Element "composer" [] [Text "Gyorgy Ligeti"],
                             Text "\n    ",
                             Element "composer" [] [Text "Johann Strauss"],
                             Text "\n    ",
                             Element "year" [] [Text "1968"],
                             Text "\n  "],
             Text "\n  ",
             Element "film" [("title","Lawrence of Arabia")]
                            [Text "\n    ",
                             Element "duration" [] [Text "228"],
                             Text "\n    ",
                             Element "director" [] [Text "David Lean"],
                             Text "\n    ",
                             Element "composer" [] [Text "Maurice Jarre"],
                             Text "\n  "],
             Text "\n"]

-------------------------------------------------------------------------
-- XSL tests

-- value-of test cases
xsl1, xsl2, xsl3, xsl4, xsl5, xsl6, xsl7,
  xsl8, xsl9 :: String
xsl1
  = "<value-of select = \"a/b/c\"></value-of>"
xsl2
  = "<value-of select = \"a/b\"></value-of>"
xsl3
  = "<value-of select = \"a/b/d\"></value-of>"
xsl4
  = "<value-of select = \"a/b/c/@att\"></value-of>"
xsl5
  = "<value-of select = \"./a/./b/c/./.\"></value-of>"
xsl6
  = "<t1><t2>Preamble</t2><t3><value-of select = \"a/b/c\"></value-of></t3></t1>"

-- for-each test cases
xsl7
  = "<for-each select=\"a/b/c\"><value-of select=\"./@att\"></value-of>\
    \</for-each>"
xsl8
  = "<for-each select=\"a/b\"><t><value-of select=\"c\"></value-of></t>\
    \</for-each>"
xsl9
  = "<for-each select=\"a/b\"><t1><value-of select=\"absent\"></value-of>\
    \</t1></for-each>"

-- Parsed versions of the above
xsl1Parsed, xsl2Parsed, xsl3Parsed, xsl4Parsed, xsl5Parsed,
  xsl6Parsed, xsl7Parsed, xsl8Parsed, xsl9Parsed :: XML
xsl1Parsed
  = Element "value-of" [("select","a/b/c")] []
xsl2Parsed
  = Element "value-of" [("select","a/b")] []
xsl3Parsed
  = Element "value-of" [("select","a/b/d")] []
xsl4Parsed
  = Element "value-of" [("select","a/b/c/@att")] []
xsl5Parsed
  = Element "value-of" [("select","./a/./b/c/./.")] []
xsl6Parsed
  = Element "t1"
            []
            [Element "t2" [] [Text "Preamble"],
             Element "t3" [] [Element "value-of" [("select","a/b/c")] []]]

xsl7Parsed
  = Element "for-each"
            [("select","a/b/c")]
            [Element "value-of" [("select","./@att")] []]
xsl8Parsed
  = Element "for-each"
            [("select","a/b")]
            [Element "t" [] [Element "value-of" [("select","c")] []]]
xsl9Parsed
  = Element "for-each"
            [("select","a/b")]
            [Element "t1" [] [Element "value-of" [("select","absent")] []]]

-- XSL template for building a films summary (example from spec.)
filmsXSL :: String
filmsXSL
  = "<html>\n\
    \<body>\n\
    \  <h2>Film List</h2>\n\
    \  <table border=\"1\">\n\
    \    <tr>\n\
    \      <th align=\"left\">Title</th>\n\
    \      <th align=\"left\">Director</th>\n\
    \      <th align=\"left\">Principal composer</th>\n\
    \    </tr>\n\
    \    <for-each select=\"filmlist/film\">\n\
    \      <tr>\n\
    \        <td><value-of select=\"@title\"></value-of></td>\n\
    \        <td><value-of select=\"director\"></value-of></td>\n\
    \        <td><value-of select=\"composer\"></value-of></td>\n\
    \      </tr>\n\
    \    </for-each>\n\
    \  </table>\n\
    \</body>\n\
    \</html>"

-- XSL template for building a list of composers (example from spec.)
composersXSL :: String
composersXSL
  = "<for-each select=\"filmlist/film\">\
      \<h2><value-of select=\"@title\"></value-of> composers</h2>\
      \<ul>\
      \<for-each select=\"composer\">\
        \<li><value-of select=\".\"></value-of></li>\
      \</for-each>\
      \</ul>\
    \</for-each>"

---------------------------------------------------------
-- Test & Helpers

tester :: IO ()
tester = runTest do
  let (.==.) = with EqXML (==.)
  label "Test 'allSame'" do
    skipSpace "\n \n\nsome \n \n text" ==. "some \n \n text"
  label "Test 'getAttribute'" do
    getAttribute "x" x2 ==. "1"
    getAttribute "x" (Text "t") ==. ""
  label "Test 'getChildren'" do
    getChildren "b" x2 ==. [Element "b" [] [Text "A"], Element "b" [] [Text "B"]]
    getChildren "c" x2 ==. []
  label "Test 'getChild'" do
    getChild "b" x2 ==. Element "b" [] [Text "A"]
    getChild "c" x2 ==. Text ""
  label "Test 'addChild'" do
    addChild (Text "B") (Element "a" [] [Text "A"]) ==. Element "a" [] [Text "A", Text "B"]
  label "Test 'getValue'" do
    getValue x1 ==. Text "A"
    getValue x2 ==. Text "AB"
  label "Test 'parse" do
    parse xsl1 ==. xsl1Parsed
    parse xsl2 ==. xsl2Parsed
    parse xsl3 ==. xsl3Parsed
    parse xsl4 ==. xsl4Parsed
    parse xsl5 ==. xsl5Parsed
    parse xsl6 ==. xsl6Parsed
    parse xsl7 ==. xsl7Parsed
    parse xsl8 ==. xsl8Parsed
    parse xsl9 ==. xsl9Parsed
    parse films ==. filmsParsed
  label "Test 'expandXSL'" do
    liftIO (parseFile "src/Year2016/filmTable.html") >>= (.==. parse (showXMLs (expandXSL (parse filmsXSL) filmsParsed)))
    liftIO (parseFile "src/Year2016/composerList.html") >>= (.==. parse (showXMLs (expandXSL (parse composersXSL) filmsParsed)))

parseFile :: FilePath -> IO XML
parseFile path = do
  contents <- readFile path
  pure $ parse contents

newtype EqXML = EqXML { unEQ :: XML }

instance Eq EqXML where
  (==) :: EqXML -> EqXML -> Bool
  EqXML x1 == EqXML x2 = equals x1 x2
    where
      equals (Text t1) (Text t2)
        = skipSpace t1 == skipSpace t2
      equals (Element name1 attrs1 children1) (Element name2 attrs2 children2)
        = name1 == name2 && attrs1 == attrs2 && and (zipWith equals children1 children2)

instance Show EqXML where
  show :: EqXML -> String
  show (EqXML xml) = showXML xml
