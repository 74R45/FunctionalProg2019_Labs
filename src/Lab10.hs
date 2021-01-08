{-# OPTIONS_GHC -Wall #-}
module Lab10 where

----------------------------
-- Syntax Analysis of XML --
----------------------------

import Data.Char(isSpace, isDigit, isLetter)

type Name       = String
type Attributes = [(String, String)]
data XML        =  Text String | Element Name Attributes [XML] deriving (Eq, Show)

-- Task 1 -----------------------------------------
spaces :: String -> String
spaces "" = ""
spaces (s:xs) | isSpace s = spaces xs
              | otherwise = s:xs
  
-- Task 2 -----------------------------------------
manyT, value, manyN :: String -> (String,String)
manyN "" = ("", "")
manyN (x:xs) | isDigit x || isLetter x || x == '.' || x == '-' =
                 let (a, b) = manyN xs in (x:a, b)
             | otherwise = ("", x:xs)

value "" = ("", "")
value (x:xs) | x == '"' = ("", x:xs)
             | otherwise = let (a, b) = value xs in (x:a, b)

manyT "" = ("", "")
manyT (x:xs) | x == '<' || x == '>' = ("", x:xs)
             | otherwise = let (a, b) = manyT xs in (x:a, b)

-- Task 3 -----------------------------------------
name, text, fullValue :: String ->  Maybe(String,String)
name "" = Nothing
name (x:xs) | isLetter x = let (a, b) = manyN xs in Just (x:a, b)
            | otherwise = Nothing

text "" = Nothing
text (x:xs) | x == '<' || x == '>' = Nothing
            | otherwise = let (a, b) = manyT xs in Just (x:a, b)

fullValue "" = Nothing
fullValue (x:xs) | x == '"' = let (a, b) = value xs in Just (a, tail b)
                 | otherwise = Nothing

-- Task 4 -----------------------------------------
attrib :: String -> Maybe ((String,String),String)
attrib xs = case name xs of
              Nothing -> Nothing
              Just (nm, rest1) -> case spaces rest1 of
                ('=':rest2) -> case fullValue (spaces rest2) of
                  Nothing -> Nothing
                  Just (val, rest3) -> Just ((nm, val), spaces rest3)
                _ -> Nothing

manyAtt :: String -> Maybe (Attributes,String)
manyAtt xs = case attrib xs of
               Nothing -> Just ([], xs)
               Just ((nm, val), rest) -> case manyAtt rest of
                 Just (atts, str) -> Just ((nm, val):atts, str)
                 _ -> Nothing

-- Task 5 -----------------------------------------
begTag :: String -> Maybe ((String,Attributes),String)
begTag xs = case xs of
              ('<':rest1) -> case name rest1 of
                Just (nm, rest2) -> case manyAtt (spaces rest2) of
                  Just (atts, ('>':str)) -> Just ((nm, atts), str)
                  _ -> Nothing
                Nothing -> Nothing
              _ -> Nothing

endTag :: String -> Maybe (String,String)
endTag xs = case xs of
              ('<':'/':rest1) -> case name rest1 of
                Just (nm, ('>':str)) -> Just (nm, str)
                _ -> Nothing
              _ -> Nothing

-- Task 6 -----------------------------------------
element :: String -> Maybe (XML,String)
element xs = case begTag xs of
               Nothing -> Nothing
               Just ((tagNm, tagAtt), rest1) -> case manyXML rest1 of
                 Nothing -> Nothing
                 Just (elems, rest2) -> case partition rest2 ("</"++tagNm++">") of
                   (_, str, "") -> Just (Element tagNm tagAtt elems, str)
                   _ -> Nothing

partition :: Eq a => [a] -> [a] -> ([a], [a], [a])
partition a b = partitionStep ([], a, b)
  where partitionStep :: Eq a => ([a], [a], [a]) -> ([a], [a], [a])
        partitionStep (pref, (x:xs), (y:ys)) | x == y = partitionStep (pref++[x], xs, ys)
                                             | otherwise = (pref, x:xs, y:ys)
        partitionStep x = x

xml :: String -> Maybe (XML,String)
xml xs = case element xs of
           Just (el, str) -> Just (el, str)
           Nothing -> case text xs of
             Just (txt, str) -> Just (Text txt, str)
             Nothing -> Nothing

manyXML :: String -> Maybe ([XML],String)
manyXML xs = case xml xs of
               Just (comp, rest1) -> case manyXML rest1 of
                 Just (comps, str) -> Just (comp:comps, str)
                 Nothing -> Nothing
               Nothing -> case xs of
                 ('<':'/':_) -> Just ([], xs)
                 _ -> Nothing

-- Task 7 -----------------------------------------
fullXML :: String -> Maybe XML
fullXML xs = case element (spaces xs) of
  Just (xm, rest) -> if null (spaces rest) then Just xm else Nothing
  Nothing -> Nothing

-- Test Data --------------------------------------
-- Simple test XML objects (without blanks)
stst1, stst2, stst3 :: String
stst1 = "<a>A</a>"
stst2 = "<a x=\"1\"><b>A</b><b>B</b></a>"
stst3 = "<a>\
      \<b>\
        \<c att=\"att1\">text1</c>\
        \<c att=\"att2\">text2</c>\
      \</b>\
      \<b>\
        \<c att=\"att3\">text3</c>\
        \<d>text4</d>\
      \</b>\
    \</a>"

-- Results of analysing previous XML objects
x1, x2, x3 :: XML
x1 = Element "a" [] [Text "A"]
x2 = Element "a"
            [("x","1")]
            [Element "b" [] [Text "A"],
             Element "b" [] [Text "B"]]
x3 = Element "a"
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
