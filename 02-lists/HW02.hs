{-
Name: <your name here>
Collaborators: <your collaborators here, or "none">
Notes: <any particular notes about your work -- what you struggled with,
        what's not working, what's really cool, etc.>
-}

module HW02 where

import Words
import Data.List
import Debug.Trace

-- Though a Scrabble hand is the same Haskell type as a Scrabble word, they
-- have different properties. Specifically, a hand is unordered whereas a word
-- is ordered. We denote this distinction by using a type synonym to talk
-- about hands, even though we could just say `String`.
type Hand = [Char]

-- A `Template` is like a word, but it has '?' characters in some places as
-- placeholders for letters from a player's hand. Because real words do not
-- have '?' characters, we use another type synonym to track this distinction.
type Template = String

-- A 'STemplate' is like a template, but it has markers to indicate four kinds
-- of special board locations: double-letter noted with 'D', triple-letter
-- noted with 'T', double-word noted with '2', and triple-word noted with '3'.
-- For matching, these behave just like '?' does -- they can be filled in with
-- any letter. But, when scoring, any letter played on a 'D' gets double its
-- value, and any letter played on a 'T' gets triple its value. If any square
-- in the template is a '2', the whole word's value is doubled; if any square
-- in the template is a '3', the whole word's score is tripled. If multiple of
-- these special squares are in the same word, the effects multiply.
type STemplate = Template

-- Write your code below:

-- check whether a word can be formed from a hand
formableBy :: String -> Hand -> Bool
formableBy [] h = True
formableBy (w:ws) h
  | w `elem` h = formableBy ws (delete w h)
  | otherwise = False

-- get all words that can be formed by a hand
wordsFrom :: Hand -> [String]
wordsFrom hand = filter (\w -> formableBy w hand) allWords

-- check whether a word matches a template
wordFitsTemplate :: Template -> Hand -> String -> Bool
wordFitsTemplate p h w
  | length w /= length p = False
  | elemIndex ch p /= elemIndex ch w = False
  | formableBy w h' = True
  | otherwise = False
  where
    ch = (filter (\c -> c /= '?') p) !! 0
    h' = h ++ [ch]

-- get all words a hand can match to template
wordsFittingTemplate :: Template -> Hand -> [String]
wordsFittingTemplate p h = filter (\w -> wordFitsTemplate p h w) allWords

scrabbleValueWord :: String -> Int
scrabbleValueWord [] = 0
scrabbleValueWord w = sum (map scrabbleValue w)

-- find words with highest point value
bestWords :: [String] -> [String]
bestWords [] = []
bestWords ws = filter (\w -> bestWordsHelper w max) ws
  where
    max = maximum (map scrabbleValueWord ws)

bestWordsHelper :: String -> Int -> Bool
bestWordsHelper w max
  | scrabbleValueWord w == max = True
  | otherwise = False

