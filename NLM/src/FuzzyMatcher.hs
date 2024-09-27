module FuzzyMatcher (aggregateFuzzyWordFrequencies) where

import Data.List (groupBy, sort)

-- Function to calculate the Levenshtein distance between two strings
levenshtein :: String -> String -> Int
levenshtein s1 s2 = lev (length s1) (length s2)
  where
    lev 0 n = n
    lev m 0 = m
    lev m n = minimum [ lev (m-1) (n-1) + cost
                      , lev (m-1) n + 1
                      , lev m (n-1) + 1 ]
      where cost = if s1 !! (m-1) == s2 !! (n-1) then 0 else 1

-- Group words that are within a fuzzy threshold
fuzzyGroupWords :: [String] -> [[String]]
fuzzyGroupWords = groupBy (\w1 w2 -> levenshtein w1 w2 <= 2) . sort

-- Aggregate fuzzy word frequencies
aggregateFuzzyWordFrequencies :: [String] -> [(String, Int)]
aggregateFuzzyWordFrequencies tokens =
    map (\g -> (head g, length g)) (fuzzyGroupWords tokens)

