module FuzzyMatcher (aggregateFuzzyWordFrequencies) where

import Data.List (groupBy, sort)

-- **Pure Function**: levenshtein
-- Calculates the Levenshtein distance between two strings.
-- This function is recursive and pure, focusing solely on its input values.
levenshtein :: String -> String -> Int
levenshtein s1 s2 = lev (length s1) (length s2)
  where
    lev 0 n = n  -- If the first string is empty, the distance is the length of the second string
    lev m 0 = m  -- If the second string is empty, the distance is the length of the first string
    lev m n = minimum [ lev (m-1) (n-1) + cost  -- If characters match, no cost; otherwise, 1 cost
                      , lev (m-1) n + 1        -- Deletion
                      , lev m (n-1) + 1        -- Insertion
                      ]
      where cost = if s1 !! (m-1) == s2 !! (n-1) then 0 else 1  -- Determine the cost based on character match

-- **Higher-order Function**: fuzzyGroupWords
-- Groups words that are within a fuzzy threshold based on the Levenshtein distance.
-- It utilizes `groupBy` from `Data.List` to cluster similar words.
fuzzyGroupWords :: [String] -> [[String]]
fuzzyGroupWords = groupBy (\w1 w2 -> levenshtein w1 w2 <= 2) . sort

-- **Pure Function**: aggregateFuzzyWordFrequencies
-- Aggregates fuzzy word frequencies by grouping similar words and counting occurrences.
-- This function processes the list of tokens and returns a list of tuples (word, frequency).
aggregateFuzzyWordFrequencies :: [String] -> [(String, Int)]
aggregateFuzzyWordFrequencies tokens =
    map (\g -> (head g, length g)) (fuzzyGroupWords tokens)  -- For each group, return the first word and its count

