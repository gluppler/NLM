module Tokenizer (preprocessPipeline, extractCharacters) where

import Data.Char (toLower, isAlpha)

-- **Pure Function & Functional Abstraction**: preprocessText
-- A helper function that converts text to lowercase and removes non-alphabetic characters (except spaces).
-- This is a pure function since it does not cause side effects.
preprocessText :: String -> String
preprocessText = map toLower . filter (\c -> isAlpha c || c == ' ')
-- `map` and `filter` are higher-order functions here, operating on each character in the string.

-- **Pure Function**: tokenize
-- Converts the preprocessed text into a list of words by splitting on spaces.
-- This function is pure and uses function composition to connect `words` and `preprocessText`.
tokenize :: String -> [String]
tokenize = words . preprocessText

-- **Immutable Data**: stopWords
-- A list of common stop words to be removed from the tokenized text.
-- This is an example of an immutable data structure, as lists in Haskell are immutable by nature.
stopWords :: [String]
stopWords = ["the", "is", "in", "at", "of", "on", "and", "a", "an", "to"]

-- **Higher-order Function**: removeStopWords
-- This function removes stop words from a list of tokens using `filter`, which is a higher-order function.
-- It returns a new list, filtering out any word that is in the `stopWords` list.
removeStopWords :: [String] -> [String]
removeStopWords = filter (`notElem` stopWords)

-- **Functional Composition & Pipeline**: preprocessPipeline
-- Combines tokenization and stop word removal into a single processing pipeline.
-- This demonstrates functional composition, where smaller functions are combined into a more complex operation.
preprocessPipeline :: String -> [String]
preprocessPipeline = removeStopWords . tokenize

-- **Pure Function**: extractCharacters
-- Extracts only the alphabetic characters from a list of tokens (removes spaces and any non-alphabetic symbols).
-- This is a pure function, as it processes the input and produces the same result given the same input.
extractCharacters :: [String] -> [Char]
extractCharacters = concatMap (filter isAlpha)
-- `concatMap` applies the `filter` function to each word in the list and concatenates the results.

