module Tokenizer (preprocessPipeline) where

import Data.Char (toLower, isAlpha)

-- Helper function to preprocess text (convert to lowercase and remove non-alphabetic characters)
preprocessText :: String -> String
preprocessText = map toLower . filter (\c -> isAlpha c || c == ' ')

-- Function to tokenize the preprocessed text
tokenize :: String -> [String]
tokenize = words . preprocessText

-- List of stop words to filter out
stopWords :: [String]
stopWords = ["the", "is", "in", "at", "of", "on", "and", "a", "an", "to"]

-- Remove stop words from the list of tokens
removeStopWords :: [String] -> [String]
removeStopWords = filter (`notElem` stopWords)

-- Full text preprocessing pipeline
preprocessPipeline :: String -> [String]
preprocessPipeline = removeStopWords . tokenize

