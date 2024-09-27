module Main where

import Tokenizer (preprocessPipeline, extractCharacters)
import Mutation (generateGenerations)
import FuzzyMatcher (aggregateFuzzyWordFrequencies)
import CSVExport (writeToCSV)
import Data.Map (Map, empty, insertWith, toList)
import Data.List (foldl')
import Control.Monad (forM_)
import Text.Printf (printf)

-- Function to count the frequency of characters in a generation
countCharFrequency :: [Char] -> Map Char Int -> Map Char Int
countCharFrequency chars freqMap = foldl' (\acc c -> insertWith (+) c 1 acc) freqMap chars

-- Calculate average frequency for each character
calculateAverageFrequencies :: [[String]] -> [(Char, Double)]
calculateAverageFrequencies generations =
    let totalGenerations = length generations
        allCharFrequencies = foldl' (\freqMap generation ->
                                       countCharFrequency (extractCharacters generation) freqMap) empty generations
        avgFrequencies = map (\(char, freq) -> (char, fromIntegral freq / fromIntegral totalGenerations)) (toList allCharFrequencies)
    in avgFrequencies

main :: IO ()
main = do
    putStrLn "Enter your text: "
    inputText <- getLine

    putStrLn "Enter the number of generations to simulate: "
    genCountStr <- getLine
    let genCount = read genCountStr :: Int

    -- Process the text through the pipeline
    let processedTokens = preprocessPipeline(inputText)

    -- Generate generations of mutated tokens
    generations <- generateGenerations processedTokens genCount

    -- Aggregate fuzzy word frequencies for each generation
    let fuzzyFrequencies = map aggregateFuzzyWordFrequencies generations

    -- Calculate average character frequencies across generations
    let avgFrequencies = calculateAverageFrequencies generations

    -- Write results to CSV
    putStrLn "Enter the filename to save (e.g., output.csv): "
    filename <- getLine
    writeToCSV filename generations fuzzyFrequencies avgFrequencies

    -- Print average frequencies for each character (formatted to two decimal places)
    putStrLn "\nAverage Frequency of Characters across Generations:"
    forM_ avgFrequencies $ \(char, avgFreq) ->
        putStrLn $ char : ": " ++ printf "%.2f" avgFreq

    putStrLn $ "Results saved to " ++ filename

