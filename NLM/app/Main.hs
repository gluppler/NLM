module Main where

-- Import modules and functions
import Tokenizer (preprocessPipeline, extractCharacters)
import Mutation (generateGenerations)
import FuzzyMatcher (aggregateFuzzyWordFrequencies)
import CSVExport (writeToCSV)
import Data.Map (Map, empty, insertWith, toList)
import Data.List (foldl')  -- Higher-order function
import Control.Monad (forM_)  -- Monadic structure to handle side-effects
import Text.Printf (printf)

-- **Pure Function & Recursion (Higher-order function)**: countCharFrequency
-- A recursive function that counts the frequency of characters in a generation of tokens.
countCharFrequency :: [Char] -> Map Char Int -> Map Char Int
countCharFrequency chars freqMap = foldl' (\acc c -> insertWith (+) c 1 acc) freqMap chars
-- `foldl'` is a higher-order function here, processing each character with the accumulating frequency map.

-- **Pure Function & Higher-order function**: calculateAverageFrequencies
-- A pure function that calculates the average frequency of each character over all generations.
calculateAverageFrequencies :: [[String]] -> [(Char, Double)]
calculateAverageFrequencies generations =
    let totalGenerations = length generations
        allCharFrequencies = foldl' (\freqMap generation ->
                                       countCharFrequency (extractCharacters generation) freqMap) empty generations
        -- Uses `foldl'` as a higher-order function to process and aggregate results over all generations
        avgFrequencies = map (\(char, freq) -> (char, fromIntegral freq / fromIntegral totalGenerations)) (toList allCharFrequencies)
        -- **First-class function**: `map` processes each character-frequency pair and calculates the average frequency
    in avgFrequencies

-- **Main Monadic Flow (IO Monad)**: main
-- This is the main monadic entry point of the program, handling user input, mutation generations, and output.
main :: IO ()
main = do
    -- **Side-effect (I/O)**: Prompts user input and handles the input/output flow
    putStrLn "Enter your text: "
    inputText <- getLine

    putStrLn "Enter the number of generations to simulate: "
    genCountStr <- getLine
    let genCount = read genCountStr :: Int

    -- **Pure Function Composition**: Text preprocessing pipeline
    -- Process the text through the pipeline to extract tokens (pure function)
    let processedTokens = preprocessPipeline inputText

    -- **Higher-order function & Recursion**: Generate generations of mutated tokens
    generations <- generateGenerations processedTokens genCount
    -- `generateGenerations` is a recursive, higher-order function that mutates tokens over multiple generations

    -- **Fuzzy Systems Logic**: Fuzzy word frequency aggregation
    -- Aggregate fuzzy word frequencies for each generation
    let fuzzyFrequencies = map aggregateFuzzyWordFrequencies generations
    -- Uses a fuzzy matching function to group and count similar words based on Levenshtein distance

    -- **Functional Abstraction**: Average character frequency across generations
    -- Calculate average character frequencies across generations (pure function)
    let avgFrequencies = calculateAverageFrequencies generations

    -- **Monadic Structure (IO Monad)**: Write results to CSV
    -- Handle side-effects while writing the results to a CSV file
    putStrLn "Enter the filename to save (e.g., output.csv): "
    filename <- getLine
    writeToCSV filename generations fuzzyFrequencies avgFrequencies

    -- **Functional Abstraction & Monadic Flow**: Print average frequencies for each character
    -- Print formatted average frequencies (side effect handled via IO Monad)
    putStrLn "\nAverage Frequency of Characters across Generations:"
    forM_ avgFrequencies $ \(char, avgFreq) ->
        putStrLn $ char : ": " ++ printf "%.2f" avgFreq
    -- `forM_` is a monadic function used to iterate through average frequencies and print them out

    -- **Monadic Side-effect**: Final confirmation output
    putStrLn $ "Results saved to " ++ filename

