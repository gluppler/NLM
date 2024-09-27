module Main where

import Tokenizer (preprocessPipeline)
import Mutation (generateGenerations)
import FuzzyMatcher (aggregateFuzzyWordFrequencies)
import CSVExport (writeToCSV)

main :: IO ()
main = do
    putStrLn "Enter your text: "
    inputText <- getLine

    putStrLn "Enter the number of generations to simulate: "
    genCountStr <- getLine
    let genCount = read genCountStr :: Int

    -- Process the text through the pipeline
    let processedTokens = preprocessPipeline inputText

    -- Generate generations of mutated tokens
    generations <- generateGenerations processedTokens genCount

    -- Aggregate fuzzy word frequencies for each generation
    let fuzzyFrequencies = map aggregateFuzzyWordFrequencies generations

    -- Print the output for each generation
    putStrLn "\nGenerational Output:"
    mapM_ printGeneration (zip generations fuzzyFrequencies)

    -- Write results to CSV
    putStrLn "Enter the filename to save (e.g., output.csv): "
    filename <- getLine
    writeToCSV filename generations fuzzyFrequencies

    putStrLn $ "Results saved to " ++ filename

-- Function to print each generation with its fuzzy frequencies
printGeneration :: ( [String], [(String, Int)]) -> IO ()
printGeneration (tokens, frequencies) = do
    putStrLn "Tokens: "
    print tokens
    putStrLn "Fuzzy Frequencies: "
    print frequencies
    putStrLn ""  -- Print an empty line for better readability


