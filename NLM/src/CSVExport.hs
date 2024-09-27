module CSVExport (writeToCSV) where

import System.IO
import Data.List (intercalate)

-- Function to write generations and fuzzy frequencies to a CSV file
writeToCSV :: FilePath -> [[String]] -> [[(String, Int)]] -> IO ()
writeToCSV filePath generations fuzzyFrequencies = do
    withFile filePath WriteMode $ \handle -> do
        -- Write headers
        hPutStrLn handle "Generation,Word,Frequency"

        -- Write each generation and its fuzzy frequencies
        mapM_ (writeGeneration handle) (zip generations fuzzyFrequencies)

-- Write a single generation and its fuzzy frequencies
writeGeneration :: Handle -> ([String], [(String, Int)]) -> IO ()
writeGeneration handle (tokens, frequencies) = do
    let tokenLine = intercalate "," tokens
    mapM_ (writeFrequency handle tokenLine) frequencies

-- Write each fuzzy frequency associated with the current generation
writeFrequency :: Handle -> String -> (String, Int) -> IO ()
writeFrequency handle tokenLine (word, freq) = do
    let line = intercalate "," [tokenLine, word, show freq]
    hPutStrLn handle line

