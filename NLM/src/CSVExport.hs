module CSVExport (writeToCSV) where

import System.IO (withFile, IOMode(WriteMode), hPutStrLn)
import Control.Monad (zipWithM_)
import Text.Printf (printf)

-- **I/O Function**: writeToCSV
-- Writes generations, fuzzy word frequencies, and average character frequencies to a CSV file.
-- The function uses the 'withFile' function for safe file handling, ensuring the file is properly closed after writing.
writeToCSV :: FilePath -> [[String]] -> [[(String, Int)]] -> [(Char, Double)] -> IO ()
writeToCSV filename generations fuzzyFrequencies avgFrequencies =
    withFile filename WriteMode $ \handle -> do
        -- Write the header for generations and fuzzy frequencies
        hPutStrLn handle "Generation,Word,Fuzzy Frequency"

        -- **Higher-order Function**: zipWithM_
        -- Iterates over generations and their corresponding fuzzy frequencies, writing them to the CSV.
        zipWithM_ (\gen fuzzyFreq -> do
            -- For each word in the generation, add its fuzzy frequency to the row
            mapM_ (\(word, freq) ->
                hPutStrLn handle $ unwords gen ++ "," ++ word ++ "," ++ show freq
                ) fuzzyFreq
            ) generations fuzzyFrequencies

        -- Write average character frequencies in a separate section
        hPutStrLn handle "\nCharacter,Average Frequency"
        -- **List Comprehension**: Map through average frequencies and write them to the CSV
        mapM_ (\(char, avgFreq) ->
            hPutStrLn handle $ char : "," ++ printf "%.2f" avgFreq
            ) avgFrequencies


