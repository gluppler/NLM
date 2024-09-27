module CSVExport (writeToCSV) where

import System.IO (withFile, IOMode(WriteMode), hPutStrLn)
import Control.Monad (zipWithM_)
import Text.Printf (printf)

-- Function to write generations, fuzzy word frequencies, and average character frequencies to CSV
writeToCSV :: FilePath -> [[String]] -> [[(String, Int)]] -> [(Char, Double)] -> IO ()
writeToCSV filename generations fuzzyFrequencies avgFrequencies =
    withFile filename WriteMode $ \handle -> do
        -- Write the header for generations and fuzzy frequencies
        hPutStrLn handle "Generation,Word,Fuzzy Frequency"

        -- Write each generation and its corresponding fuzzy word frequencies
        zipWithM_ (\gen fuzzyFreq -> do
            -- For each word in the generation, add its fuzzy frequency to the row
            mapM_ (\(word, freq) ->
                hPutStrLn handle $ unwords gen ++ "," ++ word ++ "," ++ show freq
                ) fuzzyFreq
            ) generations fuzzyFrequencies

        -- Write average character frequencies in a separate section
        hPutStrLn handle "\nCharacter,Average Frequency"
        mapM_ (\(char, avgFreq) ->
            hPutStrLn handle $ char : ", " ++ printf "%.2f" avgFreq
            ) avgFrequencies

