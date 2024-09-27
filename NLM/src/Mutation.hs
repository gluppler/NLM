module Mutation (generateGenerations) where

import System.Random (randomRIO)

-- Mutate a word by randomly altering it
mutateWord :: String -> IO String
mutateWord word
    | null word = return word  -- Return the empty string as is
    | otherwise = do
        idx <- randomRIO (0, length word - 1)
        let (before, after) = splitAt idx word
        randChar <- randomRIO ('a', 'z')
        return $ before ++ [randChar] ++ tail after

-- Mutate an entire token list
mutateTokens :: [String] -> IO [String]
mutateTokens = mapM mutateWord

-- Generate multiple generations of tokens
generateGenerations :: [String] -> Int -> IO [[String]]
generateGenerations tokens 0 = return [tokens]
generateGenerations tokens n = do
    mutated <- mutateTokens tokens
    rest <- generateGenerations mutated (n - 1)
    return (mutated : rest)

