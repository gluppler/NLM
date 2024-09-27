module Mutation (generateGenerations) where

import System.Random (randomRIO)

-- **Pure Function**: mutateWord
-- Mutates a word by randomly altering one character.
-- This function uses IO because it generates random values, but it still follows the structure of a pure function.
mutateWord :: String -> IO String
mutateWord word
    | null word = return word  -- Base case: Return the empty string as is
    | otherwise = do
        -- **Side-effecting IO Operation**: randomRIO
        -- Get a random index to mutate
        idx <- randomRIO (0, length word - 1)
        let (before, after) = splitAt idx word  -- Split the word into two parts at the random index
        -- **Side-effecting IO Operation**: randomRIO
        -- Generate a random character between 'a' and 'z'
        randChar <- randomRIO ('a', 'z')
        -- Combine the parts back together with the mutated character
        return $ before ++ [randChar] ++ tail after

-- **Pure Function**: mutateTokens
-- Applies mutation to each token in a list of strings (words).
-- This uses `mapM`, which is a higher-order function that applies an IO action to each element of a list.
mutateTokens :: [String] -> IO [String]
mutateTokens = mapM mutateWord

-- **Recursive Function**: generateGenerations
-- Generates multiple generations of mutated tokens.
-- This is a recursive function where the base case is when n equals 0.
generateGenerations :: [String] -> Int -> IO [[String]]
generateGenerations tokens 0 = return [tokens]  -- Base case: return the original tokens
generateGenerations tokens n = do
    -- **Use of IO Monad**: Chaining IO actions
    mutated <- mutateTokens tokens  -- Generate the next generation of mutated tokens
    rest <- generateGenerations mutated (n - 1)  -- Recursively generate the rest of the generations
    return (mutated : rest)  -- Return the current mutated generation along with the rest

