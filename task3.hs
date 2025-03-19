----------------------------
-- File: HangmanFromFile.hs
----------------------------
module Main where

import Data.Char (toLower)
import Data.List (nub, sort)
import System.IO (hFlush, stdout)

-- | Check if the secret word has been fully guessed.
isWordGuessed :: String -> [Char] -> Bool
isWordGuessed word guesses = all (`elem` guesses) word

-- | Shows the current state of the guessed word (underscores for unguessed letters).
showGuessedWord :: String -> [Char] -> String
showGuessedWord word guesses =
  [ if c `elem` guesses then c else '_' | c <- word ]

-- | Primary "hangman" function. Reads secret word from a file and starts the game loop.
hangmanFromFile :: String -> IO ()
hangmanFromFile filename = do
    putStrLn $ "Reading word from file: " ++ filename
    fileContent <- readFile filename
    let wordList = words fileContent
    if null wordList
      then putStrLn "Error: The file is empty or doesn't contain a valid word."
      else do
        -- Convert secret word to lowercase for easier guessing
        let secret = map toLower (head wordList)
        putStrLn "Word loaded successfully. Let's play Hangman!"
        putStrLn $ "The word has " ++ show (length secret) ++ " letters."
        gameLoop secret [] 0

-- | The main game loop that asks the user for letters until they guess the entire word or quit.
gameLoop :: String   -- ^ The secret word
         -> [Char]   -- ^ Letters guessed so far
         -> Int      -- ^ Number of guesses made
         -> IO ()
gameLoop word guesses guessCount = do
    putStrLn $ "\nCurrent word: " ++ showGuessedWord word guesses
    putStrLn $ "Letters guessed so far: " ++ sort guesses
    putStrLn $ "Number of guesses: " ++ show guessCount
    
    if isWordGuessed word guesses
      then putStrLn $ "Congratulations! You've guessed the word \"" 
                    ++ word ++ "\" in " ++ show guessCount ++ " guesses!"
      else do
        putStr "Guess a letter (or press Enter with no input to quit): "
        hFlush stdout  -- Ensure prompt is displayed before input
        guess <- getLine
        
        if null guess
          then putStrLn "No letter entered. Exiting the game!"
          else do
            let letter = toLower (head guess)
            if letter `elem` guesses
              then do
                putStrLn $ "You already guessed '" ++ [letter] ++ "'. Try again!"
                gameLoop word guesses guessCount
              else do
                let newGuesses = letter : guesses
                let newCount   = guessCount + 1
                if letter `elem` word
                  then putStrLn $ "Good guess! The letter '" ++ [letter] ++ "' is in the word."
                  else putStrLn $ "Sorry, the letter '" ++ [letter] ++ "' is not in the word."
                gameLoop word newGuesses newCount

-- | Main function
main :: IO ()
main = do
    putStrLn "=== Hangman Game (Reading Word From File) ==="
    putStrLn "Enter the filename containing the secret word:"
    filename <- getLine
    hangmanFromFile filename
    
    putStrLn "\nWould you like to play again? (y/n)"
    response <- getLine
    if map toLower response == "y"
      then main
      else putStrLn "Thanks for playing! Goodbye!"


-- | Main function\fgbr
tr
t
r
r
tbr
tb
r
by

