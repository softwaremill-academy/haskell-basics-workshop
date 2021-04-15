module Main where

import qualified Hangman as H
import Relude

-- TODO T13
main :: IO ()
main = do
  putStrLn "Zgaduj zgadula"
  game <- H.newGameRandomWord
  winner <- evalStateT H.hangman game
  printWinner winner
  where
    printWinner H.Human = putStrLn "wygrałeś!"
    printWinner H.Computer = putStrLn "przegrałeś :("
