module HangmanSpec where

import qualified Data.Set as Set
import qualified Hangman as H
import Relude
import Test.Hspec

-- TODO T9
runGuesses :: H.GameState -> [H.Guess] -> Maybe H.Winner
runGuesses s g = H.winner finalState
  where
    finalState = foldl' (flip H.guess) s g

spec :: Spec
spec = describe "hangman game" $ do
  it "misses letter 8 times loses the game" $
    let guesses = replicate 8 (H.LetterGuess 'x')
        winner = runGuesses (H.newGame (H.RandomWord "dzik")) guesses
     in winner `shouldBe` Just H.Computer

  it "misses word 8 times loses the game" $
    let guesses = replicate 8 (H.WordGuess "xxx")
        winner = runGuesses (H.newGame (H.RandomWord "dzik")) guesses
     in winner `shouldBe` Just H.Computer

  it "misses 7 times, there is no winner yet" $
    let guesses = replicate 7 (H.WordGuess "xxx")
        winner = runGuesses (H.newGame (H.RandomWord "dzik")) guesses
     in winner `shouldBe` Nothing

  it "guesses all letters wins the game" $
    let guesses = H.LetterGuess <$> ['z', 'd', 'i', 'k']
        winner = runGuesses (H.newGame (H.RandomWord "dzik")) guesses
     in winner `shouldBe` Just H.Human

  it "guesses word wins the game" $
    let guesses = [H.WordGuess "dzik"]
        winner = runGuesses (H.newGame (H.RandomWord "dzik")) guesses
     in winner `shouldBe` Just H.Human

  it "shows total misses not guessed chars as _" $
    let gs = H.GameState 2 (Set.fromList ['d', 'k']) (H.RandomWord "dzik")
        expected = "(2) d _ _ k" :: Text
     in show gs `shouldBe` expected
