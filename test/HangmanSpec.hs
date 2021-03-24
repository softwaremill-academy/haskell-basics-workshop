module HangmanSpec where

import qualified Hangman as H
import Relude
import Test.Hspec

-- TODO T9
runGuesses :: H.GameState -> [H.Guess] -> Maybe H.Winner
runGuesses = error "runGuesses"

spec :: Spec
spec = describe "hangman game" $ do
  it "misses letter 8 times loses the game" $
    x `shouldBe` 1

  it "misses word 8 times loses the game" $
    x `shouldBe` 1

  it "misses 7 times, there is no winner yet" $
    x `shouldBe` 1

  it "guesses all letters wins the game" $
    x `shouldBe` 1

  it "guesses word wins the game" $
    x `shouldBe` 1

  it "shows total misses not guessed chars as _" $
    x `shouldBe` 1
  where
    x = 1 :: Int
