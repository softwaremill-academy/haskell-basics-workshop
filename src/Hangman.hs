{-# LANGUAGE TemplateHaskell #-}

module Hangman where

import qualified Control.Monad.Loops as ML
import qualified Data.Set as Set
import Relude
import qualified Text.Show
import qualified Data.Random as R

-- TODO T1
newtype RandomWord = RandomWord {unRandomWord :: Text} deriving (Show, Eq)

randomWordChars :: RandomWord -> Set Char
randomWordChars = Set.fromList . toString . unRandomWord

randomWords :: NonEmpty RandomWord
randomWords = coerce predefinedWords
  where
    predefinedWords :: NonEmpty Text
    predefinedWords =
      "kosmita"
        :| [ "piwko",
             "tapir",
             "sttp",
             "klientmazawszeracje",
             "mlynarz",
             "chrum",
             "birr"
           ]

-- TODO T2
data GameState = GameState
  { gsMisses :: Int,
    gsHits :: Set Char,
    gsWord :: RandomWord
  }

-- TODO T3
guessMiss :: GameState -> GameState
guessMiss s@(GameState m _ _) = s {gsMisses = m + 1}

-- TODO T4 instance Show GameState
-- (2) _ z _ k
instance Show GameState where
  show (GameState misses hits word) = misses' <> " " <> intersperse ' ' (showChar <$> word')
    where
      misses' = "(" <> show misses <> ")"
      word' = (toString . unRandomWord) word
      showChar c
        | Set.member c hits = c
        | otherwise = '_'

-- TODO T5
newGame :: RandomWord -> GameState
newGame = GameState 0 Set.empty

-- TODO T6
data Winner
  = Human
  | Computer
  deriving (Show, Eq)

data Guess
  = LetterGuess Char
  | WordGuess Text

-- TODO T7
guess :: Guess -> GameState -> GameState
guess (LetterGuess l) s@(GameState _ hits word)
  | hasLetter = s {gsHits = Set.insert l hits}
  | otherwise = guessMiss s
  where
    hasLetter = Set.member l (randomWordChars word)
guess (WordGuess w) s@(GameState _ _ (RandomWord actualWord))
  | w == actualWord = s {gsHits = (Set.fromList . toString) actualWord}
  | otherwise = guessMiss s

-- TODO T8
winner :: GameState -> Maybe Winner
winner (GameState misses hits word)
  | misses >= 8 = Just Computer
  | hits == randomWordChars word = Just Human
  | otherwise = Nothing

-- TODO T10
readGuess :: (MonadIO m) => m (Maybe Guess)
readGuess = do
  toGuess . toString <$> getLine
  where
    toGuess "" = Nothing
    toGuess [c] = Just (LetterGuess c)
    toGuess cs = Just (WordGuess (toText cs))

-- TODO T11
hangman :: StateT GameState IO Winner
hangman = do
  ML.untilJust guessRound
  where
    guessRound :: StateT GameState IO (Maybe Winner)
    guessRound = do
      game <- get
      print game
      playerGuess <- readGuess
      case playerGuess of
        (Just g) -> do
          let game' = guess g game
          put game'
          return $ winner game'
        Nothing -> return Nothing

-- TODO T12
newGameRandomWord :: IO GameState
newGameRandomWord = R.runRVar randomElem R.StdRandom
  where
    word = R.randomElement (toList randomWords)
    randomElem = newGame <$> word
