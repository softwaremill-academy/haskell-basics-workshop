{-# LANGUAGE TemplateHaskell #-}

module Hangman where

import Relude

-- TODO T1
data RandomWord = RandomWord

randomWordChars :: RandomWord -> Set Char
randomWordChars = error "randomWordChars"

randomWords :: NonEmpty RandomWord
randomWords = error "randomWords"

--   _
--     <$> "kosmita"
--       :| [ 
--            "piwko",
--            "tapir",
--            "sttp",
--            "klientmazawszeracje",
--            "mlynarz",
--            "chrum",
--            "birr"
--          ]

-- TODO T2
data GameState = GameState deriving (Show)

-- TODO T3
guessMiss :: GameState -> GameState
guessMiss = error "guessMiss"

-- TODO T4 instance Show GameState


-- TODO T5
newGame :: RandomWord -> GameState
newGame = error "newGame"

-- TODO T6
data Winner = Winner

data Guess = Guess

-- TODO T7
guess :: Guess -> GameState -> GameState
guess = error "guess"

-- TODO T8
winner :: GameState -> Maybe Winner
winner = error "winner"

-- TODO T10
readGuess :: {-- (MonadIO m) => --} m (Maybe Guess)
readGuess = error "readGuess"

-- TODO T11
hangman :: StateT GameState IO Winner
hangman = error "hangman"

-- TODO T12
newGameRandomWord :: IO GameState
newGameRandomWord = error "newGameRandomWord"
