# Warsztat podstaw Haskella

## Podstawy

### dane
``` haskell
-- product type
data Czlowiek = Czlowiek { age :: Int, name :: Text }
data Czlowiek = Czlowieczek { age :: Int, name :: Text }

-- sum type
data Kolor = R | G | B
data Maybe a = Some a | None

-- newtype
newtype S = S Text deriving (Show)
s = "asdf" :: Text
S s
-- coerce :: Coercible a b => a -> b
coerce s :: S
```

### funkcje
```haskell
f :: Int -> Int -> Int
f a b = a + b 

($) :: (a -> b) -> a -> b
infixr 0 $

(.) :: (b -> c) -> (a -> b) -> a -> c

f = (+1) . (*5)

data D = D Int Int deriving(Show)
:t D
:t D 2
D 2 + 2 + 2
D 2 (2 + 2)
D 2 $ 2 + 2
```

### do notation
```haskell
do; a <- [1, 2, 3]; b <- [4, 5, 6]; return (a,b)
```

### typeclassy
```haskell
class Show a where
  GHC.Show.show :: a -> String

instance Show A where
    show a = undefined

show :: (Show a, IsString b) => a -> b
```
### ghci
plik o nazwie`.ghci` w katalogu projektu
```import Control.Lens
:set prompt "> "
```

```
:t ($)
:k Maybe
:k Monad
:k StateT
:k Show
:browse Control.Monad
:i Monad
:i Int
:doc Maybe
```

`stack ghci` - odpalenie REPLa z konfiguracją projektu
### Relude - uzasadnienie
- undefined emits a warning
```haskell
-- prelude
show :: a -> String

head []
-- *** Exception: Prelude.head: empty list
tail []
-- *** Exception: Prelude.tail: empty list
-- and others
```

### Hoogle
Przydatna wyszukiwarka
```haskell
m a -> (a -> m b) -> m b
-- https://hoogle.haskell.org/?hoogle=m%20a%20-%3E%20(a%20-%3E%20m%20b)%20-%3E%20m%20b
```

### ghcid
- `ghcid` - live kompilacja i feedback
- ` ghcid --command "stack ghci haskell-workshop-basic:lib haskell-workshop-basic:test:haskell-workshop-basic-test" --test "main" --warning` - live odpalenie testów

### stack
```
stack build
stack test
stack run
```

## Część praktyczna
Napiszemy konsolową grę w wisielca. Użytkownik może zgadnąć literkę bądź słowo. Kilka błędów powoduje przegraną

### Zadania
- T1: zdefinuj newtype `RandomWord` oraz funkcje `randomWordChars` i uzupełnij `randomWords` - wspomagamy się typed hole
- T2: zdefiniuj `GameState` (liczba błędów, trafione znaki, słowo)
- T3: napisz funkcję `guessMiss` (zwiększa licznik błędów) - np. przy pomocy lensów
- T4: napisz instancję `Show` dla `GameState` - `(2) _ z _ k` oraz prosty test do niej
- T5: dodaj implementację funkcji `newGame`
- T6: zdefiniuj typy `Winner` (człowiek / komputer), `Guess` (pojedyncza litera / całe słowo)
- T7: zaimplementuj funkcję `guess`
- T8: zaimplementuj funkcję `winner`
- T9: napisz testy dla `guess` oraz `winner`
- T10: zaimplementuj funkcję `readGuess`
- T11: zaimplementuj funkcję `hangman` (hoogle `m (Maybe a) -> m a`)
- T12: zaimplementuj `newGameRandomWord`
- T13: zaimplementuj `main`
