module Cards where

import System.Random

data Suite = Heart | Diamond | Club | Spade deriving (Read, Show)

data Color = Red | Black deriving (Read, Show)

data Card = Card Int Suite
instance Show Card where
	show (Card n s)
		| n <= 10 = show n ++ " of " ++ show s ++ "s"
		| n == 11 = "Jack of " ++ show s ++ "s"
		| n == 12 = "Queen of " ++ show s ++ "s"
		| n == 13 = "King of " ++ show s ++ "s"
		| otherwise = "Invalid Card"

type Pile = [Card]

startingDeck :: Pile
startingDeck = map (uncurry Card) [(n, s) | n <- [1..3], s <- [Heart, Diamond, Club, Spade]]

emptyDeck :: Pile
emptyDeck = []

shuffle :: (RandomGen g) => Pile -> g -> Pile
shuffle deck g = fst (shuffle' deck g emptyDeck)
	where 
		shuffle' [] gen acc = (acc, gen)
		shuffle' l gen acc = shuffle' (lead++xs) gen' (x:acc)
			where
				(k, gen') = next gen
				(lead, x:xs) = splitAt (k `mod` length l) l