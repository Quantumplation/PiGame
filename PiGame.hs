import Cards
import System.Random
import Data.List

data Player = PlayerA | PlayerB deriving Show
data PlayerState = PS { 
	player			:: Player, 
	hand  			:: Pile, 
	piles 			:: [Pile] 
} deriving Show
data Game = Start { 
	pickingPlayer	:: Player, 
	playerA      	:: PlayerState,
	playerB      	:: PlayerState,
	unpicked     	:: [Pile]	
} deriving Show

top :: Pile -> Card
top = head

draw :: Pile -> Int -> (Pile, Pile)
draw p n = (take n p, drop n p)

splitN :: (RandomGen g) => Pile -> Int -> g -> [Pile]
splitN p n g = splitN' p (splits (length p - 1) g)
	where
		size = min (length p) n
		splitN' [] _ = []
		splitN' ps [] = [ps]
		splitN' ps (s:ss) = let (f, l) = splitAt s ps in f : splitN' l (map (\x -> x-s) ss)
		splits c gen = sort $ take (size-1) $ nub $ map (\x -> (x `mod` c) + 1) $ unfoldr (Just . next) gen

defaultStart :: (RandomGen g) => Pile -> Int -> g -> Game
defaultStart deck count gen = Start PlayerA pA pB (splitN deck count gen)
	where
		pA = PS PlayerA [] [[]]
		pB = PS PlayerB [] [[]]