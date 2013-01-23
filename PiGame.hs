import Cards
import System.Random
import Data.List

data Player = PlayerA | PlayerB deriving (Eq, Show)
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
} | InProgress {
	playingPlayer	:: Player,
	playerA      	:: PlayerState,
	playerB      	:: PlayerState,
	field        	:: Pile
}
 deriving Show

flop :: Player -> Player
flop PlayerA = PlayerB
flop PlayerB = PlayerA

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
		pA = PS PlayerA [] []
		pB = PS PlayerB [] []

pick :: Game -> Int -> Game
pick (Start _ pA pB []) _ = InProgress PlayerB pA pB []
pick (Start p pA pB u) n = 
	case unpicked' of
		[] -> InProgress PlayerB pA' pB' []
		_  -> Start (flop p) pA' pB' unpicked'
	where 
		(pA', pB') = case p of
			PlayerA -> (pA{piles = pile : piles pA}, pB)
			PlayerB -> (pA, pB{piles = pile : piles pB})
		pile = u !! n
		unpicked' = let (ys, zs) = splitAt n u in ys ++ tail zs
pick InProgress{} _ = undefined